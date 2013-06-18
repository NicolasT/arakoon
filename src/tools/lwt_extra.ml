(*
This file is part of Arakoon, a distributed key-value store. Copyright
(C) 2010 Incubaid BVBA

Licensees holding a valid Incubaid license may use this file in
accordance with Incubaid's Arakoon commercial license agreement. For
more information on how to enter into this agreement, please contact
Incubaid (contact details can be found on www.arakoon.org/licensing).

Alternatively, this file may be redistributed and/or modified under
the terms of the GNU Affero General Public License version 3, as
published by the Free Software Foundation. Under this license, this
file is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.

See the GNU Affero General Public License for more details.
You should have received a copy of the
GNU Affero General Public License along with this program (file "COPYING").
If not, see <http://www.gnu.org/licenses/>.
*)

open Lwt

let section = Logger.Section.main

let igns = Hashtbl.create 10
let detacheds = Hashtbl.create 10

let rec get_key () =
  let candidate = Random.int 1073741823 in
  if Hashtbl.mem igns candidate
  then
    get_key ()
  else
    candidate

let _condition = Lwt_condition.create ()
let _tearing_down = ref false

let _conditionally_wrap_catch_canceled t =
  Lwt.catch
    (fun () -> t)
    (function
      | Canceled ->
          if (fun () -> !_tearing_down) ()
          then
            Logger.debug_ "Ignoring canceled while tearing down" >>= fun () ->
            Lwt.return ()
          else
            begin
              Lwt.fail Canceled
            end
      | exn -> Lwt.fail exn)


let ignore_result (t:'a Lwt.t) =
  let key = get_key () in
  let t' () =
    _conditionally_wrap_catch_canceled
      (Lwt.finalize
         (fun () -> t)
         (fun () ->
           Hashtbl.remove igns key;
           Lwt_condition.signal _condition ();
           Lwt.return ())) in
  Hashtbl.add igns key t;
  Lwt.ignore_result (t' ())

let detach (t: unit -> string) =
  let t' = Lwt_preemptive.detach t () in
  let key = get_key () in
  Hashtbl.add detacheds key t';
  Lwt.finalize
    (fun () -> t')
    (fun () ->
      Hashtbl.remove detacheds key;
      Lwt_condition.signal _condition ();
      if !_tearing_down
      then
        Lwt.fail Canceled
      else
        Lwt.return ())


let _pickeds_finalizing = ref 0

let _pick ts a =
  _pickeds_finalizing := !_pickeds_finalizing + List.length ts;
  Lwt.pick
    (List.map
       (fun t ->
         Lwt.catch
           (fun () ->
             Lwt.finalize
               (fun () -> t)
               (fun () ->
                 decr _pickeds_finalizing;
                 Lwt_condition.signal _condition ();
                 Lwt.return ()))
           (function
             | Canceled -> Lwt.return a
             | exn -> Lwt.fail exn))
       ts)

let pick ts =
  _pick ts ()

let run t =
  let act () =
    Lwt.finalize
      (fun () -> t)
      (fun () ->
        _tearing_down := true;
        Logger.debug_ "Cancelling ignored and detached threads" >>= fun () ->
        Lwt.finalize
          (fun () ->
            Lwt.catch
              (fun () ->
                let ignored_threads = Hashtbl.fold (fun k t acc -> t :: acc) igns [] in
                Lwt.pick (Lwt.return () :: ignored_threads))
              (fun exn ->
                Logger.info_ ~exn "Picking the ignored threads failed" >>= fun () ->
                Lwt.wrap (fun () -> !Lwt.async_exception_hook exn)) >>= fun () ->
            Lwt.catch
              (fun () ->
                let detached_threads = Hashtbl.fold (fun k t acc -> t :: acc) detacheds [] in
                Lwt.pick (Lwt.return "" :: detached_threads) >>= fun _ ->
                Lwt.return ())
              (fun exn ->
                Logger.info_ ~exn "Picking the ignored threads failed" >>= fun () ->
                Lwt.wrap (fun () -> !Lwt.async_exception_hook exn)) >>= fun () ->
            let rec wait () =
              let c_igns = Hashtbl.length igns in
              let c_detacheds = Hashtbl.length detacheds in
              print_endline (Printf.sprintf "igns = %i; detacheds = %i; pickeds = %i" c_igns c_detacheds !_pickeds_finalizing);
              if c_igns > 0 or c_detacheds > 0 or !_pickeds_finalizing > 0
              then
                begin
                  Lwt_condition.wait _condition >>= fun () ->
                  wait ()
                end
              else
                Lwt.return () in

            wait ())

          (fun () ->
            _tearing_down := false;
            Logger.debug Logger.Section.main "Finished cancelling ignored and detached threads"))
  in
  Lwt_main.run (act ())

