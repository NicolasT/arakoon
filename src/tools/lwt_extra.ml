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

let rec get_key ht =
  let candidate = Random.int 1073741823 in
  if Hashtbl.mem ht candidate
  then
    get_key ht
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


let ignore_result (t:unit Lwt.t) =
  let key = get_key igns in
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
  (* TODO can detached thread be run in parallel with thread waiting on condition?
     Join on the threads, if thread with condition cancels then cancel the entire thread

     This would make detached threads cancelable without being in cleanup phase
  *)
  let t' = Lwt_preemptive.detach t () in
  let key = get_key detacheds in
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
let _pickeds = ref 0

let _pick ts a =
  incr _pickeds;
  Lwt.pick
    (List.map
       (fun t ->
         Lwt.finalize
           (fun () ->
             incr _pickeds_finalizing;
             Lwt.catch
               (fun () -> t)
               (function
                 | Canceled -> Lwt.return a
                 | exn -> Lwt.fail exn))
           (fun () ->
             decr _pickeds_finalizing;
             Lwt_condition.signal _condition ();
             Lwt.return ()))
       ts) >>= fun () ->
  decr _pickeds;
  Lwt.return ()

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
            let cancel t =
              try
                Lwt.cancel t
              with exn -> () in
            Hashtbl.iter (fun k t -> cancel t) igns;
            Hashtbl.iter (fun k t -> cancel t) detacheds;

            let rec wait () =
              let c_igns = Hashtbl.length igns in
              let c_detacheds = Hashtbl.length detacheds in
              print_endline (Printf.sprintf "igns = %i; detacheds = %i; pickeds = %i" c_igns c_detacheds !_pickeds_finalizing);
              if c_igns > 0 or c_detacheds > 0 or !_pickeds_finalizing > 0 or !_pickeds > 0
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

