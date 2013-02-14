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

open Store
open Lwt
open Log_extra
open Update
open Interval
open Routing

module StringMap = Map.Make(String);;

let try_lwt_ f = Lwt.catch (fun () -> Lwt.return (f ())) (fun exn -> Lwt.fail exn)

class mem_store db_name =
  let now64 () = Int64.of_float (Unix.gettimeofday ())
  in

object (self: #store)

  val mutable i = None
  val mutable kv = StringMap.empty
  val mutable master = None
  val mutable _interval = Interval.max
  val mutable _routing = None

  method incr_i () =
    Lwt.return (self # _incr_i ())

  method _incr_i () =
    let i2 = match i with
    | None -> Some 0L
    | Some i' -> Some ( Sn.succ i' )
    in
    let () = i <- i2 in
    ()

  method _set_i x =
    i <- Some x

  method exists ?(_pf=__prefix) key =
    try_lwt_ (fun () -> StringMap.mem key kv)

  method get ?(_pf=__prefix) key =
    try_lwt_ (fun () -> StringMap.find key kv)

  method multi_get ?(_pf=__prefix) keys =
    let values = List.fold_left
      (fun acc key -> let value = StringMap.find key kv in value :: acc)
      [] keys
    in
    Lwt.return (List.rev values)

  method range ?(_pf=__prefix) first finc last linc max =
    let keys = Test_backend.range_ kv first finc last linc max in
    Lwt.return keys

  method range_entries ?(_pf=__prefix) first finc last linc max =
    let entries = Test_backend.range_entries_ kv first finc last linc max in
    Lwt.return entries

  method rev_range_entries ?(_pf=__prefix) first finc last linc max =
    let entries = Test_backend.rev_range_entries_ kv first finc last linc max in
    Lwt.return entries

  method prefix_keys ?(_pf=__prefix) prefix max =
    let reg = "^" ^ prefix in
    let keys = StringMap.fold
      (fun k v a ->
	if (Str.string_match (Str.regexp reg) k 0)
	then k::a
	else a
      ) kv []
    in Lwt.return keys

  method set ?(_pf=__prefix) key value =
    self # set_no_incr key value

  method private set_no_incr ?(_pf=__prefix) key value =
    let () = kv <- StringMap.add key value kv in
    Lwt.return ()

  method set_master master' l =
    let () = master <- Some (master', now64()) in
    Lwt.return ()

  method set_master_no_inc master' l =
    let () = master <- Some (master', now64()) in
    Lwt.return ()


  method quiesce () = Lwt.return ()

  method unquiesce () = Lwt.return ()

  method quiesced () = false

  method optimize () = Lwt.return ()
  method defrag () = Lwt.return ()

  method aSSert ?(_pf=__prefix) key vo =
    let r =
      match vo with
	| None -> not (StringMap.mem key kv)
	| Some v ->
	  begin
	    try StringMap.find key kv = v
	    with Not_found -> false
	  end
    in Lwt.return r

  method aSSert_exists ?(_pf=__prefix) key =
    let r = (StringMap.mem key kv)
    in Lwt.return r

  method reload_some_cfg () =
    Lwt_log.debug "reload_some_cfg mem_store" >>= fun () ->
    let signal_to_itself number = Unix.kill (Unix.getpid ()) number in
    signal_to_itself 10;
    Lwt.return ()

  method who_master () = master

  method private delete_no_incr ?(_pf=__prefix) key =
    if StringMap.mem key kv then
      begin
	Lwt_log.debug_f "%S exists" key >>= fun () ->
	let () = kv <- StringMap.remove key kv in
	Lwt.return ()
      end
    else
      begin
	Lwt_log.debug "going to fail" >>= fun () ->
	Lwt.fail (Key_not_found key)
      end

  method delete ?(_pf=__prefix) key =
    Lwt_log.debug_f "mem_store # delete %S" key >>= fun () ->
    self # delete_no_incr key

  method test_and_set ?(_pf=__prefix) key expected wanted =
    Lwt.catch
      (fun () ->
	    self # get key >>= fun res -> Lwt.return (Some res))
      (function
	    | Not_found -> Lwt.return None
	    | exn -> Lwt.fail exn)
    >>= fun existing ->
    if existing <> expected
    then Lwt.return existing
    else
      begin
	    (match wanted with
	      | None -> self # delete key
	      | Some wanted_s -> self # set key wanted_s)
	    >>= fun () -> 
        Lwt.return wanted
      end


  method sequence ?(_pf=__prefix) updates =
    Lwt_log.info "mem_store :: sequence" >>= fun () ->
    let do_one u =
      let u_s = Update.update2s u in
      Lwt_log.debug_f "u=%s" u_s >>= fun () ->
      match u with
	| Update.Set (k,v) -> self # set_no_incr k v
	| Update.Delete k  -> self # delete_no_incr k
	| Update.Assert(k,vo) ->
	  begin
	    self # aSSert k vo >>= function
	      | true -> Lwt.return ()
	      | false ->
		let ex =
		  Arakoon_exc.Exception(Arakoon_exc.E_ASSERTION_FAILED, k) in
		Lwt.fail ex
	  end
	| Update.Assert_exists(k) ->
	  begin
	    self # aSSert_exists k >>= function
	      | true -> Lwt.return ()
	      | false ->
		let ex =
		  Arakoon_exc.Exception(Arakoon_exc.E_ASSERTION_FAILED, k) in
		Lwt.fail ex
	  end
	| Update.Reload_some_cfg() ->
            Lwt_log.debug_f "mem_store :: reload_some_cfg" >>= fun () ->
	    self # reload_some_cfg ()
	| _ -> Llio.lwt_failfmt "Sequence does not support %s" u_s
    in
    let old_kv = kv in
    Lwt.catch
      (fun () ->
	Lwt_list.iter_s do_one updates)
      (fun exn -> kv <- old_kv;
	Lwt_log.debug ~exn "mem_store :: sequence failed" >>= fun () ->
	Lwt.fail exn)

  method consensus_i () = i

  method close () = Lwt.return ()

  method reopen when_closed = Lwt.return ()

  method get_location () = failwith "not supported"

  method user_function name po =
    Lwt_log.debug_f "mem_store :: user_function %s" name >>= fun () ->
    Lwt.return None

  method set_interval iv =
    Lwt_log.debug_f "set_interval %s" (Interval.to_string iv) >>= fun () ->
    _interval <- iv;
    Lwt.return ()

  method get_interval () =
    Lwt_log.debug "get_interval" >>= fun () ->
    Lwt.return _interval

  method get_routing () =
    match _routing with
      | None -> Lwt.fail Not_found
      | Some r -> Lwt.return r

  method set_routing r =
    _routing <- Some r;
    Lwt.return ()

  method set_routing_delta left sep right =
    match _routing with
      | None -> failwith "Cannot update non-existing routing"
      | Some r ->
        begin
          let new_r = Routing.change r left sep right in
          Lwt.return ( _routing <- Some new_r )
        end

  method get_key_count ?(_pf=__prefix) () =
    let inc key value size =
      Int64.succ size
    in
    Lwt.return (StringMap.fold inc kv 0L)

  method copy_store ?(_networkClient=true) oc = failwith "copy_store not supported"

  method relocate new_location = failwith "Memstore.relocation not implemented"

  method get_fringe boundary direction =
    Lwt_log.debug_f "mem_store :: get_border_range %s" (Log_extra.string_option2s boundary) >>= fun () ->
    let cmp =
      begin
        match direction, boundary with
          | Routing.UPPER_BOUND, Some b -> (fun k -> b < k )
          | Routing.LOWER_BOUND, Some b -> (fun k -> b >= k)
          | _ , None -> (fun k -> true)
      end
    in
    let all = StringMap.fold
      (fun k v acc ->
	if cmp k
	then (k,v)::acc
	else acc)
      kv []
    in
    Lwt.return all

  method delete_prefix ?(_pf=__prefix) prefix = Lwt.return 0
    
end

let make_mem_store ?(read_only=false) db_name =
  let store = new mem_store db_name in
  let store2 = (store :> store) in
  Lwt.return store2

let copy_store old_location new_location overwrite =
  Lwt.return ()


