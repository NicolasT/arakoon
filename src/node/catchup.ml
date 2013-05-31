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
open Node_cfg
open Remote_nodestream
open Tlogcollection
open Log_extra
open Update
open Store
open Tlogcommon

exception StoreAheadOfTlogs of (Int64.t * Sn.t)
exception StoreCounterTooLow of string

type store_tlc_cmp =
  | Store_n_behind
  | Store_1_behind
  | Store_tlc_equal
  | Store_ahead

let _with_client_connection (ips,port) f = 
  let sl2s ss = list2s (fun s -> s) ss in
  let rec loop = function
    | [] -> Llio.lwt_failfmt "None of the ips: %s can be reached" (sl2s ips)
    | ip :: rest ->
      Lwt.catch
        (fun () ->
          let address = Network.make_address ip port in
          Lwt_io.with_connection address f
        )
        (fun exn -> 
          Logger.info_f_ ~exn "ip = %s " ip >>= fun () ->
          loop rest
        )
  in
  loop ips
      

let compare_store_tlc store tlc =
  tlc # get_last_i () >>= fun tlc_i ->
  let m_store_i = store # consensus_i () in
  match m_store_i with
    | None ->
        if tlc_i = ( Sn.succ Sn.start )
        then Lwt.return Store_1_behind
        else Lwt.return Store_n_behind
    | Some store_i when store_i = tlc_i -> Lwt.return Store_tlc_equal
    | Some store_i when store_i > tlc_i -> Lwt.return Store_ahead
    | Some store_i ->
        if store_i = (Sn.pred tlc_i)
        then Lwt.return Store_1_behind
        else Lwt.return Store_n_behind

let head_saved_epilogue hfn tlog_coll = 
  (* maybe some tlogs must be thrown away because 
     they were already collapsed into 
     the received head 
  *)
  Logger.debug_f_ "head_saved_epilogue %s" hfn >>= fun () ->
  let module S = (val (Store.make_store_module (module Batched_store.Local_store))) in
  S.make_store ~read_only:true hfn >>= fun store ->
  let hio = S.consensus_i store in
  S.close store >>= fun () ->
  Logger.info_ "closed head" >>= fun () ->
  begin
    match hio with 
      | None -> Lwt.return () 
      | Some head_i -> 
	begin
	  Logger.debug_f_ "head_i = %s" (Sn.string_of head_i) >>= fun () ->	    
	  tlog_coll # remove_below head_i 
	end
  end



let catchup_tlog (type s) me other_configs ~cluster_id (current_i: Sn.t) mr_name ((module S : Store.STORE with type t = s),store,tlog_coll)
    =
  Logger.debug_f_ "catchup_tlog %s" (Sn.string_of current_i) >>= fun () ->
  let mr_cfg = List.find (fun cfg -> Node_cfg.node_name cfg = mr_name)
    other_configs in
  let mr_addresses = Node_cfg.client_addresses mr_cfg 
  and mr_name = Node_cfg.node_name mr_cfg in
  Logger.debug_f_ "getting last_entries from %s" mr_name >>= fun () ->
  let head_saved_cb hfn = 
    Logger.debug_f_ "head_saved_cb %s" hfn >>= fun () -> 
    head_saved_epilogue hfn tlog_coll >>= fun () -> 
    let when_closed () = 
      Logger.debug_ "when_closed" >>= fun () ->
      let target_name = S.get_location store in
      File_system.copy_file hfn target_name 
    in
    S.reopen store when_closed >>= fun () ->
    Lwt.return ()
  in

  let copy_tlog connection =
    make_remote_nodestream cluster_id connection >>= fun (client:nodestream) ->
    let f (i,value) =
      tlog_coll # log_value i value >>= fun _ -> 
      Lwt.return ()
    in

    client # iterate current_i f tlog_coll ~head_saved_cb
  in

  Lwt.catch
    (fun () ->
      _with_client_connection mr_addresses copy_tlog >>= fun () ->
      Logger.debug_f_ "catchup_tlog completed" 
    )
    (fun exn -> Logger.warning_ ~exn "catchup_tlog failed") 
  >>= fun () ->
  let tlc_i = tlog_coll # get_last_i () in
  let too_far_i = Sn.succ ( tlc_i ) in 
  Lwt.return too_far_i

let catchup_store (type s) me ((module S : Store.STORE with type t = s), store,tlog_coll) (too_far_i:Sn.t) =
  Logger.info_ "replaying log to store"
  >>= fun () ->
  let store_i = S.consensus_i store in
  let start_i =
    match store_i with
      | None -> Sn.pred (Sn.zero)
      | Some i -> Sn.succ i
  in
  if Sn.compare start_i too_far_i > 0 
  then 
    let msg = Printf.sprintf "Store counter (%s) is ahead of tlog counter (%s). Aborting."
      (Sn.string_of start_i) (Sn.string_of too_far_i) in
    Logger.error_ msg >>= fun () ->
    Lwt.fail (StoreAheadOfTlogs(start_i, too_far_i))
  else
  begin 
  Logger.debug_f_ "will replay starting from %s into store, too_far_i:%s" 
    (Sn.string_of start_i) (Sn.string_of too_far_i)
  >>= fun () ->
    let acc = ref None in
    let maybe_log_progress pi =
      let (+:) = Sn.add 
      and border = Sn.of_int 100 in
      if pi < start_i +: border ||
         pi +: border > too_far_i 
      then
	    Logger.debug_f_ "%s => store" (Sn.string_of pi) 
      else 
        if Sn.rem pi border = Sn.zero 
        then
          Logger.debug_f_ "... %s ..." (Sn.string_of pi) 
        else
          Lwt.return ()
    in
    let f entry =
      let i = Entry.i_of entry 
      and value = Entry.v_of entry 
      in
      match !acc with
	    | None ->
	      let () = acc := Some(i,value) in
	      Logger.debug_f_ "value %s has no previous" (Sn.string_of i) >>= fun () ->
	      Lwt.return ()
	        | Some (pi,pv) ->
	          if pi < i then
		        begin
                  maybe_log_progress pi >>= fun () ->
		          (* not happy with this, but we need to avoid that 
		             a node in catchup thinks it's master due to 
		             some lease starting in the past *)
		          let pv' = Value.clear_master_set pv in
		          S.safe_insert_value store pi pv' >>= fun _ ->
		          let () = acc := Some(i,value) in
		          Lwt.return ()
		        end
	          else
		        begin
		          Logger.debug_f_ "%s => skip" (Sn.string_of pi) >>= fun () ->
		          let () = acc := Some(i,value) in
		          Lwt.return ()
		        end
    in
    tlog_coll # iterate start_i too_far_i f >>= fun () ->
    begin
      match !acc with 
        | None -> Lwt.return ()
        | Some(i,value) -> 
            Logger.debug_f_ "%s => store" (Sn.string_of i) >>= fun () ->
            S.safe_insert_value store i value >>= fun _ -> Lwt.return ()
    end >>= fun () -> 
    let store_i' = S.consensus_i store in
    Logger.info_f_ "catchup_store completed, store is @ %s" 
      ( option2s Sn.string_of store_i')
  >>= fun () ->
    begin
      
	  let si = match store_i' with
        | Some i -> i
        | None -> Sn.start
      in
      let pred_too_far_i = Sn.pred too_far_i in
      if si < pred_too_far_i 
      then
        let msg = Printf.sprintf
          "Catchup store failed. Store counter is too low: %s < %s"
          (Sn.string_of si) (Sn.string_of pred_too_far_i)
        in
        Lwt.fail (StoreCounterTooLow msg)
      else
        Lwt.return ()
      end >>= fun () ->
    (* TODO: straighten interface *)
    let vo = match !acc with
      | None -> None
      | Some (i,v) -> Some v
    in
    Lwt.return (too_far_i, vo)
  end

let catchup me other_configs ~cluster_id dbt current_i mr_name (future_n,future_i) =
  Logger.info_f_ "CATCHUP start: I'm @ %s and %s is more recent (%s,%s)"
    (Sn.string_of current_i) mr_name (Sn.string_of future_n) 
    (Sn.string_of future_i)
  >>= fun () ->
  catchup_tlog me other_configs ~cluster_id current_i mr_name dbt >>= fun too_far_i ->
  catchup_store me dbt too_far_i >>= fun (end_i,vo) ->
  Logger.info_f_ "CATCHUP end" >>= fun () ->
  Lwt.return (future_n, end_i,vo)


let verify_n_catchup_store (type s) me ((module S : Store.STORE with type t = s), store, tlog_coll, ti_o) ~current_i forced_master =
  let io_s = Log_extra.option2s Sn.string_of  in
  let si_o = S.consensus_i store in
  Logger.info_f_ "verify_n_catchup_store; ti_o=%s current_i=%s si_o:%s" 
    (io_s ti_o) (Sn.string_of current_i) (io_s si_o) >>= fun () ->
   match ti_o, si_o with
    | None, None -> Lwt.return (0L,None)
    | Some 0L, None -> Lwt.return (0L,None)
    | Some i, Some j when i = Sn.succ j -> (* tlog 1 ahead of store *)
      begin
        let vo = tlog_coll # get_last_value i in
        Lwt.return (i,vo)
      end
    | Some i, Some j when i = j -> Lwt.return ((Sn.succ j),None)
    | Some i, Some j when i > j -> 
      begin
	    catchup_store me ((module S),store,tlog_coll) current_i >>= fun (end_i, vo) ->
	    Lwt.return (end_i,vo)
      end
    | Some i, None ->
      begin
	    catchup_store me ((module S),store,tlog_coll) current_i >>= fun (end_i, vo) ->
	    Lwt.return (end_i,vo)
      end
    | _,_ ->
      let msg = Printf.sprintf
	    "ti_o:%s, si_o:%s should not happen: tlogs have been removed?"
	    (io_s ti_o) (io_s si_o)
      in
      Logger.fatal_ msg >>= fun () ->
      let maybe a = function | None -> a | Some b -> b in
      Lwt.fail (StoreAheadOfTlogs(maybe (-1L) si_o, maybe (-1L) ti_o))

let get_db db_name cluster_id cfgs =

  let get_db_from_stream conn = 
    make_remote_nodestream cluster_id conn >>= fun (client:nodestream) ->
    client # get_db db_name 
  in
  let try_db_download m_success cfg =
    begin
      match m_success with
        | Some s -> Lwt.return m_success
        | None ->
          Lwt.catch 
          ( fun () ->
            Logger.info_f_ "get_db: Attempting download from %s" (Node_cfg.node_name cfg) >>= fun () ->
            let mr_addresses = Node_cfg.client_addresses cfg in
            _with_client_connection mr_addresses get_db_from_stream >>= fun () -> 
            Logger.info_ "get_db: Download succeeded" >>= fun () ->
            Lwt.return (Some (Node_cfg.node_name cfg))
          ) 
          ( fun e ->
            Logger.error_f_ "get_db: DB download from %s failed (%s)" (Node_cfg.node_name cfg) (Printexc.to_string e) >>= fun () -> 
            Lwt.return None
          )
    end
  in
  Lwt_list.fold_left_s try_db_download None cfgs 
