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

open Multi_paxos_type
open Master_type
open Multi_paxos
open Lwt
open Mp_msg.MPMessage
open Update

(* a (possibly potential) master has found consensus on a value
   first potentially finish of a client request and then on to
   being a stable master *)

let master_consensus constants ((finished_funs : master_option),v,n,i) () =
  let gen_e = EConsensus(finished_funs, v,n,i) in
  let log_e = explain "on_consensus for : %s => %i finished_fs " 
    (Value.value2s v) (List.length finished_funs)
  in
  let state = (v,n,(Sn.succ i)) in
  Fsm.pure ~sides:[gen_e;log_e] (Stable_master state)
    


let stable_master constants ((v',n,new_i) as current_state) = function
  | LeaseExpired n' ->
      let me = constants.me in
      if n' < n 
      then
        let log_e = explain "stable_master: ignoring old lease_expired with n:%s < n:%s" 
	      (Sn.string_of n') (Sn.string_of n)
        in
	    Fsm.pure ~sides:[log_e] (Stable_master current_state)
      else
	    begin
	      let extend () = 
            let log_e = explain "stable_master: half-lease_expired: update lease."  in
            let v = Value.create_master_value (me,0L) in
            let ff = fun _ -> Lwt.return () in
		    (* TODO: we need election timeout as well here *)
	        Fsm.pure ~sides:[log_e] (Master_dictate ([ff], v,n,new_i))
	      in
	      match constants.master with
	        | Preferred ps when not (List.mem me ps) ->
              let lws = List.map (fun name -> (name, constants.last_witnessed name)) ps in
              let slws = List.fast_sort (fun (_, b) (_, a) -> compare a b) lws in
              let (p, p_i) = List.hd slws in
	          let diff = Sn.diff new_i p_i in
	          if diff < (Sn.of_int 5) 
              then
                let log_e = explain "stable_master: handover to %s" p in
		        Fsm.pure ~sides:[log_e] (Stable_master current_state)
	          else
		        extend () 
	        | _ -> extend()
	    end
    | FromClient ufs ->
        begin
          let updates, finished_funs = List.split ufs in
          let synced = List.fold_left (fun acc u -> acc || Update.is_synced u) false updates in
          let value = Value.create_client_value updates synced in
	      Fsm.pure (Master_dictate (finished_funs, value, n, new_i))
        end
    | FromNode (msg,source) ->
      begin
	    let me = constants.me in
	    match msg with
	      | Prepare (n',i') ->
	        begin
              if am_forced_master constants me
	          then
		        begin
		          let reply = Nak(n', (n,new_i)) in
                  let send_e = ESend (reply, source) in
                  let sides = [send_e] in
                  let state' = 
		          if n' > 0L 
		          then
		            let new_n = update_n constants n' in
		            Forced_master_suggest (new_n,new_i)
		          else
		            Stable_master current_state
                  in
                  Fsm.pure ~sides state'
		        end
	          else
		        begin
		          match handle_prepare constants source n n' i' with
		            | Nak_sent sides
		            | Prepare_dropped sides      -> Fsm.pure ~sides  (Stable_master current_state )
		            | Promise_sent_up2date sides ->
		              begin
                        let l_val = constants.tlog_coll # get_last () in
			            Fsm.pure ~sides (Slave_wait_for_accept (n', new_i, None, l_val))
		              end
		            | Promise_sent_needs_catchup sides ->
                      let i = Store.get_succ_store_i constants.store in
                      Fsm.pure ~sides (Slave_discovered_other_master (source, i, n', i'))
		        end
	        end
          | Accepted(n,i) -> 
              (* This one is not relevant anymore, but we're interested
                 to see the slower slaves in the statistics as well :
                 TODO: should not be solved on this level.
              *)
              let () = constants.on_witness source i in
              Fsm.pure (Stable_master current_state)
	      | _ ->
	          begin
                let log_e = explain "stable_master received %S: dropping" (string_of msg) in
	            Fsm.pure ~sides:[log_e] (Stable_master current_state)
	          end
      end
    | ElectionTimeout n' -> 
      begin
        let log_e = explain "ignoring election timeout (%s)" (Sn.string_of n') in          
        Fsm.pure ~sides:[log_e] (Stable_master current_state)
      end
    | Quiesce (sleep,awake) ->
      begin
        let e = EGen(fun() ->
          fail_quiesce_request constants.store sleep awake Quiesced_fail_master)
        in
        Fsm.pure ~sides:[e] (Stable_master current_state)
      end
        
    | Unquiesce -> failwith "Unexpected unquiesce request while running as master"
                
(* a master informes the others of a new value by means of Accept
   messages and then waits for Accepted responses *)

let master_dictate constants (mo,v,n,i) () =
  let accept_e = EAccept (v,n,i) in
  
  let start_e = EStartLeaseExpiration (v,n,false) in
  let mcast_e = EMCast (Accept(n,i,v)) in
  let me = constants.me in
  let others = constants.others in
  let needed = constants.quorum_function (List.length others + 1) in
  let needed' = needed - 1 in
  let ballot = (needed' , [me] ) in
  
  let log_e = explain "master_dictate n:%s i:%s needed:%d" 
    (Sn.string_of n) (Sn.string_of i) needed' 
  in
  let sides = [accept_e;start_e;mcast_e;log_e] in
  Fsm.pure ~sides (Accepteds_check_done (mo, n, i, ballot, v))
