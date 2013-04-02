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
open Multi_paxos
open Lwt
open Mp_msg.MPMessage
open Update

let time_for_elections constants n' maybe_previous =
  begin
    if constants.store # quiesced () 
    then false, "quiesced"
    else
      begin
	    let origine,(am,al) = 
          match constants.store # who_master() with
		    | None         -> "not_in_store", ("None", Sn.start) 
		    | Some (sm,sd) -> "stored", (sm,sd) 
	    in
	    let now = Int64.of_float (Unix.time()) in
	    (* 
           let ns' = Sn.string_of n' in
           Lwt_log.debug_f "time_for_elections: lease expired(n'=%s) (lease:%s (%s,%s) now=%s"
		  ns' origine am (Sn.string_of al) (Sn.string_of now) >>= fun () -> 
        *)
	    let diff = abs (Int64.to_int (Int64.sub now al)) in
	    diff >= constants.lease_expiration, Printf.sprintf "diff=%i" diff
      end
  end

(* a forced slave or someone who is outbidded sends a fake prepare
   in order to receive detailed info from the others in a Nak msg *)
let slave_fake_prepare constants (current_i,current_n) () =
  (* fake a prepare, and hopefully wake up a dormant master *)
  let log_e = explain "slave_fake_prepare: sending Prepare(-1)" in
  let fake = Prepare( Sn.of_int (-1), current_i) in
  let mcast_e = EMCast fake in
  Fsm.pure ~sides:[log_e;mcast_e] (Slave_waiting_for_prepare (current_i,current_n))

(* a pending slave that is in sync on i and is ready
   to receive accepts *)
let slave_steady_state constants ((n,i,previous) as state) event =
  let store = constants.store in
  match event with
    | FromNode (msg,source) ->
      begin
	    match msg with
	      | Accept (n',i',v) when (n',i') = (n,i) ->
            begin
              let store_e = 
		        match store # consensus_i ()  with
		          | None -> EConsensusX (previous, n, Sn.pred i)
		          | Some store_i ->
                    let prev_i = Sn.pred i in
                    if (Sn.compare store_i (Sn.pred prev_i) ) == 0
                    then EConsensusX (previous, n,prev_i) 
                    else
			          if Sn.compare store_i prev_i == 0
			          then explain "Preventing re-push of : %s. Store at %s" 
                        (Sn.string_of prev_i) (Sn.string_of store_i) 
			          else failwith (Printf.sprintf "Illegal push requested: %s. Store at %s" 
                                       (Sn.string_of prev_i) (Sn.string_of store_i))
              in
	          let reply = Accepted(n,i) in
              let accept_e = EAccept (v,n,i) in
              let start_e = EStartLeaseExpiration(v,n, true) in
              let send_e = ESend(reply, source) in
              let log_e = explain "steady_state :: replying with %S" (string_of reply) in
              let sides = [store_e;accept_e;start_e; send_e; log_e] in
              Fsm.pure ~sides (Slave_steady_state (n, Sn.succ i, v))
	        end
	      | Accept (n',i',v) when (n'<=n && i'<i) || (n'< n && i'=i)  ->
	        begin
              let sides = [explain "slave_steady_state received old %S for my n, ignoring" 
		        (string_of msg)]
              in
	          Fsm.pure ~sides (Slave_steady_state state)
	        end
	      | Accept (n',i',v) ->
	        begin
              let cu_pred = Store.get_succ_store_i constants.store in
              let new_state = (source,cu_pred,n',i') in 
              let sides = [explain
                "slave_steady_state foreign (%s,%s) from %s <> local (%s,%s) discovered other master"
		        (Sn.string_of n') (Sn.string_of i') source (Sn.string_of  n) (Sn.string_of  i)]
              in
              Fsm.pure ~sides (Slave_discovered_other_master new_state ) 
	        end
	      | Prepare(n',i') ->
	        begin
              match handle_prepare constants source n n' i' with
		        | Prepare_dropped sides 
		        | Nak_sent sides              -> Fsm.pure ~sides (Slave_steady_state state)
		        | Promise_sent_up2date sides  ->
		          let next_i = Store.get_succ_store_i constants.store in
		          Fsm.pure ~sides (Slave_wait_for_accept (n', next_i, None, None))
		        | Promise_sent_needs_catchup sides ->
		          let i = Store.get_succ_store_i constants.store in
		          let new_state = (source, i, n', i') in 
		          Fsm.pure ~sides (Slave_discovered_other_master new_state ) 
	        end
	      | Nak _ 
	      | Promise _ 
	      | Accepted _ ->
            let sides = [explain "steady state :: dropping %s" (string_of msg)] in
	        Fsm.pure ~sides (Slave_steady_state state)
	          
      end
    | ElectionTimeout n' ->
      begin
        let sides = [explain "steady state :: ignoring election timeout"] in
        Fsm.pure  ~sides (Slave_steady_state state)
      end
    | LeaseExpired n' -> 
      let ns  = (Sn.string_of n) 
      and ns' = (Sn.string_of n') in
      if (not (is_election constants || constants.is_learner)) || n' < n
      then 
	    begin
          let sides = [explain "steady state: ignoring old lease expiration (n'=%s,n=%s)" ns' ns] in
	      Fsm.pure ~sides (Slave_steady_state (n,i,previous))
	    end
      else
	    begin 
	      let elections_needed, msg = time_for_elections constants n' (Some (previous,Sn.pred i)) in
	      if elections_needed then
	        begin
	          let new_n = update_n constants n in
              let el_i = Store.get_succ_store_i constants.store in
              let el_up =
		        begin
		          if el_i = (Sn.pred i) 
		          then Some previous
		          else None
		        end
              in
              let sides = [explain "ELECTIONS NEEDED"] in
	          Fsm.pure ~sides (Election_suggest (new_n, el_i, el_up ))
	        end
	      else
	        begin
              let sides = [explain
                "slave_steady_state ignoring lease expiration (n'=%s,n=%s) %s" ns' ns msg]
              in
	          Fsm.pure ~sides (Slave_steady_state state)
	        end
	    end
    | FromClient ufs -> 
      begin
        
          (* there is a window in election 
	         that allows clients to get through before the node became a slave
	         but I know I'm a slave now, so I let the update fail.
          *)      
        let e = EGen(fun () ->
          let updates,finished_funs = List.split ufs in
          let result = Store.Update_fail (Arakoon_exc.E_NOT_MASTER, "Not_Master") in
          let rec loop = function
            | []       -> Lwt.return ()
            | f :: ffs -> f result >>= fun () ->
              loop ffs
          in
          loop finished_funs )
        in
        Fsm.pure ~sides:[e] (Slave_steady_state state)
      end
    | Quiesce (sleep,awake) ->
      begin
        let e = EGen(fun () ->
          handle_quiesce_request constants.store sleep awake 
        ) in
        
        Fsm.pure ~sides:[e] (Slave_steady_state state)
      end
        
    | Unquiesce ->
      begin
        let e = EGen (fun () ->
          handle_unquiesce_request constants n >>= fun (store_i, vo) ->
          Lwt.return ()
        )
        in
        Fsm.pure ~sides:[e] (Slave_steady_state state)
      end
        
(* a pending slave that has promised a value to a pending master waits
   for an Accept from the master about this *)
let slave_wait_for_accept constants ((n,i, vo, maybe_previous) as state) event =
  match event with 
    | FromNode(msg,source) ->
      begin
	    match msg with
	      | Prepare (n',i') ->
            begin
	          let () = constants.on_witness source i' in
              match handle_prepare constants source n n' i' with
                | Prepare_dropped sides      -> Fsm.pure ~sides ( Slave_wait_for_accept state)
                | Nak_sent sides             -> Fsm.pure ~sides ( Slave_wait_for_accept state)
                | Promise_sent_up2date sides -> Fsm.pure ~sides ( Slave_wait_for_accept 
                                                                       (n',i,vo, maybe_previous))
                | Promise_sent_needs_catchup sides -> 
                  let i = Store.get_succ_store_i constants.store in
                  let state' = (source, i, n', i') in 
                  Fsm.pure ~sides ( Slave_discovered_other_master state')
            end
          | Accept (n',i',v) when n'=n ->
            begin
              let () = constants.on_witness source i' in
              let tlog_coll = constants.tlog_coll in
              let tlc_i = tlog_coll # get_last_i () in
              if i' < tlc_i 
              then
                begin
                  let sides = [explain
                    "slave_wait_for_accept: dropping old accept (i=%s , i'=%s)" 
                    (Sn.string_of i) (Sn.string_of i')]
                  in
                  Fsm.pure ~sides (Slave_wait_for_accept state)
                end
              else
                begin
	              if i' > i 
                  then 
                    let cu_pred = Store.get_succ_store_i constants.store in
                    Fsm.pure (Slave_discovered_other_master(source, cu_pred, n', i'))   
                  else
                    begin
                      let accept_e = EAccept(v,n,i') in
                      let lease_e = EStartLeaseExpiration(v,n, true) in
                      let consensus_e = 
                        match maybe_previous with
		                  | None -> explain "No previous" 
		                  | Some( pv, pi ) -> 
                            let store_i = constants.store # consensus_i () in
                            begin
		                      match store_i with
		                        | Some s_i ->
			                      if (Sn.compare s_i pi) == 0 
			                      then explain "slave_wait_for_accept: Not pushing previous"
			                      else EConsensusX(pv,n,pi)
                                | None -> EConsensusX(pv,n,pi) 
                          end
                      in
	                  let reply = Accepted(n,i') in
                      let log_e = explain "replying with %S" (string_of reply) in
                      let send_e = ESend (reply,source) in
                      let sides = [log_e;accept_e;lease_e;consensus_e;send_e] in
	                  Fsm.pure ~sides (Slave_steady_state (n, Sn.succ i', v))
	                end
                end
            end
          | Accept (n',i',v) when n' < n ->
            begin
              if i' > i 
              then
                let sides = [explain
                  "slave_wait_for_accept: Got accept from other master with higher i (i: %s , i' %s)" 
                  (Sn.string_of i) (Sn.string_of i')]  
                in
                let cu_pred = Store.get_succ_store_i constants.store in
                let new_state = (source, cu_pred, n', i') in 
                Fsm.pure ~sides (Slave_discovered_other_master new_state) 
              else
                let sides = [explain "slave_wait_for_accept: dropping old accept: %s " (string_of msg)] in
	            Fsm.pure ~sides (Slave_wait_for_accept state)
	        end
	      | Accept (n',i',v) ->
	        begin
              let sides = [explain "slave_wait_for_accept : foreign(%s,%s) <> (%s,%s) sending fake prepare" 
		        (Sn.string_of n') (Sn.string_of i') (Sn.string_of n) (Sn.string_of i)] 
              in
	          Fsm.pure ~sides (Slave_fake_prepare (i,n'))
	        end
	      | Promise _
          | Nak _
          | Accepted _ ->
	        begin
              let sides = [explain "dropping : %s" (string_of msg)] in
	          Fsm.pure ~sides (Slave_wait_for_accept state)
	        end
      end
    | ElectionTimeout n' 
    | LeaseExpired n' ->
      if (not (is_election constants || constants.is_learner)) || n' < n
      then 
        begin
          let ns = (Sn.string_of n) 
          and ns' = (Sn.string_of n') in
          let sides = [explain
            "slave_wait_for_accept: Ingoring old lease expiration (n'=%s n=%s)" ns' ns ]
          in
          Fsm.pure ~sides (Slave_wait_for_accept state)
        end
      else
        let elections_needed,_ = time_for_elections constants n' maybe_previous in
        if elections_needed 
        then
          begin
            let el_i = Store.get_succ_store_i constants.store in
            let el_up = constants.get_value el_i in
            let new_n = update_n constants n in
            let sides = [explain "slave_wait_for_accept: Elections needed"] in
            Fsm.pure ~sides (Election_suggest (new_n, el_i, el_up))
          end
        else
          Fsm.pure (Slave_wait_for_accept state)
    | FromClient msg -> failwith "slave_wait_for_accept only registered for FromNode"
      
    | Quiesce (sleep,awake) ->
      let e = EGen (fun () ->
        handle_quiesce_request constants.store sleep awake)
      in
      Fsm.pure ~sides:[e] (Slave_wait_for_accept state)
    | Unquiesce ->
      let e = EGen (fun () ->
        handle_unquiesce_request constants n >>= fun (store_i, store_vo) -> 
        Lwt.return ())
      in
      Fsm.pure ~sides:[e] (Slave_wait_for_accept state)
        
        
(* a pending slave that discovered another master has to do
   catchup and then go to steady state or wait_for_accept
   depending on if there was an existing value or not *)
        
let slave_discovered_other_master constants state () =
  let (master, current_i, future_n, future_i) = state in
  let me = constants.me
  and other_cfgs = constants.other_cfgs
  and store = constants.store
  and tlog_coll = constants.tlog_coll
  in
  if current_i < future_i then
    begin
      log ~me "slave_discovered_other_master: catching up from %s" master >>= fun() ->
      let m_val = tlog_coll # get_last_value current_i in
      let reply = Promise(future_n, current_i, m_val) in
      constants.send reply me master >>= fun () ->
      let cluster_id = constants.cluster_id in
      Catchup.catchup me other_cfgs ~cluster_id (store, tlog_coll) current_i master (future_n, future_i) 
      >>= fun (future_n', current_i', vo') ->
      begin
	    let fake = Prepare( Sn.of_int (-2), (* make it completely harmless *)
			                Sn.pred current_i') (* pred =  consensus_i *)
	    in
	    Act.multi_cast constants fake >>= fun () ->
	    match vo' with
	      | Some v ->
            begin
              let sides = [EStartLeaseExpiration(v, future_n', true)] in 
              Fsm.return ~sides (Slave_steady_state (future_n', current_i', v))
            end
	      | None -> 
            let vo =
              begin
                match vo' with
                  | None -> None
                  | Some u -> Some ( u, current_i' )
              end in
            let sides = [EStartElectionTimeout future_n] in
            Fsm.return ~sides (Slave_wait_for_accept (future_n', current_i', None, vo))
      end
    end
  else if current_i = future_i then
    begin
      let prom_val = constants.get_value future_i in
      let reply = Promise(future_n, future_i, prom_val ) in
      let send_e = ESend (reply, master) 
      and start_e = EStartElectionTimeout future_n 
      and log_e = explain "slave_discovered_other_master: no need for catchup %s" master 
      and last = tlog_coll # get_last () in
      Fsm.return ~sides:[send_e;start_e;log_e] (Slave_wait_for_accept (future_n, current_i, None, last))
    end
  else
    begin
      let next_i = Store.get_succ_store_i constants.store in
      let s, m =
        if is_election constants
        then 
	      (* we have to go to election here or we can get in a situation where
	         everybody just waits for each other *)
	      let new_n = update_n constants future_n in
	      let tlog_coll = constants.tlog_coll in
	      let l_up_v = tlog_coll # get_last_value next_i in
	      (Election_suggest (new_n, next_i, l_up_v)), 
          "my i is bigger then theirs ; back to election"
        else
          begin
            Slave_wait_for_accept( future_n, next_i, None, None ),
            "forced slave, back to slave mode" 
          end
      in
      let sides = [explain "slave_discovered_other_master: %s" m] in
      Fsm.return ~sides s
    end
