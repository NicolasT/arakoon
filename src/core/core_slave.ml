open Core_types

open Core_state
open Core_state.Slave

type t = Slave.t

let handle_election_timeout config state =
    let next_n = N.succ state.n in
    let msg = Message.prepare next_n in
    let new_state = Candidate.Fields.create ~n:next_n ~i:state.i
    and commands = [ Command.Log "Transition to Candidate"
                   ; Command.Broadcast msg
                   ] in
    (Candidate new_state, commands)

let handle config state = function
  | Event.ElectionTimeout _ -> handle_election_timeout config state
  | _ -> failwith "Not implemented"
