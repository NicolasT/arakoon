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

let handle_prepare config state from m = failwith "Not implemented"

let handle_promise config state from m =
    let s = Printf.sprintf "Ignoring Promise from %S in Slave state" from in
    (Slave state, [Command.Log s])

let handle_accept config state from m = failwith "Not implemented"

let handle_accepted config state from m =
    let s = Printf.sprintf "Ignoring Accepted from %S in Slave state" from in
    (Slave state, [Command.Log s])

let handle_nop config state from =
    (Slave state, [])

let handle_message config state from = function
  | Message.Prepare m -> handle_prepare config state from m
  | Message.Promise m -> handle_promise config state from m
  | Message.Accept m -> handle_accept config state from m
  | Message.Accepted m -> handle_accepted config state from m
  | Message.Nop -> handle_nop config state from

let handle config state = function
  | Event.ElectionTimeout _ -> handle_election_timeout config state
  | Event.Message (f, m) -> handle_message config state f m
