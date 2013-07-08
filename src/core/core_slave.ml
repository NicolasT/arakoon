open Core_types
open Core_types.Event
open Core_types.Command
open Core_types.Message

open Core_state
open Core_state.Slave

open Core_signatures

type t = Slave.t
type timeout_handler = Config.t -> t -> float -> handler_result
type 'a message_handler = Config.t -> t -> Node.t -> 'a -> handler_result

let handle_election_timeout config state _ =
    let next_n = N.succ state.n in
    let new_state = Candidate.Fields.create ~n:next_n ~i:state.i
    and commands = [ Log "Transition to Candidate"
                   ; Broadcast (Message.prepare next_n)
                   ] in
    (Candidate new_state, commands)

let handle_prepare config state from = function
  | m when m.Prepare.n < state.n ->
      let s = Printf.sprintf !"Ignoring old Prepare for n %{N}, mine is %{N}"
                  m.Prepare.n state.n in
      (Slave state, [Log s])
  | _ -> failwith "Not implemented"

let handle_promise config state from m =
    let s = Printf.sprintf "Ignoring Promise from %S in Slave state" from in
    (Slave state, [Log s])

let handle_accept config state from m = failwith "Not implemented"

let handle_accepted config state from m =
    let s = Printf.sprintf "Ignoring Accepted from %S in Slave state" from in
    (Slave state, [Log s])

let handle_nop config state from _ =
    (Slave state, [])
