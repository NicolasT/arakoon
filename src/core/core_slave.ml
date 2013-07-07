open Core_types

open Core_state
open Core_state.Slave

type t = Slave.t

let handle config state event =
    let new_state = Candidate.Fields.create state.i in
    (Candidate new_state, [Command.Log "Transition to Candidate"])
