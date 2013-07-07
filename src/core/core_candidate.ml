open Core_state
open Core_state.Candidate

type t = Candidate.t

let handle config state event =
    let new_state = Master.Fields.create state.i in
    (Master new_state, [])
