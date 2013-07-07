open Core_state
open Core_state.Master

type t = Master.t

let handle config state event =
    let new_state = Slave.Fields.create state.i in
    (Slave new_state, [])
