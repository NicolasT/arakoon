open Core_state

let handle config state event = match state with
    | Master s -> Core_master.handle config s event
    | Slave s -> Core_slave.handle config s event
    | Candidate s -> Core_candidate.handle config s event
