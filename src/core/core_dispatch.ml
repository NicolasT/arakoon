open Core_state
open Core_types.Event
open Core_types.Message
open Core_signatures

let dispatch (type t) (module M : HANDLER with type t = t) config (state:t) = function
  | ElectionTimeout (g, e) -> M.handle_election_timeout config state g e
  | Message (from, m) -> match m with
    | Prepare m' -> M.handle_prepare config state from m'
    | Promise m' -> M.handle_promise config state from m'
    | Accept m' -> M.handle_accept config state from m'
    | Accepted m' -> M.handle_accepted config state from m'
    | Nop m' -> M.handle_nop config state from m'

let handle config state event = match state with
  | Master s -> dispatch (module Core_master) config s event
  | Slave s -> dispatch (module Core_slave) config s event
  | Candidate s -> dispatch (module Core_candidate) config s event
