open Core_state
open Core_state.Candidate
open Core_types
open Core_signatures

open Core_types.Command

type t = Candidate.t
type timeout_handler = Config.t -> t -> float -> handler_result
type 'a message_handler = Config.t -> t -> Node.t -> 'a -> handler_result

let handle_election_timeout _ s _ =
    (* TODO *)
    let s' = Candidate s
    and cs = [ Log "Remaining in candidate state"
             ; ResetElectionTimeout 10.0
             ]
    in (s', cs)

let handle_prepare _ _ _ _ = failwith "Not implemented"
let handle_promise _ _ _ _ = failwith "Not implemented"
let handle_accept _ _ _ _ = failwith "Not implemented"
let handle_accepted _ _ _ _ = failwith "Not implemented"
let handle_nop _ _ _ _ = failwith "Not implemented"
