open Core.Std

open Core_state
open Core_state.Candidate
open Core_types
open Core_signatures

type t = Master.t
type timeout_handler = Config.t -> t -> Time.Span.t -> Time.Span.t -> handler_result
type 'a message_handler = Config.t -> t -> Node.t -> 'a -> handler_result

let handle_election_timeout _ _ _ _ = failwith "Not implemented"
let handle_prepare _ _ _ _ = failwith "Not implemented"
let handle_promise _ _ _ _ = failwith "Not implemented"
let handle_accept _ _ _ _ = failwith "Not implemented"
let handle_accepted _ _ _ _ = failwith "Not implemented"
let handle_nop _ _ _ _ = failwith "Nop implemented"
