open Core.Std

open Core_types
open Core_types.Message

type handler_result = Core_state.t * Command.t list

module type HANDLER = sig
    type t

    type timeout_handler = Config.t -> t -> Time.Span.t -> Time.Span.t -> handler_result
    type 'a message_handler = Config.t -> t -> Node.t -> 'a -> handler_result

    val handle_election_timeout : timeout_handler
    val handle_prepare : Prepare.t message_handler
    val handle_promise : Promise.t message_handler
    val handle_accept : Accept.t message_handler
    val handle_accepted : Accepted.t message_handler
    val handle_nop : Nop.t message_handler
end
