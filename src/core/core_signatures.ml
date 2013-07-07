open Core_types

module type HANDLER = sig
    type t

    val handle : Config.t -> t -> Event.t -> (Core_state.t * Command.t list)
end
