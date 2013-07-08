module Node = Core_types.Node
module Config = Core_types.Config
module Message = Core_types.Message
module Event = Core_types.Event
module Command = Core_types.Command

module State = struct
    type t = Core_state.t

    let sexp_of_t = Core_state.sexp_of_t
    let tag_of_t = Core_state.tag_of_t

    let handle = Core_dispatch.handle

    let state0 = Core_state.state0
end
