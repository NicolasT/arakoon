open Core.Std

module Node : sig
    type t = string
    val compare : t -> t -> int

    include Binable with type t :=t
    include Sexpable with type t := t
end

module Config : sig
    module NodeSet : sig
        type t

        val of_list : Node.t list -> t
    end

    type t = { nodes : NodeSet.t
             ; me : Node.t
             ; election_timeout : Time.Span.t
             ; lease_extension_timeout : Time.Span.t
             }

    module Fields : sig
        val create :  nodes:NodeSet.t
                   -> me:Node.t
                   -> election_timeout:Time.Span.t
                   -> lease_extension_timeout:Time.Span.t
                   -> t
    end

    val sexp_of_t : t -> Sexp.t
end with type t = Core_types.Config.t

module Message : sig
    type t
    include Binable with type t := t
    include Sexpable with type t := t
end with type t = Core_types.Message.t

module Event : sig
    type t = Message of Node.t * Message.t
           | ElectionTimeout of (Time.Span.t * Time.Span.t)

    val sexp_of_t : t -> Sexp.t
end

module Command : sig
    type t = Log of string
           | Broadcast of Message.t
           | Send of Node.t * Message.t
           | ResetElectionTimeout of Time.Span.t

    val sexp_of_t : t -> Sexp.t
end

module State : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val tag_of_t : t -> [> `Candidate | `Master | `Slave ]

    val state0 : t
    val handle : Config.t -> t -> Event.t -> (t * Command.t list)
end
