open Core.Std

module Node : sig
    type t = string
    val compare : t -> t -> int
end

module Config : sig
    module NodeSet : sig
        type t

        val of_list : Node.t list -> t
    end

    type t = { nodes : NodeSet.t
             }

    module Fields : sig
        val create : nodes:NodeSet.t -> t
    end

    val sexp_of_t : t -> Sexp.t
end

module Message : sig
    type t

    val sexp_of_t : t -> Sexp.t
end

module Event : sig
    type t = Message of Node.t * Message.t
           | ElectionTimeout of float

    val sexp_of_t : t -> Sexp.t
end

module Command : sig
    type t

    val sexp_of_t : t -> Sexp.t
end

module State : sig
    type t

    val sexp_of_t : t -> Sexp.t

    val state0 : t
    val handle : Config.t -> t -> Event.t -> (t * Command.t list)
end
