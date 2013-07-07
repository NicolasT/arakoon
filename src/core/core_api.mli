open Core.Std

module Node : sig
    type t = string
end

module Config : sig
    type t = { nodes : Node.t Bag.t
             }

    module Fields : sig
        val create : nodes:Node.t Bag.t -> t
    end

    val sexp_of_t : t -> Sexp.t
end

module Message : sig
    type t = Nop

    val sexp_of_t : t -> Sexp.t
end

module Event : sig
    type t = Message of Node.t * Message.t

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
