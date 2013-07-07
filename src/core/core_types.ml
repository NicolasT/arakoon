open Core.Std

module Node = struct
    type t = string
           with sexp, compare
end

module I : sig
    type t

    val compare : t -> t -> int

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t

    val i0 : t
    val succ : t -> t
end = struct
    type t = I of int
           with sexp, compare

    let i0 = I 0
    let succ (I i) = I (i + 1)
end

module Config = struct
    module NodeSet = Set.Make(Node)

    type t = { nodes : NodeSet.t
             } with sexp, fields
end

module Message = struct
    type t = Nop
           with sexp, variants, compare
end

module Event = struct
    type t = Message of Node.t * Message.t
           with sexp, variants, compare
end

module Command = struct
    type t = Log of string
           with sexp, variants, compare
end
