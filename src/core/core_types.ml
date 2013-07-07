open Core.Std

module Node = struct
    type t = string
    with sexp, compare
end

module N : sig
    type t

    val compare : t -> t -> int

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t

    val n0 : t
    val succ : t -> t
end = struct
    type t = int
    with sexp, compare

    let n0 = 0
    let succ = Pervasives.succ
end

module I : sig
    type t

    val compare : t -> t -> int

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t

    val i0 : t
    val succ : t -> t
end = struct
    type t = int
    with sexp, compare

    let i0 = 0
    let succ = Pervasives.succ
end

module V = struct
    type t = User of string
           | MasterSet of Node.t
    with sexp, variants, compare
end

module Config = struct
    module NodeSet = Set.Make(Node)

    type t = { nodes : NodeSet.t
             } with sexp, fields
end

module Message = struct
    module Prepare = struct
        type t = { n : N.t
                 } with sexp, fields, compare
    end
    module Promise = struct
        type t = { n : N.t
                 ; i : I.t
                 ; v : V.t
                 } with sexp, fields, compare
    end
    module Accept = struct
        type t = { n : N.t
                 ; i : I.t
                 ; v : V.t
                 } with sexp, fields, compare
    end
    module Accepted = struct
        type t = { n : N.t
                 ; i : I.t
                 ; v : V.t
                 } with sexp, fields, compare
    end

    type t = Nop
           | Prepare of Prepare.t
           | Promise of Promise.t
           | Accept of Accept.t
           | Accepted of Accepted.t
    with sexp, variants, compare

    let prepare ~n = Prepare (Prepare.Fields.create ~n)
    let promise ~n ~i ~v = Promise (Promise.Fields.create ~n ~i ~v)
    let accept ~n ~i ~v = Accept (Accept.Fields.create ~n ~i ~v)
    let accepted ~n ~i ~v = Accepted (Accepted.Fields.create ~n ~i ~v)
end

module Event = struct
    type t = Message of Node.t * Message.t
           | ElectionTimeout of float
    with sexp, variants, compare
end

module Command = struct
    type t = Log of string
           | Broadcast of Message.t
           | Send of Node.t * Message.t
           | ResetElectionTimeout of float
    with sexp, variants, compare
end
