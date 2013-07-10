open Core.Std

module Node = struct
    type t = string
    with sexp, compare, bin_io
end

module N : sig
    type t

    val compare : t -> t -> int

    include Binable with type t := t
    include Sexpable with type t := t

    val to_string : t -> string

    val n0 : t
    val succ : t -> t
end = struct
    type t = int
    with sexp, compare, bin_io

    let n0 = 0
    let succ = Pervasives.succ
    let to_string = string_of_int
end

module I : sig
    type t

    val compare : t -> t -> int

    include Binable with type t := t
    include Sexpable with type t := t

    val to_string : t -> string

    val i0 : t
    val succ : t -> t
end = struct
    type t = int
    with sexp, compare, bin_io

    let i0 = 0
    let succ = Pervasives.succ
    let to_string = string_of_int
end

module V = struct
    type t = User of string
           | MasterSet of Node.t
    with sexp, variants, compare, bin_io
end

module Config = struct
    module NodeSet = Set.Make(Node)

    type t = { nodes : NodeSet.t
             ; me : Node.t
             ; election_timeout : Time.Span.t
             ; lease_extension_timeout : Time.Span.t
             } with sexp, fields
end

module Message = struct
    module Prepare = struct
        type t = { n : N.t
                 } with sexp, fields, compare, bin_io
    end
    module Promise = struct
        type t = { n : N.t
                 ; i : I.t
                 ; v : V.t
                 } with sexp, fields, compare, bin_io
    end
    module Accept = struct
        type t = { n : N.t
                 ; i : I.t
                 ; v : V.t
                 } with sexp, fields, compare, bin_io
    end
    module Accepted = struct
        type t = { n : N.t
                 ; i : I.t
                 ; v : V.t
                 } with sexp, fields, compare, bin_io
    end
    module Nop : sig
        type t

        include Binable with type t := t
        include Sexpable with type t := t

        val compare : t -> t -> int

        val singleton : t
    end = struct
        type t = unit
        with sexp, compare, bin_io

        let singleton = ()
    end

    type t = Nop of Nop.t
           | Prepare of Prepare.t
           | Promise of Promise.t
           | Accept of Accept.t
           | Accepted of Accepted.t
    with sexp, variants, compare, bin_io

    let nop = Nop Nop.singleton
    let prepare ~n = Prepare (Prepare.Fields.create ~n)
    let promise ~n ~i ~v = Promise (Promise.Fields.create ~n ~i ~v)
    let accept ~n ~i ~v = Accept (Accept.Fields.create ~n ~i ~v)
    let accepted ~n ~i ~v = Accepted (Accepted.Fields.create ~n ~i ~v)
end

module Event = struct
    type t = Message of Node.t * Message.t
           | ElectionTimeout of (Time.Span.t * Time.Span.t)
    with sexp, variants, compare
end

module Command = struct
    type t = Log of string
           | Broadcast of Message.t
           | Send of Node.t * Message.t
           | ResetElectionTimeout of Time.Span.t
    with sexp, variants, compare
end
