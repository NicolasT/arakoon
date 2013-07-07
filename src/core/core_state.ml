open Sexplib.Std

open Core_types

module Master = struct
    type t = { n : N.t
             ; i : I.t
             } with sexp, fields, compare
end

module Slave = struct
    type t = { n : N.t
             ; i : I.t
             } with sexp, fields, compare
end

module Candidate = struct
    type t = { n : N.t
             ; i : I.t
             } with sexp, fields, compare
end

type t = Master of Master.t
       | Slave of Slave.t
       | Candidate of Candidate.t
with sexp, variants, compare

let state0 =
    let t = Slave.Fields.create ~n:N.n0 ~i:I.i0 in
    Slave t
