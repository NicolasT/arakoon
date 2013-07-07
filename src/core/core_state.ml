open Sexplib.Std

open Core_types

module Master = struct
    type t = { i : I.t
             } with sexp, fields, compare
end

module Slave = struct
    type t = { i : I.t
             } with sexp, fields, compare
end

module Candidate = struct
    type t = { i : I.t
             } with sexp, fields, compare
end

type t = Master of Master.t
       | Slave of Slave.t
       | Candidate of Candidate.t
with sexp, variants, compare

let state0 = Slave { Slave.i = I.i0 }
