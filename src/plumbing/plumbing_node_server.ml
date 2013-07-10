open Async.Std
open Sexplib.Std
open Bin_prot.Std

open Core_api

module Server =
    Typed_tcp.Simple
        (struct type t = Node.t * Message.t with sexp, bin_io end)
        (struct type t = unit with sexp, bin_io end)

include Server
