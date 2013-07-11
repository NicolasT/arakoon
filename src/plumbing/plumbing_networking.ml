open Core.Std
open Async.Std

module ReconnectingClient : sig
    type timeout_fun = Time.Span.t Option.t -> Time.Span.t Option.t

    val run :  timeoutf:timeout_fun
            -> [< Socket.Address.t ] Tcp.where_to_connect
            -> 'a Pipe.Reader.t
            -> (Writer.t -> 'a -> unit)
            -> unit Deferred.t

    val doubling_timeout :  init:Time.Span.t
                         -> max:Time.Span.t
                         -> timeout_fun
    val const_timeout : Time.Span.t -> timeout_fun
end = struct
    type timeout_fun = Time.Span.t Option.t -> Time.Span.t Option.t

    let try_connect f addr =
        let rec go t =
            after (Option.value t ~default:Time.Span.zero) >>= fun () ->
            try_with ~rest:`Raise
                (fun () -> Tcp.connect addr)
            >>= function
              | Ok r -> return r
              | Error e ->
                  let t' = f t in
                  go t'
        in
        go None

    let run ~timeoutf addr pipe f =
        let go () =
            try_connect timeoutf addr >>= fun (s, _, w) ->
            Socket.shutdown s `Receive;

            Writer.transfer w pipe (f w)
        in
        let rec loop () =
            try_with go >>= function
              | Ok () -> return ()
              | Error e -> loop ()
        in
        loop ()

    let doubling_timeout ~init ~max = function
      | None -> Some init
      | Some t -> Some (Time.Span.scale t 2.0)

    let const_timeout t = fun _ -> Some t
end
