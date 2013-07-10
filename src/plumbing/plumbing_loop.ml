open Core.Std
open Async.Std

open Core_types

module State = struct
    type t = { state : Core_api.State.t
             ; election_timeout : (Time.Span.t * float) Deferred.t
             ; node_messages : (Core_api.Node.t * Core_api.Message.t) Pipe.Reader.t
             } with sexp_of, fields
end

let init c s m =
    let ct = Unix.gettimeofday () in
    let et = after c.Config.election_timeout >>| fun () -> (c.Config.election_timeout, ct) in
    State.Fields.create
        ~state:s
        ~election_timeout:et
        ~node_messages:m

let find_event s =
    let open State in

    if Deferred.is_determined s.election_timeout
    then
        s.election_timeout >>| fun (g, t) ->
            let ct = Unix.gettimeofday () in
            let p = sec (ct -. t) in
            Ok (Core_api.Event.ElectionTimeout (g, p))
    else begin
        Deferred.any [ (s.election_timeout >>| fun s' -> `ElectionTimeout s')
                     ; (Pipe.values_available s.node_messages >>| fun r -> `NodeMessages r)
                     ]
        >>= function
          | `ElectionTimeout (a, b) ->
                let ct = Unix.gettimeofday () in
                let p = sec (ct -. b) in
                return (Ok (Core_api.Event.ElectionTimeout (a, p)))
          | `NodeMessages `Ok ->
              Pipe.read s.node_messages >>| begin function
                | `Ok (n, m) -> Ok (Core_api.Event.Message (n, m))
                | `Eof -> Error `Eof
              end
          | `NodeMessages `Eof ->
              return (Error `Eof)
    end

let eval_commands c0 s0 cmds =
    let open Core_api.Command in

    let f acc cmd =
        match acc with
          | Error e -> return (Error e)
          | Ok (c, s) ->
              printf !"C: %{sexp:Core_api.Command.t}\n" cmd;
              match cmd with
                | Log s' ->
                    printf "L: %s\n" s';
                    return (Ok (c, s))
                | ResetElectionTimeout t ->
                    let ct = Unix.gettimeofday () in
                    let et = after t >>| fun () -> (t, ct) in
                    return (Ok (c, { s with State.election_timeout = et }))
                | Send (_, _) ->
                    printf "Send not implemented\n";
                    return (Ok (c, s))
                | Broadcast _ ->
                    printf "Broadcast not implemented\n";
                    return (Ok (c, s))
    in

    Deferred.List.fold cmds ~init:(Ok(c0, s0)) ~f

let loop =
    let open State in
    let rec go c s =
        printf !"Pre-state: %{sexp:Core_api.State.t}\n" s.state;
        find_event s >>= function
          | Error e -> return (Error e)
          | Ok e ->
              printf !"E: %{sexp:Core_api.Event.t}\n" e;
              let (ns, cs) = Core_api.State.handle c s.state e in
              printf !"Post-state: %{sexp:Core_api.State.t}\n" ns;
              let s' = { s with state = ns } in
              eval_commands c s' cs >>= function
                | Ok (c', s'') -> go c' s''
                | Error e -> return (Error e)
    in
    go
