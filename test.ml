open Core.Std

open Core_api

let dump p s = Printf.printf "%s: %s\n" p (Sexp.to_string_hum s)

let config () =
    let nodes = Bag.create () in
    Config.Fields.create nodes

let main () =
    let state = State.state0 in
    dump "Pre" (State.sexp_of_t state);
    let event = Event.Message ("node0", Message.Nop) in
    dump "Event" (Event.sexp_of_t event);
    let (state', commands) = Core_api.State.handle (config ()) state event in
    dump "Post" (State.sexp_of_t state');
    dump "Commands" ((<:sexp_of< Command.t list >>) commands)
;;

main ()
