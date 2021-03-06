open Core.Std

open Core_api

let dump p s = Printf.printf "%s: %s\n" p (Sexp.to_string_hum s)

let config =
    let nodes = Config.NodeSet.of_list ["node0"]
    and me = "node0"
    and election_timeout = sec 10.0
    and lease_extension_timeout = sec 5.0 in
    Config.Fields.create ~nodes ~me ~election_timeout ~lease_extension_timeout

let main () =
    let state = State.state0 in
    dump "Pre" (State.sexp_of_t state);
    let event = Event.ElectionTimeout (sec 10.0, sec 10.1) in
    dump "Event" (Event.sexp_of_t event);
    let (state', commands) = Core_api.State.handle config state event in
    dump "Post" (State.sexp_of_t state');
    dump "Commands" ((<:sexp_of< Command.t list >>) commands);
    let sn = match State.tag_of_t state' with
      | `Master -> "Master"
      | `Slave -> "Slave"
      | `Candidate -> "Candidate"
    in
    Printf.printf "Current state: %s\n" sn
;;

main ()
