open Lwt

module S = (val (Store.make_store_module (module Batched_store.Local_store)))

let replay_tlogs tlog_dir db_name end_i = 
  let console x = Lwt_io.eprintlf x in
  let maybe_log_i start_i too_far_i pi = 
    let (+:) = Sn.add 
    and (-:) = Sn.sub
    and border = Sn.of_int 10
    and period = Sn.of_int 1000
    in
    let lo_b = start_i +: border 
    and hi_b = too_far_i -: border 
    in
    let pis = Sn.string_of pi in
    match pi with
      | pi when pi = lo_b                     -> console "\t%s\n\t..." pis
      | pi when pi < lo_b || pi > hi_b        -> console "\t%s"       pis
      | pi when (Sn.rem pi period) = Sn.start -> console "\t... %s ..." pis
      | _ -> Lwt.return ()
  in
  let t () = 
    Lwt.catch 
      (fun () ->
        S.make_store db_name >>= fun store ->
        let cio = S.consensus_i store in
        let cios = Log_extra.option2s Sn.string_of cio in
        console "store @ %s" cios >>= fun () ->
        begin
          match
            end_i with
              | None ->
                begin
                  Tlc2.get_last_tlog tlog_dir >>= fun (new_c,fn) ->
                  Tlc2._validate_one fn "" false >>= fun (last, index) -> 
                  let i = 
                    match last with
                     | None -> Sn.start
                     | Some e -> Sn.succ (Tlogcommon.Entry.i_of e)
                  in
                  Lwt.return i 
                end
              | Some i -> Lwt.return i
        end
        >>= fun too_far_i ->
        let start_i = 
          match cio with 
            | None     -> Sn.start
            | Some c_i -> Sn.succ c_i
        in
        console "replay: [%s,%s) " (Sn.string_of start_i) (Sn.string_of too_far_i) >>= fun () ->
        begin
          if start_i < too_far_i 
          then
            let acc = ref None in
            let log_i pi = maybe_log_i start_i too_far_i pi in
            let f = Catchup.make_f (module S) log_i acc store in
            Tlc2.iterate_tlog_dir tlog_dir ~index:None 
              start_i 
              too_far_i
              f
             >>= fun () ->
            Catchup.epilogue (module S) acc store >>= fun () ->
            S.close store
          else
            console "nothing to do"
        end 
        >>= fun () ->
        console "done" >>= fun () ->
        Lwt.return 0
      )
      (function e -> 
        console "error: %s" (Printexc.to_string e) >>= fun () ->
        Lwt.return 1
      )
  in
  Lwt_extra.run (t())


