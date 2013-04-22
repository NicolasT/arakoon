
open Camlp4.PreCast


let module_name _loc =
  let file_name = Loc.file_name _loc in
  if file_name = "" then
    ""
  else
    String.capitalize (Filename.basename (try
                                            Filename.chop_extension file_name
                                          with Invalid_argument _ ->
                                            file_name))

let rec apply e = function
  | [] -> e
  | x :: l -> let _loc = Ast.loc_of_expr x in apply <:expr< $e$ $x$ >> l

let split e =
  let rec aux acc = function
    | <:expr@_loc< explain $format_string$ >> ->
        `Explain(format_string, acc)
    | <:expr@loc< $a$ $b$ >> -> begin
        match b with
          | b ->
              aux (b :: acc) a
      end
    | _ ->
        `Not_explain
  in
  aux [] e

let make_loc _loc =
  <:expr<
    ($str:Loc.file_name _loc$,
     $int:string_of_int (Loc.start_line _loc)$,
     $int:string_of_int (Loc.start_off _loc - Loc.start_bol _loc)$)
  >>

let map =
object
  inherit Ast.map as super

  method expr e =
    let _loc = Ast.loc_of_expr e in
    match split e with
      | `Explain(format_string, args) ->
          let args = List.map super#expr args in
          <:expr<
            (if Lwt_log.Section.level section <= Lwt_log.Debug
            then ELog (fun b -> Buffer.add_string b ($apply <:expr< Printf.sprintf $format_string$ >> args$) )
            else ENop) >>
      | `Not_explain ->
          super#expr e
end

let () =
  AstFilters.register_str_item_filter map#str_item;
  AstFilters.register_topphrase_filter map#str_item;












