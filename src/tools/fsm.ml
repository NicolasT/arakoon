(*
This file is part of Arakoon, a distributed key-value store. Copyright
(C) 2010 Incubaid BVBA

Licensees holding a valid Incubaid license may use this file in
accordance with Incubaid's Arakoon commercial license agreement. For
more information on how to enter into this agreement, please contact
Incubaid (contact details can be found on www.arakoon.org/licensing).

Alternatively, this file may be redistributed and/or modified under
the terms of the GNU Affero General Public License version 3, as
published by the Free Software Foundation. Under this license, this
file is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.

See the GNU Affero General Public License for more details.
You should have received a copy of the
GNU Affero General Public License along with this program (file "COPYING").
If not, see <http://www.gnu.org/licenses/>.
*)

open Lwt


type 'e e_execute = 'e -> unit Lwt.t

type ('s,'e ) punit_transition = unit -> 's * 'e list
type ('m,'s, 'e) pmsg_transition = 'm -> 's * 'e list
type ('s, 'e) unit_transition  = unit -> ('s * 'e list) Lwt.t
type ('m,'s, 'e) msg_transition = 'm  -> ('s * 'e list) Lwt.t


type ('m, 's, 'e) state =
  | Msg_arg  of  ('m, 's,'e) msg_transition
  | Unit_arg of  ('s, 'e) unit_transition
  | PUnit_arg of ('s, 'e) punit_transition
  | PMsg_arg of ('m,'s,'e) pmsg_transition

and  ('m, 's,'e) lookup = 's -> ('m,'s,'e) state


let do_side_effects e_execute es  = 
  Lwt_list.iter_s e_execute es

let return ?(sides=[]) x = Lwt.return (x, sides)
let pure ?(sides=[]) x = (x,sides)


let nop_trace _ = Lwt.return ()

let loop ?(trace=nop_trace) 
    (e_execute : 'e e_execute)
    produce lookup (transition: ('s,'e) punit_transition) =
  
  let rec _interprete key =
    let arg, product_type = lookup key in
    match arg with
      | Unit_arg next  -> _step_unit next
      | Msg_arg next   -> begin produce product_type >>= fun msg -> _step_msg next msg end
      | PUnit_arg next -> _step_punit next
      | PMsg_arg next  -> begin produce product_type >>= fun msg -> _step_pmsg next msg end
        
  and _step_unit transition    = transition () >>= _rest
  and _step_msg transition msg = transition msg >>= _rest
  and _step_punit transition   = let key_es = transition () in
                                 _rest key_es
  and _step_pmsg transition msg = let key_es = transition msg in
                                  _rest key_es
  and _rest (key,es) = 
    do_side_effects e_execute es >>= fun () ->
    trace key >>= fun () ->
    _interprete key
  in _step_punit transition
  
let expect_loop 
    (e_execute : 'e e_execute)
    expected step_count trans_init produce lookup (transition: ('s,'e) punit_transition) =
  let maybe_out_of_steps step_count = 
    if step_count = 0 
    then Lwt.fail (Failure "out of steps!")
    else Lwt.return ()
  in
  let rec _step_unit prev_key step_count transition =
    begin
      maybe_out_of_steps step_count >>= fun () ->
	  transition () >>= fun (key, es) ->
      do_side_effects e_execute es >>= fun () ->
	  expected prev_key key >>= function
	    | Some x -> Lwt.return x
	    | None   -> do_none key
    end
  and _step_punit prev_key step_count transition = 
    begin
      maybe_out_of_steps step_count >>= fun () ->
      let key_es = transition () in 
      _rest prev_key key_es
    end
  and _step_msg prev_key step_count transition msg =
    begin
      maybe_out_of_steps step_count >>= fun () ->
	  transition msg >>= fun key_es ->
      _rest prev_key key_es
    end
  and _step_pmsg prev_key step_count transition msg = 
    begin
      maybe_out_of_steps step_count >>= fun () ->
      let key_es = transition msg in
      _rest prev_key key_es
    end
  and _rest prev_key (key,es) = 
    do_side_effects e_execute es >>= fun () ->
    expected prev_key key >>= function
      | Some x -> Lwt.return x
      | None -> do_none key
  and do_none key = 
    let arg,product_type = lookup key in
    match arg with
      | PUnit_arg next -> _step_punit key (step_count -1) next
	  | Unit_arg next -> _step_unit key (step_count-1) next
	  | Msg_arg next ->
        begin
		  produce product_type >>= fun msg ->
		  _step_msg key (step_count-1) next msg
        end
      | PMsg_arg next -> 
        begin
          produce product_type >>= fun msg ->
          _step_pmsg key (step_count -1) next msg
        end
  in _step_punit trans_init step_count transition
