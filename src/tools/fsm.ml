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

type ('m, 's) state =
  | Msg_arg of ('m -> 's)
  | Unit_arg of (unit -> 's)
and  ('m, 's) lookup = 's -> ('m,'s) state

type 's unit_transition = unit -> 's Lwt.t
type ('m,'s) msg_transition = 'm -> 's Lwt.t

let nop_trace _ = Lwt.return ()

let loop ?(trace=nop_trace) produce lookup transition =
  let rec _interprete key =
    let arg, product_type = lookup key in
    match arg with
      | Unit_arg next -> _step_unit next
      | Msg_arg next -> produce product_type >>= fun msg -> _step_msg next msg
  and _step_unit transition =
    transition () >>= fun key ->
    trace key >>= fun () ->
    _interprete key
  and _step_msg transition msg =
    transition msg >>= fun key ->
    trace key >>= fun () ->
    _interprete key
  in _step_unit transition

let expect_loop expected step_count trans_init produce lookup transition =
  let rec _step_unit prev_key step_count transition =
    if step_count = 0 then Lwt.fail (Failure "out of steps!")
    else
      begin
	transition () >>= fun key ->
	expected prev_key key >>= function
	  | Some x ->
	    (* Printf.printf "XXX finished\n"; *)
	    Lwt.return x
	  | None ->
		(* Printf.printf "XXX continuing %d\n" step_count; *)
	    let arg,product_type = lookup key in
	    match arg with
	      | Unit_arg next -> _step_unit key (step_count-1) next
	      | Msg_arg next ->
		produce product_type >>= fun msg ->
		_step_msg key (step_count-1) next msg
      end
  and _step_msg prev_key step_count transition msg =
    if step_count = 0 then Lwt.fail (Failure "out of steps!")
    else
      begin
	transition msg >>= fun key ->
	expected prev_key key >>= function
	  | Some x ->
	    (* Printf.printf "XXX finished\n"; *)
	    Lwt.return x
	  | None ->
		(* Printf.printf "XXX continuing %d\n" step_count; *)
	    let arg,product_type = lookup key in
	    match arg with
	      | Unit_arg next -> _step_unit key (step_count-1) next
	      | Msg_arg next ->
		produce product_type >>= fun msg ->
		_step_msg key (step_count-1) next msg
      end
  in _step_unit trans_init step_count transition