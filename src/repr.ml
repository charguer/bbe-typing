open Ast_fix
open Ast_aux
open Errors
open Errors_aux

let check_no_cycle (ty : typ) : unit =
  (* This is a depth-first search in the type chains: the goal here is to prevent recursive types
    (for instance [('a list) as 'a]) or infinite sequences of [Unified].
    We use the marks to color each reached type:
    - [m]: this node is a predecessor of the current type, and we want to prevent that it appears.
    - [m']: we have already visited this type, and it is fine (no loops within this type).
    - any older value: we have not yet visited this type. *)
  let m = incr_mark () in
  let m' = incr_mark () in
  let rec visit ty =
    incr Counters.counter_cycle_visit ;
    if ty.typ_mark = m
      then (* failwith "Broken invariant: cycle in type"; *)
           raise (Error (Cycle_creation (ty, ty), loc_none));
    if ty.typ_mark < m
      then begin
        ty.typ_mark <- m;
        typ_iter visit ty; (* Note that [typ_iter] does nothing if [ty] is not recursive. *)
        ty.typ_mark <- m'
      end
  in
  visit ty

let get_repr (ty : typ) : typ =
  (* In addition to follow chains of [Unified], we check that there is no infinite sequences
   of [Unified]: this enables us to avoid calling [check_no_cycle] too often, which would be
   costly.*)
  let m = incr_mark () in
  let rec visit ty =
    incr Counters.counter_repr_visit ;
    if ty.typ_mark = m then
      (* failwith "Broken invariant: cycle in type" ; *)
      raise (Error (Cycle_creation (ty, ty), loc_none));
    ty.typ_mark <- m ;
    match ty.typ_desc with
    | Unified t1 ->
      let r = visit t1 in
      (* Path compression within the union-find, only for optimization purpose. *)
      History.make_modif_desc ty (Unified r) ;
      r
    | _ -> ty
    in
  visit ty

