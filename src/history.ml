open Ast_fix
open Ast_aux
open Ast_print
open Printf
open Tools

(* A flag to specifically debug the modification history. *)
let debug = false


(*#########################################################################*)
(* ** Printing modifs *)

let print fmt =
  Printf.ksprintf (fun s ->
    if debug then Debug.log "=== %s" s) fmt

let print_modif (t : typ) (td : typ_desc) : unit =
  print "set %s to [%s]\n"
    (typ_to_string t)
    (typ_to_string (mktyp td))


(*#########################################################################*)
(* ** Rollback mechanism for the representation of types  *)

(** Type objects, of type [typ], have a mutable field [typ_desc]. The use
    of mutation is key for efficiency. However, for performing "unification
    attempts", as needed for "trying" several instances, we need to be able
    to rollback on the mutations performed during a failed attempt. The
    interface provided below supports such a backtracking process without
    polluting all the implementation of the typechecker.

    The "history" is saved in the form of a stack of modifications.
    Special "checkpoints" entries may be inserted in that stack.
    At any point, the code may revert all changes made since the last checkpoint.

    The higher-order functions [with_rollback f] and [with_rollback_on_error f]
    provide a high-level functional interface for evaluating a function "f"
    with possible rollback. See further.
    The history is used only during calls of [with_rollback*]. *)

type modif =
  | Modif_desc of typ * typ_desc
  | Modif_checkpoint

type history = modif Stack.t

(** [is_recording] is on only during a call to [with_rollback]
    or [with_rollback_on_error]. *)
let is_recording : bool ref = ref false

let (hist : history) = Stack.create ()

(** [save_modif_desc t td] should be called for modifying the [desc] field
    of a type, and saving the changes in the history stack. *)


(** [checkpoint()] adds a checkpoint onto the history stack. *)

let checkpoint () : unit =
  print "History checkpoint" ;
  Stack.push Modif_checkpoint hist

(* [rollback ()] should be called to undo all changes since the last checkpoint.
   The history stack is reverted to just the point just before that checkpoint
   was inserted. *)

let rec rollback () : unit =
  let m = Stack.pop hist in
  match m with
  | Modif_checkpoint -> ()
  | Modif_desc (t, td) ->
      print ">> unrolling modif updating %s " (typ_to_string t);
      t.typ_desc <- td;
      print " back to %s \n" (typ_to_string t);
      rollback ()

let make_modif_desc (t : typ) (td : typ_desc) : unit =
  if debug then print_modif t td;
  if debug then printf ">> saving modif updating %s to " (typ_to_string t);
  if !is_recording then begin
    let old_td = t.typ_desc in
    Stack.push (Modif_desc (t, old_td)) hist;
    incr Counters.counter_history;
  end;
  t.typ_desc <- td;
  if debug then printf "%s \n" (typ_to_string t)

let with_rollback (f : unit -> 'a) : 'a =
  if !is_recording then failwith "with_rollback does not support re-entrance";
  is_recording := true;
  checkpoint ();
  let r = f () in
  rollback ();
  is_recording := false;
  r

let with_rollback_on_error (f : unit -> bool) : bool =
  if !is_recording then failwith "with_rollback does not support re-entrance";
  is_recording := true;
  checkpoint ();
  let success = f() in
  if not success then rollback ();
  is_recording := false;
  success


(*#########################################################################*)

(* TEMPORARY: for debugging purpose?
   prints the modifications since the last checkpoint *)

let current_modif () : (typ * typ_desc) list =
  let cm = ref [] in
  let recent = ref true in
  let is_recent (x : modif) : unit =
    match x with
    | Modif_checkpoint -> recent := false
    | Modif_desc (ty, td) -> if !recent then cm := ((ty,td) :: !cm)
    in
  Stack.iter is_recent hist;
  List.rev !cm


