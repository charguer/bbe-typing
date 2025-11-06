(** The types defined in Ast are all mutually recursive, and makes use of sets that
  depends on other mutually recursive types.
  This file thus makes the module recurive definition that makes all these definitions hold. *)

module rec Ast_Fix : sig
    include Ast.T
      with type typ0 = Ast_Fix.typ
      with type syntyp0 = Ast_Fix.syntyp
      with type env0 = Ast_Fix.env
      with type trm0 = Ast_Fix.trm
      with type varsyntyp0 = Ast_Fix.varsyntyp
      with type varid_set = VaridSet.t
  end = Ast_Fix (* This only works because Ast_Fix only contains type definitions. *)
and VaridSet : Set.S with type elt = Ast_Fix.varid =
  Set.Make (struct
    type t = Ast_Fix.varid
    let compare v1 v2 =
      compare v1.Ast_Fix.varid_unique_int v2.Ast_Fix.varid_unique_int
  end)

include Ast_Fix

