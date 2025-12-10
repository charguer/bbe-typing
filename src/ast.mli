open Var

module type T = sig

(** The types on this file are all mutually recursive.
  To help on the definition, we first define these abstract types, which will be made
  equal to their corresponding definitions here in Ast_fix. *)
type typ0
type syntyp0
type env0
type trm0
type varsyntyp0
type varid_set

type instance_value = trm0

(* * Syntax *)
(** Define the AST of the considered language. *)

(** A [loc] denotes a location in the source code *)
type loc = Location.t


(*Yanni: Unnecessary*)
(** An overloaded variable, e.g. [(+)]. *)
type symbol =

  | SymbolName of var (* A simple variable. *)

  | SymbolTuple of int (* >= 0 and <> 1 as there is no syntax for unary tuple. [__Tuple0] is [()]. *)
  | SymbolNumericInt (* [42] in the source is encoded as [Trm_apps (Trm_var SymbolNumericInt) (Trm_cst (Cst_int 42))]
                        with annotation on the Trm_apps *)
  | SymbolNumericFloat
  | SymbolString
  | SymbolBool

  | SymbolGetField of field
  | SymbolSetField of field
  | SymbolMakeRecord of field list (* Invariant: ordered. *)
  | SymbolRecordWith of field


(** A [styp] (syntactic type) describes the piece of syntax that corresponds
    to a user-provided type annotation. Not to be confused with a [typ],
    which describes a type internally in the type-checker. *)
type styp = Parsetree.core_type (* FIXME: rename to parsetyp *)


(** * Types *)


(** Mode for instance arguments.
   The [mode] can be 'input' or 'output'.
   For an overloaded function, we have a mode to every argument.
   The resolution is guided by the type of the 'input' arguments only. *)
(*Yanni: Unnecessary*)
type mode =
  | Mode_in (* true: input parameter (default) *)
  | Mode_out (* false: output parameter (mainly used for iterators) *)

(** An [symbol_modes] gives the modes associated with an overloaded function.
  The list of modes corresponds to the inputs, and the second mode to the context-unification
  of the output. *)
(* LATER: change this to a [(mode list * mode) list], with a list of modes for every possible arity? *)
type symbol_modes = (mode list * mode) option

(* Every [typ] may carry a mark, used internally for detecting cycles in [get_repr]. *)
type mark = int

(* Another kind of type variables are the 'rigid' type variable, to capture polymorphic ['a],
  whose structure cannot be refined.  We implement them as a local abstract type constructor,
  whose name is ['a].  Note that the quote is part of its name. *)
type tvar_rigid = tconstr

(** A [typ_desc] describes the possible structure of a type. A type may be:
    - a 'flexible' variable, that is a variable that can unify with anything else.
    - a 'unified' variable, which was a flexible variable but has since then been
      unified with another type variable (Unified is like a link in the Union-Find data structure:
      the [typ] pointed by Unified contains mutable fields).
    - a 'structured' type, that is, the application of a type constructor, e.g. [int list].
    - internal types can furthermore be a 'not-yet-resolved-instance-of-an-overloaded-symbol'
      (see [internal_type] below).
    - a 'rigid' variable (i.e. polymorphic type) is also represented as a constr. *)
type typ_desc =
  | Flexible of tvar
  | Unified of typ
  | Typ_constr of tconstr * typs

(** A [typ] describes a type object in the unification process performed by the typechecker.
    A [typ] has a _mutable_ description, of type [typ_desc].
    This description should only be modified by means of [History.make_modif_desc],
    for the rollback mechanism to work properly.

    To check whether two types are equal, first we follow the [Unified] links,
    to reach the roots, like in a union-find. Then, we may compare the two roots:
    - if the two roots are physically equal, the types are the same
    - else the types might have unifiable structures (e.g. ['a list] and [int list]) *)
and typ = {
  mutable typ_desc : typ_desc (* to be modified only via [History.make_modif_desc] *) ;
  mutable typ_mark : mark (* for internal use by the [get_repr] function *)
}

(** [typs] is a shorthand for a list of [typ] *)
and typs = typ list

(** A [sch] denotes a ML type scheme, that is, a type of the form [forall 'a 'b. t],
   with head-polymorphism (forall), and a type body without further quantifiers. *)
(*Yanni: Probably unnecessary*)
and sch = {
  sch_tvars : tvar_rigid list;
  sch_body : typ;
}

(** A varid records information on how a variable is resolved,
    in particular in the case it is an overloaded symbol. *)
and varid = {
  varid_unique_int : varid_unique_int; (* Used for storing varids in sets *)
  varid_var : var; (* Needed to print assumption instantiations in the output AST. *)
  varid_loc : loc ;
  mutable varid_env : env0 ; (* Needed to resolve the assumptions that may be associated with instances *)
  mutable varid_resolution : varid_resolution; (* Describes how the symbol is resolved *)
  varid_depth : int; (* The dependency depth at which this symbol has been created. *)
  varid_typ : typ; (* Type of the overloaded symbol, as seen by its context *)
  varid_context : symbol option; (* To ease bug reporting: when this varid is an instance assumption, it carries its parent symbol. *)

  (* The following booleans are markers, for internal use only (see the StackUnique submodule
    in Typecheck. *)
  mutable varid_marker_strong : bool;
  mutable varid_marker_weak : bool
}

(** [varid_resolution] describes the resolution status of a [varid].
    It case a symbol is resolved to an instance, the assumptions of
    this instance might not yet be fully resolved. *)
and varid_resolution =
  | VarUnknown (* Status is unknown after parsing. *)
  | VarRegular (* Variable bound by a let or a lambda-expression. *)
               (* LATER of loc (* Location of the binding point *) *)

(** [assumptions] describes a list of varids that corresponds to the
    assumptions of a instance. E.g. addition on type ['a matrix] has
    for assumption addition on type ['a]. *)
and assumptions = varid list

(** [candidates] describes a list of instances to which a symbol could resolve.
    [candidates] is used as synoynmous for [instance list] in environments
    and in [VarUnresolved] for clarity. *)
and candidates = instance list

(** A [candidates_and_modes] record denotes the list of instances registered
   with a given overloaded symbol.
   It also records the input-output modes that should be used when resolving
   that overloaded symbol. *)
and candidates_and_modes = {
  candidates_and_modes_candidates : candidates;
  candidates_and_modes_modes : symbol_modes; (* LATER: remove modes? *)
}

(** [instance_sig] describes the type scheme and the assumptions of an instance.
   Example for addition on [a matrix]:
   - tvars : the list [a]
   - assumptions : a list of assumption_desc [add[@instance (+)] : a -> a -> a]
   - typ : the type of the matrix addition [a matrix -> a matrix -> a matrix]. *)
and instance_sig = {
  instance_tvars : tvar_rigid list;
  instance_assumptions : assumption_desc list;
  instance_typ : typ;
}

(** An assumption can take two forms as part of the arguments of an instance declaration:
  - [(op[@implicit (+)] : a -> a -> a)].  In this case, the [op] is irrelevant for the typing context (it will be seen in the instance value).
  - [(_[@implicit (+)] : a -> a -> a)]. *)
(*Yanni: Unnecessary*)
and assumption_desc = {
  assumption_symbol : symbol;
  assumption_typ : syntyp0;
}

(** An [instance] denotes a possibility of resolution for an overloaded symbol,
    e.g. [let[@instance (+)] int_add : int -> int -> int = ..]
    or [let[@instance (+)] _ = int_add].
    Each instance has a name (e.g. [int_add]), not to be confused with its
    symbol (e.g. [+]).
    Each instance has a signature, e.g. [int -> int -> int] for a simple
    addition on [int], or a more complex signature, see the example
    given for matrix in the definition of [instance_sig].

    Instance arguments can have some special arguments, for instance:
    [[
      let[@instance (+)] matrix_add (type a) (op[@implicit_instance (+)] : a -> a -> a)
        (m1 m2 : matrix a) : matrix a = ...
    ]]
    Here is what these attributes on instance arguments mean:
    - [op[@instance (+)]] means that [op] is a normal argument from the outside point of vue.
      Inside the body of the function, however, it behaves as a local instance.
      It is equivalent to taking [op] as a normal argument, then declaring
      [let[@register (+)] _ = op] in the beginning of the function.
    - [op[@implicit (+)] means that from the outside point of vue, callers won't provide
      this argument [op] when calling the instance.
      It is a bit like the [`{ typeclass }] notation in Rocq for arguments.
      Formally, this means that [op] is an [assumption_desc] for the instance.
      Within the instance body, however, [op] behaves like a normal argument.
    - [op[@implicit_instance (+)] is syntactic sugar for [op[@implicit (+)][@instance (+)]].
    Syntactially, we use the convention that within instance declarations, type arguments
    come first, then implicit arguments, then normal arguments.
*)
(*Yanni: useful for overloading, probably unnecessary for us*)
and instance = {
  (** Term to be extracted at the place of the instance use.
    It takes the instance's arguments as parameters. *)
  instance_value : instance_value;
  instance_sig : instance_sig;
  instance_loc : loc; (* Where the instance was declared. *)
  instance_symbol : symbol
}


(* ** Syntactic types *)

(** A [syntyp] is used to carry in the AST information about a
    user-provided type annotation. It carries not only the syntactic
    type provided by the user, but also a type variable that represents
    that type from the perspective of the typechecker. This internal
    type consists of a fresh flexible type after parsing, and of a proper
    internal type after typechecking. *)
type syntyp = {
  syntyp_syntax : styp;
  syntyp_typ : typ; (* a fresh flexible after parsing,
                       possibly a fresh flexible after typing,
                       e.g. in case of an argument without explicit type *)
}

(** A [synsch] is similar to a [syntyp] for a type scheme, with
    a list of quantified variables. Variables in the syntactic
    list and in the [sch] object must appear in the same order. *)
(*Yanni: probably unnecessary*)
and synsch = {
  synsch_syntax : tvar_rigid list * styp;
  synsch_sch : sch; (* possibly dummy after parsing *)
}


(** A [varsyntyp] describes e.g. [x: a list] to represent a
   typed argument name. A [varsyntps] represent a list of
   typed arguments. *)
and varsyntyp = var * syntyp
and varsyntyps = varsyntyp list

(** A [varsynschopt] describes e.g. [x: (type a. a list)] to
   represent a type-annotated let-bound variable, whose
   type is possibly polymorphic. *)
type varsynschopt = var * synsch option


(** * Terms *)

(** The different kinds of let-bindings:
  - normal let-bindings (including sequences).
  - let[@register <sig>]. This command isn't really directly available to the user.
    It takes as an argument an instance signature.
  - let[@instance (+) foo = ...] is compiled by Ocaml_to_ast into:
    [[
      let foo = ...
      let[@register (+)] _ = foo
    ]]
    A more complex example: the line
    [[
      let[@instance (+)] foo (type a) (bar[@implicit (-)] : a -> a) : a -> a = ... in t'
    ]]
    is compiled by Ocaml_to_ast into
    [[
      let foo bar = ... in
      let[@register ((+) : type a. ((-) : a -> a) -> (a -> a) __result)] _ = foo in
      t'
    ]]
  Syntactially, we take the convention that within instance declarations, type arguments
  come first, then implicit arguments, then normal arguments.

  Remark: the two lines below are equivalent.
  [[
    let[@instance (+)] x = t in t'
    (fun x[@instance (+)] -> t') t
  ]]
 *)
type bind =
  | Bind_anon                                       (* From sequences and [let _ =]. *)
  | Bind_var of varsynschopt                        (* [let x : (type a. a -> a) = ...] *)
  | Bind_register_instance of symbol * instance_sig (* [let[@register (+)] _ (type a) (op[@implicit (+)]) = ...] *)

type cst =
  | Cst_bool of bool
  | Cst_int of int
  | Cst_float of float
  | Cst_string of string
  | Cst_unit of unit

type pat = {
  pat_desc : pat_desc;
  pat_loc : loc;
  pat_typ : typ;
}

and pat_desc =
  | Pat_any                         (* _ *)
  | Pat_var of var                  (* x *)
  | Pat_alias of pat * var          (* _ as x *)
  | Pat_constant of cst             (* 42. Note that it then means the base 42 value, not one with an implicit class. *) (*TODO: rename to Pat_cst*)
  | Pat_tuple of pats               (* (_, _) *)
  | Pat_construct of constr * pats  (* Some _ *)
  | Pat_constraint of pat * syntyp  (* (_ : sty) *)
  | Pat_or of pat * pat             (* _ | _ *)

and pats = pat list

type rec_flag = Asttypes.rec_flag

(** Annotations to recognise special terms generated by the parser that the printer should know about. => LATER: rename to decoration *)
type annot =
  | AnnotNone

  (* The term was a literal that has been encoded as [__class literal].
    Note that this annotation comes in the function call, not in the literal. *)
  | AnnotLiteralUnit
  | AnnotLiteralBool
  | AnnotLiteralInt
  | AnnotLiteralFloat
  | AnnotLiteralString

  (* The term was a record operation encoded as a function call. *)
  | AnnotRecordGet
  | AnnotRecordSet
  | AnnotRecordMake
  | AnnotRecordWith

  | AnnotTuple of int

type let_def = {
  let_def_rec : rec_flag;
  let_def_bind : bind;
  let_def_body : trm0;
}

(* The representation of [42] in a parsed AST is
  [Trm_apps (Trm_var (SymbolNumericInt, ...), [Trm_cst (Cst_int 42)]]. *) (*Yanni: Unnecessary now, a normal cst is enough.*)
(* Records are compiled as stated in the article. *)
type trm_desc =
  | Trm_var of varid
  | Trm_cst of cst
  | Trm_funs of varsyntyps * trm          (* fun (xn : tyn) -> trm_body *)
  | Trm_if of trm * trm * trm             (* if t1 then t2 else t3 *)
  | Trm_let of let_def * trm              (* [t1 ; t2], [let rec x = t1 in t2], [let[@register (+) _ = t1 in t2]] *)
  | Trm_apps of trm * trms                (* Application. Partial application is not allowed. *)
  | Trm_annot of trm * syntyp             (* (t : ty) *)
  | Trm_forall of tvar_rigid * trm        (* fun (type a) -> t *)
  | Trm_match of trm * (pat * trm) list   (* match t with p1 -> t1 | ... | pn -> tn *)
  (*BBE constructions*)
  | Trm_bbeis of trm * trm_pat
  (*Pattern constructions*)
  | Trm_patvar of varid
  | Trm_patwild

and trm = {
  trm_desc : trm_desc;
  trm_loc : loc;
  trm_typ : typ; (* a fresh flexible after parsing *)
  (* Should we add a trm_binds attribute? Like : if it was a bbe, then it would bind ...
    And then for patterns, the type would be the input, and "binds" would be the output... *)
  trm_binds : env0 option; (* An option used both to easily get result bindings, and notify if the term actually has result bindings *)
  trm_env : env0; (* a dummy environment after parsing *)
  trm_annot : annot (* to help printing back of encoded terms *)
}

and trms = trm list

and bbe = trm
and trm_pat = trm (*temporary solution, hoping to remove the "pat" type and change "pattern" to pat later on.*)

(* Definition of a type. *)
type tconstr_def =
  | Tconstr_special_nary
    (* Special n-ary operators like "->" and "*". *)
  | Tconstr_abstract
    (* Example: [type t] without definition, or a rigid variable. *)
  | Tconstr_def_alias of typ
    (* Example: [type 'a t = 'a * 'a]. In this case, 'a is a rigid type variable. *)
  | Tconstr_def_sum of (constr * typ) list
    (* Example: [type t = A | B of int].
      Its type may contain rigid variables from its carrying type.
      The type associated to a conctructor is its type: usually an arrow type whose result
      is the overall type. *)
    (* ==> TODO: rename to [Tconstr_def_algebraic]  *)
  | Tconstr_record of (field * typ) list
    (* Example: [type t = { field : int }].
      The type associated to each field is the return type of its projection (i.e. [int],
      and not [t -> int]).
      It may contains rigid variables from the record type.
      The list of fields is supposed to be sorted with the [compare] function. *)

(** A [tconstr_desc] describes the definition of type constructor, e.g. [list]. It carries:
    - the list of the polymorphic type variables, e.g. [a] in [a list]; these variables must be 'rigid'
    - if the type is constant, e.g. [int], it stores the internal type
      object ([typ]) that represents this constant type (for optimization purpose)
    - the definition of the body of the type, e.g. if it is an alias or an algebraic datatype, or a record type
     *)
type tconstr_desc = {
  (* TODO?: include the name e.g. [list] here? *)
  (* TODO?: include the repr of the type [a list] here? *)
  tconstr_tvars : tvar_rigid list;
  tconstr_def : tconstr_def;
  tconstr_typ : typ option;
    (** When [tconstr_tvars] is empty (and that [tconstr_def] is not [Tconstr_special_nary]),
      then all instances of this type descriptor are the same (as there are no parameter).
      In such cases [tconstr_typ] is the unique type represented by this type descriptor.
      We could have generated new type variable instances each time we reach one, but as an
      optimisation, we store the one type here. *)
  (* LATER: tconstr_loc for error messages *)
}

(* Top-level type definition *)
type typ_def = {
  typ_def_rec : rec_flag;
  typ_def_td : Parsetree.type_declaration list;  (* FIXME: syntypdef *)
  typ_def_typs : tconstr_desc list (* The list of type constructors defined within this type declarationm.  Dummy after parsing *) (* FIXME: typedef *)
}

(* Top-level external declaration *)
type external_def = {
  external_def_var : var;
  external_def_syntyp : synsch; (* TODO: Rename into external_def_synsch. *)
  external_def_def : string list;
}

type topdef_desc =
  | Topdef_val_def of let_def
  | Topdef_typ_def of typ_def
  | Topdef_external of external_def

(* Some definitions marked with [@type_error "message"] are expected to fail with this
  exact error message. *)
type expected_error = string option

and topdef = {
  topdef_desc : topdef_desc;
  topdef_loc : loc;
  topdef_expected_error : expected_error
}

and topdefs = topdef list
and program = topdefs


(** * Typing Environment *)

(** An [env_item] describes an entry in the typing environment.
    An entry may be one of:
    - a binding that associates a type scheme with a non-overloaded variable name
    - a binding that associates a set of instances ([candidates_and_modes]) with an
      overloaded variable name.

    Note that the following forms of shadowing are supported, in particular:
    - an overloaded symbol may be rebound locally as a local non-overloaded name,
    - a non-overloaded name may be shadowed by the declaration of an overloaded name,
    - a non-overloaded name may be shadowed by another non-overloaded name,
    - an overloaded symbol may be shadowed by another overloaded symbol (but they are
      then considered different). *)
(* type env_item =
  | Env_item_var of sch (* regular variable (not overloaded)  *)
  | Env_item_overload of candidates_and_modes (* overloaded *)
 *)
(** An [env_var] is a typing environment for resolving program variables
  typically defined by a let-binding): it associates an [env_item] to every
  variable name. Technically, the keys are symbols, due to our encodings
  (see definition of type [symbol]). *)
type env_var = (var, sch) Env.t (* LATER: rename to env_symbol? *)

(** An [env_tconstr] is a typing environment for type constructors (e.g. [list]):
   it associates a type constructor descriptor ([tconstr_desc])
   to every type constructor name (e.g., [list], [array]) *)
type env_tconstr = (tconstr, tconstr_desc) Env.t

(** An [env] is the combined structure that provides a typing environment
    for program variables, type variables (which are represented as
    constructors of arity zero), and type constructors. *)
type env = {
  env_var : env_var;
  env_tconstr : env_tconstr;
  (* env_is_in_pattern : bool; *) (* AC *)
}

end

