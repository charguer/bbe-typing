
(* A flag to specifically debug variables. *)
let debug = false

type field = string

type tconstr = string

type constr = string

type var =
  | VarNormal of string
  | VarNoName of int

type vars = var list

type tvar_name = (string option * tconstr, int) Either.t
type tvar = tvar_name

type varid_unique_int = int

type instance_id = {
  instance_name : string ;
  instance_loc : Location.t
}


module SSet = Set.Make (String)

(* Set of already taken names. *)
let names = ref SSet.empty


let set_new_name n =
  assert (String.length n > 0) ;
  names := SSet.add n !names ;
  n

let tconstr = set_new_name
let constr = set_new_name
let field = set_new_name
let var n = VarNormal (set_new_name n)

let tvar_name ?raw n = Either.Left (raw, tconstr n)
let no_tvar_name =
  let current = ref 0 in
  fun () -> (
    incr current ;
    Either.Right !current
  )

let tvar = tvar_name
let no_name_tvar = no_tvar_name

let no_name_var =
  let current = ref 0 in
  fun () -> (
    incr current ;
    VarNoName !current
  )

let new_varid_unique_int =
  let id = ref 0 in
  fun () -> (
    incr id ;
    !id
  )

let print_varid_unique_int = string_of_int

let merge_tvar v1 v2 =
  match v1, v2 with
  | Either.Left (o, n), Either.Right _ | Either.Right _, Either.Left (o, n) -> Either.Left (o, n)
  | Either.Right _, Either.Right _ -> v1
  | Either.Left (_o1, n1), Either.Left (_o2, n2) -> Either.Left (None, n1 ^ "_" ^ n2)


(* OCaml keywords: these names should never be generated. *)
let keywords =
  SSet.of_list [
    "and"; "as"; "asr"; "assert"; "begin"; "class"; "constraint"; "do"; "done"; "downto"; "else";
    "end"; "exception"; "external"; "false"; "for"; "fun"; "function"; "functor"; "if"; "in";
    "include"; "inherit"; "initializer"; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match";
    "method"; "mod"; "module"; "open"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "open";
    "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val"; "virtual";
    "when"; "while"; "with"
  ]

(* Generate a new acceptable name (readable, not a keyword, and new). *)
let rec new_name =
  let name_counter = ref 0 in fun () ->
  (* Some examples have a lot of variables: I prefer to generate readable names. *)
  let rec letters =
    let vowels = "aeiou" in
    let consonants = "bdfgjklmnprtvz" in
    vowels :: consonants :: letters in
  let rec name counter = function
    | [] -> assert false
    | current :: letters ->
      let len = String.length current in
      if counter < len then String.make 1 current.[counter]
      else Printf.sprintf "%s%c" (name (counter / len) letters) current.[counter mod len] in
  incr name_counter;
  let n = name !name_counter letters in
  if SSet.mem n keywords || SSet.mem n !names then new_name ()
  else n


let new_name_from_seed seed ok =
  let empty = function
    | "" | "'" -> true
    | _ -> false in
  let rec aux () =
    let n = Printf.sprintf "%s%s%s" seed (if empty seed then "" else "_") (new_name ()) in
    if ok n then n
    else aux () in
  if not (empty seed) && ok seed then seed else aux ()


let print_tconstr n = n
let print_constr n = n
let print_field n = n

let print_var = function
  | VarNormal n -> n
  | VarNoName i -> Printf.sprintf "__local_var_%i" i

let string_to_tconstr (s : string) : tconstr = s

let instance_id x loc = {
  instance_name = set_new_name (print_var x) ;
  instance_loc = loc
}


module IMap = Map.Make (struct type t = int let compare = compare end)

let unnamed = ref IMap.empty

let print_tvar_name ?(comment = "") o =
  let comment = if debug then Printf.sprintf " (* %s *)" comment else "" in
  let n =
    match o with
    | Either.Left (_o, n) -> n
    | Either.Right i ->
      let n =
        match IMap.find_opt i !unnamed with
        | Some n -> n
        | None ->
          let n = new_name () in
          let n = if debug then Printf.sprintf "%s_%i" n i else n in
          unnamed := IMap.add i n !unnamed ;
          n in
      (* There might be a clash if an anonymous variable was printed (and thus
        given a name, but later on this name was used: in such case, we add a
        number to the variable. *)
      let ok n = not (SSet.mem n !names) in
      let rec aux i =
        let n = Printf.sprintf "%s__%i" n i in
        if ok n then n else aux (1 + i) in
      if ok n then n else aux 0 in
  "'" ^ n ^ comment

let print_tvar = print_tvar_name ~comment:"flexible"

let tvar_raw = function
  | Either.Left (o, _n) -> o
  | _ -> None

let print_instance_id inst =
  let (file, line, char) = Location.get_pos_info inst.instance_loc.Location.loc_start in
  Printf.sprintf "%s@%s-%i:%i" inst.instance_name file line char

let suggest x =
  if x = "" then []
  else (
    let prefix = [ "'" ; "_" ] in
    let suffix = prefix @ List.init 10 string_of_int in
    let aux x =
      (* Just checking some common patterns. *)
      List.map (fun p -> p ^ x) prefix
      @ List.map (fun s -> x ^ s) suffix in
    let len = String.length x in
    let char_to_string = String.make 1 in
    (if List.mem (char_to_string x.[0]) prefix then aux (String.sub x 1 (len - 1)) else [])
    @ (if List.mem (char_to_string x.[len - 1]) suffix then aux (String.sub x 0 (len - 1)) else [])
    @ aux x
  )


let constr_to_var (c : constr) : var =
  var (print_constr c)

