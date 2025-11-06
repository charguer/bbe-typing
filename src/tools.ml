

let rec list_mem_assoc_val (m : 'b) (l : ('a * 'b) list) : bool =
  match l with
  | [] -> false
  | (x,h)::_ when h = m -> true
  | _::q -> list_mem_assoc_val m q


(* deprecated
let rec list_debut n l =
  if n = 0 then
    [],l
  else
    match l with
    | [] -> [],[]
    | h::q ->
        let ts,lr = list_debut (n-1) q in
        h :: ts, lr
*)

(* [unlast l] returns [(l',x)] such that [l = l'@[x]].
    NOTE: fails on empty lists. *)
let unlast (l : 'a list) : 'a list * 'a =
  match List.rev l with
  | [] -> failwith "Xlist.unlast: the input list should not be empty."
  | x::l' -> (List.rev l', x)

let rec list_iter3 f l1 l2 l3 = match l1, l2, l3 with
  | [], [], [] -> ()
  | (a1 :: l1), (a2 :: l2), (a3 :: l3) ->
      f a1 a2 a3;
      list_iter3 f l1 l2 l3
  | _, _, _ -> invalid_arg "iter3"

let rec list_map3 f l1 l2 l3 = match l1, l2, l3 with
  | [], [], [] -> []
  | (a1 :: l1), (a2 :: l2), (a3 :: l3) ->
      (f a1 a2 a3) :: (list_map3 f l1 l2 l3)
  | _, _, _ -> invalid_arg "iter3"

open PPrint
type doc = document

(* [doc_to_string d]: converts a document into a string. *)
let doc_to_string ?(width:PPrint.requirement=80) (d : document) : string =
  let b = Buffer.create 15 in (* the vast majority of string representation have less than 15 chars *)
  ToBuffer.pretty 0.9 width b d;
  Buffer.contents b

let doc_to_out ppf ?(newline:bool=false) d =
  Printf.fprintf ppf "%s%s" (doc_to_string d) (if newline then "\n" else "")

let doc_to_stdout ?(newline:bool=false) d =
  Printf.printf "%s%s" (doc_to_string d) (if newline then "\n" else "")

(* [with_timing f] executes [f] and returns both the execution time and the result *)
let with_timing (f : unit -> 'a) : float * 'a =
  let t_before = Sys.time () in
  let res = f () in
  let t_after = Sys.time () in
  ((t_after -. t_before), res)
