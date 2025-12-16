
(* TODO : abstract this into a simple list. Use AI for easier work. Do when the minimal configuration is done. basically. *)

(** Association objects, mapping objects of type 'a to objects of type 'b. *)
type ('a, 'b) t

(** Empty association. *)
val empty : ?compare:('a -> 'a -> int) -> unit -> ('a, 'b) t

(** Return the object associated to a.
  Raises Not_found if out of the domain. *)
val read : ('a, 'b) t -> 'a -> 'b

(** Return the object associated to a, or None if not in the domain. *)
val read_option : ('a, 'b) t -> 'a -> 'b option

(** Adding an object. *)
val add : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

(** Removing an object. *)
val remove : ('a, 'b) t -> 'a -> ('a, 'b) t

(** Checking if an object is in the domain. *)
val mem : ('a, 'b) t -> 'a -> bool

(** The number of elements stored in the environment. *)
val size : ('a, 'b) t -> int

(* Fold over all the mappings of the environnement. *)
val fold : ('a, 'b) t -> ('c -> 'a -> 'b -> 'c) -> 'c -> 'c

(* Print-out the whole environment, for debug purposes. *)
val print : ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string

val is_empty : ('a, 'b) t -> bool

val to_list : ('a, 'b) t -> ('a * 'b) list

val find_first_opt: ('a, 'b) t -> ('a -> bool) -> ('a * 'b) option

val exists: ('a, 'b) t -> ('a -> 'b -> bool) -> bool