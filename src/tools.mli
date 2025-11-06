
(** Useful OCaml functions. *)

val list_mem_assoc_val : 'b -> ('a * 'b) list -> bool

val unlast : 'a list -> 'a list * 'a

val list_iter3 : ('a -> 'b -> 'c -> unit) -> 'a list -> 'b list -> 'c list -> unit

val list_map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

type doc = PPrint.document
val doc_to_string : ?width:PPrint.requirement -> doc -> string
val doc_to_out : out_channel -> ?newline:bool -> doc -> unit
val doc_to_stdout : ?newline:bool -> doc -> unit

val with_timing : (unit -> 'a) -> float * 'a

