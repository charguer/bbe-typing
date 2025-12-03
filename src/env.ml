
module type T = sig
  type key
  type value
  module Map : Map.S with type key = key
  val env : value Map.t
end

type ('a, 'b) t =
  (module T with type key = 'a and type value = 'b)

let empty (type a) (type b) ?(compare = Stdlib.compare) () : (a, b) t =
  (module struct
    type key = a
    type value = b
    module Map = Map.Make (struct type t = a let compare = compare end)
    let env = Map.empty
  end)

let read (type a) (type b) (module O : T with type key = a and type value = b) x =
  O.Map.find x O.env

let read_option (type a) (type b) (module O : T with type key = a and type value = b) x =
  O.Map.find_opt x O.env

let add (type a) (type b) (module O : T with type key = a and type value = b) x y =
  (module struct
    include O
    let env = O.Map.add x y O.env
  end : T with type key = a and type value = b)

let remove (type a) (type b) (module O : T with type key = a and type value = b) x =
  (module struct
    include O
    let env = O.Map.remove x O.env
  end : T with type key = a and type value = b)

let size (type a) (type b) (module O : T with type key = a and type value = b) =
  O.Map.cardinal O.env

let mem (type a) (type b) (module O : T with type key = a and type value = b) x =
  O.Map.mem x O.env

let fold (type a) (type b) (module O : T with type key = a and type value = b) f i =
  O.Map.fold (fun key value acc -> f acc key value) O.env i

let print print_key print_val t =
  String.concat " ; " (
    fold t (fun l k v ->
      Printf.sprintf "%s -> %s" (print_key k) (print_val v) :: l
    ) []
  )

(* TODO: add an intersection function *)