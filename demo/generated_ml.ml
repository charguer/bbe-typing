exception Exn_Next of string 
exception Exn_Exit of string * float 
type 'a func = 'a
let (simple_if_next_2 : _) =
  let _x2 = 2 in
  let x = _x2 in try raise (Exn_Next "L") with | Exn_Next "L" -> 3