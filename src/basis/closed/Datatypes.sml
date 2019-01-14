structure Datatypes =
struct

(* datatype bool = false | true *)

datatype 'a list = nil | :: of 'a * 'a list

datatype 'a option = NONE | SOME of 'a

datatype order = LESS | EQUAL | GREATER

(*
open PrimTypes_
*)

end
