(*======================================================================*)
(* General pretty-printing routines					*)
(* See "A Prettier Printer", Phil Wadler, The Fun of Programming	*)
(*======================================================================*)
signature PRETTY =
sig

(* The unit used for indentation of hierarchies. Default is 2. *)
val indentUnit : int ref

val indexToString : int -> string
val indexToAlpha    : int -> string
val indexToAlphaNum : int -> string
val indexToLower    : int -> string
val indexToLowerNum : int -> string

val vec : string * string * string * string * string * string
    -> ('a -> string) -> 'a list -> string
val simpleVec : string -> ('a -> string) -> 'a list -> string
val bigVec : int -> ('a -> string) -> 'a list -> string
val parens : bool -> string -> string
val newline : int -> string

end
