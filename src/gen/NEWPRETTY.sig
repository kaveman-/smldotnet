signature NEWPRETTY = sig

(* Need to insert these declarations at point of use
infixr 6 ++		(* Concatenation *)
infixr 6 +/		(* Concatenation with space *or* newline *)
infixr 5 //		(* Concatenation with newline *)
*)

(* Underlying type of documents *)
type DOC

(* Constructor functions *)
val empty : DOC			(* The empty document *)
val ++    : DOC*DOC -> DOC	(* Concatenation *)
val nest  : int -> DOC -> DOC   (* Nested doc, indented *)
val line  : DOC                 (* A newline *)
val text  : string -> DOC	(* A piece of text *)

(* Combinators *)
val spread : DOC list -> DOC
val stack : DOC list -> DOC
val bracket : string -> DOC -> string -> DOC
val +/ : DOC*DOC -> DOC
val group : DOC -> DOC

val fill : DOC list -> DOC
val fillWith : string -> DOC list -> DOC

(* Style *)
val bold : DOC -> DOC
val italic : DOC -> DOC
val coloured : string -> DOC -> DOC
val xdef : string -> DOC -> DOC
val xref : string -> DOC -> DOC
val rule : DOC

val pretty : int -> (string -> unit) -> (DOC -> unit)
val prettyHtml : int -> (string -> unit) -> (DOC -> unit)

val size : DOC -> int

end