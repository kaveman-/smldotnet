(*======================================================================*)
(* Parse manager: manage a cache containing abstract syntax trees.      *)
(*======================================================================*)
signature PARSEMANAGER =
sig

(*----------------------------------------------------------------------*)
(* Result of calling parse.						*)
(*----------------------------------------------------------------------*)
datatype Result =
  NotFound            (* File does not exist *)
| Fail                (* There was a parsing error *)
| Success of Syntax.Dec * SourceMap.sourcemap (* Parsing was successful *)

(*----------------------------------------------------------------------*)
(* Return an up-to-date AST for the fileref specified. Reparse if	*)
(* the entry in the cache is out-of-date or missing. The file is 	*)
(* assumed to exist.							*)
(*----------------------------------------------------------------------*)
val parse : Entity.FileRef -> Result

(*----------------------------------------------------------------------*)
(* Kill the cache.							*)
(*----------------------------------------------------------------------*)
val reset : unit -> unit

end
