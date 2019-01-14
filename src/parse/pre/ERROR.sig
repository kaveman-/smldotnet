(*======================================================================*)
(* General error handling stuff						*)
(*======================================================================*)
signature ERROR = 
sig

type Error

val error     : Syntax.Location * string -> Error
val warning   : Syntax.Location * string -> Error
val print     : (string->unit) * SourceMap.sourcemap * Error list -> unit
(* print should only be used from Parse.sml (which packages a
   friendlier print function for each file containing the source map). *)
val isSerious : Error -> bool
val append    : Error * string -> Error

end
