(*======================================================================*)
(* Error manager: manage printing of error messages.                    *) 
(*======================================================================*)
signature ERRORMANAGER =
sig

(* Print a single error string *)
val printErrorString : string -> unit

(* Print a list of errors *)
val printErrors : SourceMap.sourcemap * Error.Error list -> unit

end (* of struct *)

