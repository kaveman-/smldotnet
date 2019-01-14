(*======================================================================*)
(* Process exported structures & classes.				*)
(*======================================================================*)
signature PROCESSEXPORTS =
sig

(*----------------------------------------------------------------------*)
(* Argument: a list of long identifiers paired with actual class names.	*)
(* Single identifiers are interpreted as structure identifiers,         *)
(* qualified identifiers are interpreted as class type identifiers.     *)
(*									*)
(* Result: 								*)
(*  NONE if an error was reported.					*)
(*  SOME { , tynames, mainclassopt)					*)
(*   otherwise, where tynames maps type names to actual class names and *)
(*   mainclassopt is the name of a class containing a main method, if   *)
(*   one exists.							*)
(*----------------------------------------------------------------------*)
val process   : 
  (string list * string) list -> 
  ((TyName.TyName * (Longid.longid * int)) list 
   (*@TODO: string should be longid? *)
   * string option) option

end

