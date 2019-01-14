(*======================================================================*)
(* Check the syntactic restrictions for a declaration expression, and	*)
(* resolve                                                              *)
(*   (a) infix expressions, patterns and fun decs;                      *)
(*   (b) implicitly scoped type variables.                              *)
(*======================================================================*)
signature SYNTAXCHECK = 
sig

val check : 
  {
    (* The topdec itself, possibly containing more than one binding *)
    AST       : Syntax.Dec,           

    (* Its associated location map *)
    sourcemap : SourceMap.sourcemap   
  } 
  ->
  {
    (* The resolved topdec *)
    AST       : Syntax.Dec,       

    (* A list of errors and warnings *)
    errors    : Error.Error list      
  }  

(* Given a topdec list, find a particular entity binding. *)
val find : Syntax.Dec * Entity.Ref -> Syntax.DecItem option

end