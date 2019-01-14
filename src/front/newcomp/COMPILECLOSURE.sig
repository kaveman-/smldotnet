(*======================================================================*)
(* Compile closure classes.                                             *)
(*======================================================================*)
signature COMPILECLOSURE =
sig

(*----------------------------------------------------------------------*)
(* Given a compilation environment, a number for the closure class,     *)
(* and a closure definition, compile to a class.			*)
(*----------------------------------------------------------------------*)
val compile :
  CompileEnv.Env ->                   
  int*ClosConvTypes.ClosDef ->
  RTResult.Class

(*----------------------------------------------------------------------*)
(* Given arrow types for each app method, create the superclass of	*)
(* all closure classes.							*)
(*----------------------------------------------------------------------*)
val makeTopFun :
  MILTy.Type list ->
  RTResult.Class

end