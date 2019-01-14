(*======================================================================*)
(* Special compilation for 1+ types.					*)
(*======================================================================*)
signature COMPILEONEPLUS =
sig

(*----------------------------------------------------------------------*)
(* Given a type ty, return the instructions necessary to		*)
(* generate inl <> : 1+ty on the stack, and the stack that's used       *)
(*----------------------------------------------------------------------*)
val none : 
  CompileEnv.Env -> 
  MILTy.Type ->
  RTInstrs.Instrs * int

(*----------------------------------------------------------------------*)
(* Given a type ty and instructions for v, return the instructions 	*)
(* necessary to generate inr <v> : 1+ty, and the stack that's used      *)
(*----------------------------------------------------------------------*)
val some :
  CompileEnv.Env ->
  RTInstrs.Instrs * int ->
  MILTy.Type ->
  RTInstrs.Instrs * int

(*----------------------------------------------------------------------*)
(* Given a type ty and a value for inr <v>, return the instructions and *)
(* value corresponding to v that are required for the projection.       *)
(*----------------------------------------------------------------------*)
val proj :
  CompileEnv.Env ->
  MILTy.Type ->
  RTInstrs.Instrs * int

(*----------------------------------------------------------------------*)
(* Generate the instructions necessary to create all dummy none values.	*)
(*----------------------------------------------------------------------*)
val makeNones :
  unit -> 
  RTInstrs.Instrs * int

(*----------------------------------------------------------------------*)
(* List the global fields used for all dummy none values.		*)
(*----------------------------------------------------------------------*)
val makeNoneFields :
  unit ->
  RTResult.Field list

val init : unit -> unit

end