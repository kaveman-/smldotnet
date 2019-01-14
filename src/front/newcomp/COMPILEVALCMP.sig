(*======================================================================*)
(* Compile a value term or non-continuation computation term.		*)
(*======================================================================*)
signature COMPILEVALCMP =
sig

(*----------------------------------------------------------------------*)
(* Compile a value term to produce:					*)
(*   its type ty							*)
(*   its target representation (= tyToRep env ty)			*)
(*   instructions that put the value on the top of the stack 		*)
(*   the number of stack items required to produce the value		*)
(*----------------------------------------------------------------------*)
val compileVal : 
  CompileEnv.Env ->  
  MILTerm.Val -> 
  MILTy.Type * VMTy.Type * RTInstrs.Instrs * int 

(*----------------------------------------------------------------------*)
(* Compile a list of value terms to produce:				*)
(*   their types							*)
(*   their target representations					*)
(*   instructions that put the values on the stack, with the		*)
(*     last item on the top.						*)
(*   the number of stack items required to produce the values       	*)
(*----------------------------------------------------------------------*)
val compileVals : 
  CompileEnv.Env ->  
  MILTerm.Val list -> 
  MILTy.Type list * VMTy.Type list * RTInstrs.Instrs * int

(*----------------------------------------------------------------------*)
(* Result:								*)
(*   computation type of the term					*)
(*   target representation of the result				*) 
(*   list of instructions to evaluate the term with results on stack	*)
(*   bound on stack used by instructions				*)
(*----------------------------------------------------------------------*)
val compile : 
  RTInstrs.Instrs -> (* Prepend call instruction with these (for tail annot) *)
  CompileEnv.Env ->  
  MILTerm.Cmp -> 
  MILTy.CmpType * VMTy.Type list * RTInstrs.Instrs * int

end