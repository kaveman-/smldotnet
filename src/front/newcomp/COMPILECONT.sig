(*======================================================================*)
(* Compile a continuation computation term.				*)
(*======================================================================*)
signature COMPILECONT =
sig

(*----------------------------------------------------------------------*)
(* Compile code for a continuation computation term.	              	*)
(* Arguments:                                                           *)
(*   the compilation environment env		                       *)
(*   the term e                                                         *)
(* Results:                                                             *)
(*   the MIL type of e                                                  *)
(*   a list of instructions -- the tail of a block                      *)
(*   a bound on the stack used by the instructions			*)
(*----------------------------------------------------------------------*)
val compile : 
  CompileEnv.Env  ->  
  MILTerm.Cmp -> 
  MILTy.CmpType * RTInstrs.Instrs * int

end