(*======================================================================*)
(* Simplification of MIL terms						*)
(*======================================================================*)
signature SIMPLIFY =
sig

(* perform lineBeta reductio 
   Special((Line {left,right,file},_,_),_,_) -[Effect.none]-> Triv []

   NB: erases debug information;
       enables more optimisations by erasing the effect of the Line computation
*)

val lineBeta : Controls.Flag


(*----------------------------------------------------------------------*)
(* Input:								*)
(*   a basis B=(DE,EE) with pervasive datatype and exception envs       *)
(*   a type environment mapping free variables to their types           *)
(*   a pair (ce,supply) in which the bound variables of ce are distinct *)
(*   and fresh variables can be generated from supply.                  *)
(* Output:                                                              *)
(*   a pair (ce,supply) with the same properties as the input.          *)
(*----------------------------------------------------------------------*)
val simplify : 
{ 
  removeEncaps : bool, 
  doComplex : bool
} 
-> Opt.Transformer

end