(*======================================================================*)
(* Translation into MIL							*)
(*======================================================================*)
signature TRANS =
sig

(*----------------------------------------------------------------------*)
(* Translate an SML typed term to a MIL term.				*)
(*----------------------------------------------------------------------*)
val trans : 
  {
    SE          : TransOps.StrEnv,              (* structure ID map *)
    supply      : Var.Supply,                   (* initial supply *)
    entity  	: Entity.Ref,   		(* entity reference *)
    strexp      : SMLTerm.StrExp,		(* source term *)
    tynameTys   : MILTy.Type TyName.Map.map,	(* type name -> MIL ty map *)
    sourcemap   : SourceMap.sourcemap		(* required for pat errors *)
  }
  ->
  {
    term        : MILTerm.Cmp,                  (* target term *)
    cty         : MILTy.CmpType,                (* type of target term *)
    errors      : Error.Error list,             (* errors and warnings *)
    varsupply   : Var.Supply,                   (* value variable supply *)
    tyvarsupply : Var.Supply                    (* type variable supply *)
  }   

end
