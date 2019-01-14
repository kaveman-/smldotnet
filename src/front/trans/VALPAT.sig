(*======================================================================*)
(* Irrefutable tattern matching translation.   				*)
(*======================================================================*)
signature VALPAT =
sig

val trans : 
  {
    freshAnonVar: Syntax.Location option ->
                  (MILTerm.BoundVar * MILTerm.Val),
    freshBoundVar: (Syntax.Location option * Syntax.symbol) -> 
                       (MILTerm.BoundVar * MILTerm.Val),
    TVE : MILTy.Type TyVar.Map.map,
    TNE : MILTy.Type TyName.Map.map,
    tyvars : TyVar.TyVar list,
    pat : SMLTerm.Pat,
    var : MILTerm.BoundVar,
    smlty : SMLTy.Type
  } -> 
  {
    bindings : (MILTerm.BoundVar * MILTerm.Val) list,
    VE : TransOps.ValEnv,
    TVE : MILTy.Type TyVar.Map.map,
    tyvars : (Var.Var * MILTy.Kind) list
  }

end 
