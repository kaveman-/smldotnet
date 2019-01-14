(*======================================================================*)
(* Pattern matching compiler.                 				*)
(*======================================================================*)
signature PAT =
sig

(*----------------------------------------------------------------------*)
(* Translate a typed fn construct.					*)
(*----------------------------------------------------------------------*)
val transFn : 
{
  sourcemap : SourceMap.sourcemap,
  entity : Entity.Ref,
  transExp : TransOps.ValEnv -> SMLTerm.Exp -> MILTerm.Cmp * MILTy.CmpType,
  transType : SMLTy.Type -> MILTy.Type,
  freshAnonVar: Syntax.Location option ->
                  (MILTerm.BoundVar * MILTerm.Val),
  freshBoundVar: (Syntax.Location option * Syntax.symbol) -> 
                       (MILTerm.BoundVar * MILTerm.Val),
  VE : TransOps.ValEnv,
  EE : TransOps.ExEnv,
  smlty : SMLTy.Type,
  match : SMLTerm.Match
}
-> MILTerm.TAbstr * MILTy.CmpType

(*----------------------------------------------------------------------*)
(* Translate a (generalising) pattern-matching let construct.           *)
(*----------------------------------------------------------------------*)
val transLetPat : 
{
  transExp : TransOps.ValEnv -> SMLTerm.Exp -> MILTerm.Cmp * MILTy.CmpType,
  transType : SMLTy.Type -> MILTy.Type, 
  freshAnonVar: Syntax.Location option ->
                  (MILTerm.BoundVar * MILTerm.Val),
  freshBoundVar: (Syntax.Location option * Syntax.symbol) -> 
                       (MILTerm.BoundVar * MILTerm.Val),
  VE : TransOps.ValEnv,
  EE : TransOps.ExEnv,
  var : MILTerm.BoundVar,
  smlty : SMLTy.Type,
  pat : SMLTerm.Pat,
  fail : MILTerm.Cmp * MILTy.CmpType,
  loc : Syntax.Location
}
-> MILTerm.Cmp * MILTy.CmpType

(*----------------------------------------------------------------------*)
(* Translate an exception handling construct.				*)
(*----------------------------------------------------------------------*)
val transHandle : 
{
  sourcemap : SourceMap.sourcemap,
  entity : Entity.Ref,
  transExp : TransOps.ValEnv -> SMLTerm.Exp -> MILTerm.Cmp * MILTy.CmpType,
  transType : SMLTy.Type -> MILTy.Type, 
  freshAnonVar: Syntax.Location option ->
                  (MILTerm.BoundVar * MILTerm.Val),
  freshBoundVar: (Syntax.Location option * Syntax.symbol) -> 
                       (MILTerm.BoundVar * MILTerm.Val),
  VE : TransOps.ValEnv,
  EE : TransOps.ExEnv,
  exp : SMLTerm.Exp,
  match : SMLTerm.Match
}
-> MILTerm.Cmp * MILTy.CmpType

end 
