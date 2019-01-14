signature SIMPLIFYENV =
sig
  type Env

  val emptyEnv         : MILTy.Type Var.Map.map -> Env

  val envPlusFunBind   : 
    Env * MILTerm.BoundVar * 
    ((Var.Var*MILTy.Kind) list * MILTerm.FunKind * MILTerm.TAbstr) * MILTy.Type
    -> Env

  val envPlusLocalFun  : Env * MILTerm.BoundVar * MILTy.Type -> Env
  val envPlusTyVars    : Env * (Var.Var*MILTy.Kind) list -> Env
  val envPlusTySubst   : Env * (Var.Var*MILTy.Kind) list*MILTy.Type list -> Env
  val envPlusTypedVars : Env * MILTerm.TypedVar list -> Env

  val envPlusValVar    : 
    Env * MILTerm.BoundVar * MILTerm.Val * MILTy.Type -> 
    Env

  val envPlusValCmp   : 
    Env * MILTerm.TypedVar list * MILTerm.Cmp ->
    Env 

  val envPlusValVars   : 
    Env * MILTerm.BoundVar list * MILTerm.Val list * MILTy.Type list ->
    Env

  val envPlusBoundVars : Env * MILTerm.BoundVar list * MILTy.Type list -> Env

  val lookupVarType    : Env * Var.Var -> MILTy.Type 
  val lookupVarVal     : Env * Var.Var -> MILTy.Type*MILTerm.Val option 
  val isLocalFun       : Env * Var.Var -> MILTy.Type option
  val isLocalFun'      : Env * Var.Var -> bool
  val lookupBinding    : Env * MILTerm.Val -> MILTerm.Val
  val lookupCommon     : Env * MILTerm.Val -> Var.Var option
  val lookupCmpBind    : Env * MILTerm.Val -> (Var.Var * MILTerm.Cmp) option
  val lookupFunBind    : Env * MILTerm.Val -> 
    (Var.Var*MILTy.Type Var.Map.map * MILTerm.FunKind * MILTerm.TAbstr) option

  val simplifyType     : Env -> MILTy.Type -> MILTy.Type

  val commonVal : Controls.Flag
end
