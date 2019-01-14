signature SIMPLIFYATOM =
sig
  val simplify : 
    SimplifyEnv.Env -> MILTerm.Val -> 
    MILTerm.Val * MILTy.Type

  val simplifyVec : 
    SimplifyEnv.Env -> MILTerm.Val list -> 
    MILTerm.Val list * MILTy.Type list

  val forallEta : Controls.Flag
end
