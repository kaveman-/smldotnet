(*======================================================================*)
(* Gather liveness information for local variable allocation            *)
(*======================================================================*)
signature LIVENESS = sig

  val initEffectInfo : Effect.Effect Var.Map.map -> unit

  val analyse : MILTerm.Cmp -> unit

  val stackVals : Controls.Flag 
  val stackCmps : Controls.Flag

  val isStackable : Var.Var -> bool
  val findLive : Var.Var -> Var.Set.set option
  val getBlockArgsInfo : Var.Var -> Var.Var list list option
end
