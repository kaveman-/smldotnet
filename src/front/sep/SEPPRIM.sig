(*======================================================================*)
(* Primitive structures (at present, just Prim)                         *)
(*======================================================================*)
signature SEPPRIM =
sig

val makePrimEntry : MILTy.Type TyName.Map.map * Env.Env -> UnitTypes.Entry

end