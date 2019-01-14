(*======================================================================*)
(* Perform a flow analysis for closure functions.			*)
(*======================================================================*)
signature FUNFLOW =
sig

  val gather : 
  MILTerm.Cmp -> 
  {
    (* The app methods to be used for function definitions *)
    defmeths : int Var.Map.map,

    (* The app methods to be used for function applications *)
    refmeths : int Var.Map.map,

    (* Dead function applications *)
    deadapps : MILTy.CmpType Var.Map.map,

    (* Function types for each app method *)
    methtys : MILTy.Type list
  }

  val normalizeKind : MILTerm.FunKind -> MILTerm.FunKind

end