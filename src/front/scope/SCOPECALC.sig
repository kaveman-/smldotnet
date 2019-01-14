(*======================================================================*)
(* Scope analysis: determine which bindings (value, computation, and    *)
(* function) should be floated out or hoisted in.			*)
(*======================================================================*)
signature SCOPECALC =
sig

val calc :   
  (* Type environment *)
  MILTy.Type Var.Map.map ->

  (* Computation term *)
  MILTerm.Cmp -> 
  
  (* Result of analysis *)
  ScopeTypes.Info

end