(*======================================================================*)
(* Type checking of MIL terms						*)
(*======================================================================*)
signature TYPECHECK =
sig

val term : MILTerm.Cmp option ref

val check : 
  {closedBlocks:bool,checkAtoms:bool,checkDistinctTyVars:bool,checkDistinctVars:bool,checkCC:bool,checkPoly:bool} -> 
  {
    kindenv : MILTy.Kind Var.Map.map, (* Map from tyvars to kinds *)
    tyenv   : MILTy.Type Var.Map.map, (* Map from vars to types *)
    funenv  : MILTy.Type Var.Map.map  (* Map from vars to global fun types *)
  } ->
  MILTerm.Cmp -> 
  MILTy.CmpType option

val checkAll : 
  {
    kindenv : MILTy.Kind Var.Map.map, (* Map from tyvars to kinds *)
    tyenv   : MILTy.Type Var.Map.map, (* Map from vars to types *)
    funenv  : MILTy.Type Var.Map.map  (* Map from vars to global fun types *)
  } ->
  MILTerm.Cmp -> 
  MILTy.CmpType option

end

