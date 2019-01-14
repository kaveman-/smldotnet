(*======================================================================*)
(* Various useful operations on MIL terms				*)
(*======================================================================*)
signature MILTERMOPS =
sig

val nameAnonVars : Controls.Flag


(* Set the default symbolic info for anonymous vars,
   returning a function to restore it *)
val setAnonVarName : MILTerm.SourceInfo -> (unit -> unit)
val getAnonVarName : unit -> MILTerm.SourceInfo 


(*----------------------------------------------------------------------*)
(* Environments								*)
(*----------------------------------------------------------------------*)
type TyEnv = MILTy.Type Var.Map.map
type KindEnv = MILTy.Kind Var.Map.map

val addTypedVars : 
  TyEnv * MILTerm.TypedVar list -> TyEnv
val addBoundVars : 
  TyEnv * MILTerm.BoundVar list * MILTy.Type list -> TyEnv
val addBoundVar :
  TyEnv * MILTerm.BoundVar * MILTy.Type -> TyEnv

val addTyVars :
  KindEnv * (Var.Var * MILTy.Kind) list -> KindEnv


(*----------------------------------------------------------------------*)
(* Symbol Environments                  								*)
(*----------------------------------------------------------------------*)
type SymbolEnv = MILTerm.SourceInfo Var.Map.map

val addTypedSymbols : 
  SymbolEnv * MILTerm.TypedVar list -> SymbolEnv
val addBoundSymbols : 
  SymbolEnv * MILTerm.BoundVar list -> SymbolEnv
val addBoundSymbol :
  SymbolEnv * MILTerm.BoundVar  -> SymbolEnv



(*----------------------------------------------------------------------*)
(* Construct a type abstraction term TAbs(tyvars,ve) unless tyvars is	*)
(* empty, in which case return ve.                                      *)
(*----------------------------------------------------------------------*)
val tabs : (Var.Var * MILTy.Kind) list * MILTerm.Val -> MILTerm.Val

(*----------------------------------------------------------------------*)
(* Construct a type application term TApp(ve,tys) unless tys is    	*)
(* empty, in which case return ve.                                      *)
(*----------------------------------------------------------------------*)
val tapp : MILTerm.Val * MILTy.Type list -> MILTerm.Val

(*----------------------------------------------------------------------*)
(* MIL terms of type MILTy.bool						*)
(*----------------------------------------------------------------------*)
val trueVal : MILTerm.Val
val falseVal : MILTerm.Val
val cond : MILTerm.Val * MILTerm.Cmp * MILTerm.Cmp * MILTy.CmpType -> MILTerm.Cmp

(*----------------------------------------------------------------------*)
(* Does a particular type variable occur in a value/computation term?	*)
(*----------------------------------------------------------------------*)
val tyVarOccursVal : Var.Var -> MILTerm.Val -> bool
val tyVarOccursCmp : Var.Var -> MILTerm.Cmp -> bool

val substVal       : MILTy.Type Var.Map.map -> MILTerm.Val -> MILTerm.Val
val substCmp       : MILTy.Type Var.Map.map -> MILTerm.Cmp -> MILTerm.Cmp

(*----------------------------------------------------------------------*)
(* Does the size of a value or computation term exceed a value?		*)
(*----------------------------------------------------------------------*)
val valBigger : MILTerm.Val*int -> bool
val cmpBigger : MILTerm.Cmp*int -> bool
val absSize   : MILTerm.Cmp -> int

(*----------------------------------------------------------------------*)
(* Are two values (semantically) equal?					*)
(*   SOME true    => yes                                                *)
(*   SOME false   => no                                                 *)
(*   NONE         => don't know                                         *)
(*----------------------------------------------------------------------*)
val valEq     : MILTerm.Val * MILTerm.Val -> bool option

(*----------------------------------------------------------------------*)
(* Determine the type of a value term (no checking)			*)
(*----------------------------------------------------------------------*)
val typeOfVal :
  MILTy.Type Var.Map.map -> MILTerm.Val -> MILTy.Type


(* Derive a symbol, if any, for the projection from another symbol *)
val symbolOfProj : MILTerm.SourceInfo * int -> MILTerm.SourceInfo

(*----------------------------------------------------------------------*)
(* Determine the symbol of a value term (no checking)			        *)
(*----------------------------------------------------------------------*)
val symbolOfVal :
  SymbolEnv -> MILTerm.Val -> MILTerm.SourceInfo 
  
val isLocal : MILTerm.FunKind -> bool

val dummyBoundVar : MILTerm.BoundVar

end