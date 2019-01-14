(*======================================================================*)
(* Types and helper functions for SML->MIL translation			*)
(*======================================================================*)
signature TRANSOPS =
sig

(*----------------------------------------------------------------------*)
(* A value environment maps an SML source symbol to:			*)
(*   (1) a typed MIL variable   					*)
(*   (2) a list of MIL types to which the variable should be applied    *)
(*----------------------------------------------------------------------*)
type ValEnv = ((Var.Var * MILTy.Type) * MILTy.Type list) Symbol.Map.map

(*----------------------------------------------------------------------*)
(* An exception environment maps an SML dynamically generative		*)
(* exception to a variable that holds the tag				*)
(*----------------------------------------------------------------------*)
type ExEnv = Var.Var TyName.Map.map

(*----------------------------------------------------------------------*)
(* A structure environment maps an SML structure ID to a typed var 	*)
(*----------------------------------------------------------------------*)
type StrEnv = (Var.Var * MILTy.Type) Symbol.Map.map

(*----------------------------------------------------------------------*)
(* Merge two environments, the second overriding the first.		*)
(*----------------------------------------------------------------------*)
val merge : ValEnv * ValEnv -> ValEnv
val mergeSE : StrEnv * StrEnv -> StrEnv
val mergeEE : ExEnv * ExEnv -> ExEnv

val addError : Error.Error -> unit

(*----------------------------------------------------------------------*)
(* Translate type variables and extend the TV environment appropriately	*)
(*----------------------------------------------------------------------*)
val freshDebTyVars : int -> TyVar.TyVar list -> MILTy.Type TyVar.Map.map * MILTy.Kind list
val freshTyVars : (MILTy.Type TyVar.Map.map * TyVar.TyVar list) -> MILTy.Type TyVar.Map.map * (Var.Var * MILTy.Kind) list


val freshAnonVar : unit -> MILTerm.BoundVar * MILTerm.Val 
val freshBoundVar : Syntax.symbol -> MILTerm.BoundVar * MILTerm.Val 


(*----------------------------------------------------------------------*)
(* Retrieve top bound var from pat, if any.                     	*)
(*----------------------------------------------------------------------*)
val getPatVar : SMLTerm.Pat -> Syntax.symbol option

(* the freshXXXVar functions actually ignore locations... *)
(*@TODO: remove locOpt args and clean up callers *)
val mkFreshVarFns : 
    Id.id * SourceMap.sourcemap ->
    {freshAnonVar: Syntax.Location option ->
                       (MILTerm.BoundVar * MILTerm.Val),
     freshBoundVar: (Syntax.Location option * Syntax.symbol) -> 
                       (MILTerm.BoundVar * MILTerm.Val),
     letLine: Syntax.Location -> MILTerm.Cmp * MILTy.CmpType -> MILTerm.Cmp *MILTy.CmpType,
     withSource: SourceMap.sourcemap -> 
                  ('a -> 'b) -> 'a -> 'b}

val initialize : Var.Supply -> unit
val getSupply : unit -> Var.Supply
val getTyVarSupply : unit -> Var.Supply
val getErrors : unit -> Error.Error list

val throwMessage : (SourceMap.sourcemap * Entity.Ref * Syntax.Location) -> string


end


