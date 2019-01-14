(*======================================================================*)
(* ML type structures (Section 4.9, p20 Defn)		                *)
(* We depart slightly from the definition here: a type structure is one *)
(* of the following:                                                    *)
(*   1. An arity, tyname and constructor environment (for datatypes);   *)
(*   2. A type function (for type abbreviations and where-instantiated  *)
(*      abstract types);                                                *)
(*   3. An arity and tyname (for opaque types in signatures and         *)
(*      abstype in the core).                                           *)
(*======================================================================*)
signature TYSTR = 
sig

(*---------------------------------------------------------------------*)
(* The type for type structures and constructor/deconstructor functions.*)
(*----------------------------------------------------------------------*)
type TyStr

val makeConcrete : TyVar.TyVar list * SMLTy.Type -> TyStr
val makeAbstract : TyVar.TyVar list * TyName.TyName -> TyStr
val makeDatatype : SMLTy.DatDef -> TyStr
val makeClasstype : SMLTy.ClassType -> TyStr

val fromAbstract : TyStr -> (TyVar.TyVar list*TyName.TyName) option
val fromDatatype : TyStr -> SMLTy.DatDef option
val fromConcrete : TyStr -> (TyVar.TyVar list * SMLTy.Type) option
val fromClasstype : TyStr -> SMLTy.ClassType option

(*----------------------------------------------------------------------*)
(* Type names in a type structure					*)
(*----------------------------------------------------------------------*)
val tynames : TyStr -> TyName.Set.set

(*----------------------------------------------------------------------*)
(* If tystr = (t,VE) return t.						*)
(*----------------------------------------------------------------------*)
val tyname : TyStr -> TyName.TyName option

(*----------------------------------------------------------------------*)
(* Arity of a type structure						*)
(*----------------------------------------------------------------------*)
val arity : TyStr -> int

(*----------------------------------------------------------------------*)
(* Formal type parameters of a type structure         			*)
(*----------------------------------------------------------------------*)
val tyvars : TyStr -> TyVar.TyVar list


(*----------------------------------------------------------------------*)
(* Apply the type function associated with a type structure to some     *)
(* type parameters to produce a type.					*)
(*----------------------------------------------------------------------*)
val apply : TyStr * SMLTy.Type list -> SMLTy.Type

(*----------------------------------------------------------------------*)
(* Apply a type realisation to a type structure; this turns abstract-	*)
(* tystrs into concrete tystrs and is disallowed on datatype tystrs.    *)
(*----------------------------------------------------------------------*)
val appRealisation : SMLTy.Realisation -> TyStr -> TyStr

(*----------------------------------------------------------------------*)
(* Apply a renaming to a type structure					*)
(*----------------------------------------------------------------------*)
val rename : TyName.Renaming -> TyStr -> TyStr

val match1 :
  TyName.Set.set -> SMLTy.Realisation -> (TyStr * TyStr) -> 
  SMLTy.Realisation Result.Result

val match2 :
  (TyStr * TyStr) -> string option

(*----------------------------------------------------------------------*)
(* Pretty-print.							*)
(*----------------------------------------------------------------------*)
val toString : TyStr -> string
val toStringWith : (int * (TyName.TyName -> string)) -> (TyStr -> string)

(*----------------------------------------------------------------------*)
(* Equality								*)
(*----------------------------------------------------------------------*)
val eq :
  TyStr * TyStr -> bool

val pickler : TyStr Pickle.PU

end


