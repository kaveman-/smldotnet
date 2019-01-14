(*======================================================================*)
(* ML type variables with sorts						*)
(*======================================================================*)
signature TYVAR =
sig

(*----------------------------------------------------------------------*)
(* Type variables (Section 4.1, p16 Defn)				*)
(*----------------------------------------------------------------------*)
type TyVar and Supply

datatype Sort =
  Normal of TySort.Sort
| Overloaded of TyName.Set.set

structure Set : ORD_SET where type Key.ord_key = TyVar
structure Map : ORD_MAP where type Key.ord_key = TyVar

(* From the source: either ordinary or equality *)
val explicit : Syntax.symbol -> TyVar

(* For error message purposes *)
val toString : TyVar -> string

val isExplicit : TyVar -> bool

(* What's its sort? *)
val sort      : TyVar -> Sort

val eq       : TyVar * TyVar -> bool

(* Fresh type variable supply *)
val fresh     : Sort -> Supply -> TyVar*Supply

val initial   : Supply

val pickler : TyVar Pickle.PU

(* For the purposes of partitioning *)
val toInt : TyVar -> int option

end


