(*======================================================================*)
(* Operations on ML type schemes (Section 4.5, p19 Defn)		*)
(*======================================================================*)
signature SMLSCHOPS =
sig

(*----------------------------------------------------------------------*)
(* Free type variables and type names in a type scheme			*)
(*----------------------------------------------------------------------*)
val tyvars         : SMLSch.TypeScheme -> TyVar.Set.set
val tynames        : SMLSch.TypeScheme -> TyName.Set.set

(*----------------------------------------------------------------------*)
(* Quantify zero variables to convert a type into a type scheme		*)
(*----------------------------------------------------------------------*)
val monoType 	   : SMLTy.Type -> SMLSch.TypeScheme

(*----------------------------------------------------------------------*)
(* Quantify all variables to convert a type into a type scheme		*)
(*----------------------------------------------------------------------*)
val polyType	   : SMLTy.Type -> SMLSch.TypeScheme

(*----------------------------------------------------------------------*)
(* Instantiate a type scheme to a type with fresh type variables in	*)
(* place of the bound variables.                                        *)
(*----------------------------------------------------------------------*)
val instantiate    : SMLSch.TypeScheme -> (SMLTy.Type list * SMLTy.Type)

(*----------------------------------------------------------------------*)
(* Apply a realisation							*)
(*----------------------------------------------------------------------*)
val appRealisation : 
  SMLTy.Realisation -> SMLSch.TypeScheme -> SMLSch.TypeScheme

(*----------------------------------------------------------------------*)
(* Pretty-print								*)
(*----------------------------------------------------------------------*)
val toString : SMLSch.TypeScheme -> string
val toStringWith : (TyName.TyName -> string) -> (SMLSch.TypeScheme -> string)

(*----------------------------------------------------------------------*)
(* Equality								*)
(*----------------------------------------------------------------------*)
val eq :
  SMLSch.TypeScheme * SMLSch.TypeScheme -> bool

val pickler : SMLSch.TypeScheme Pickle.PU

end

