(*======================================================================*)
(* ML type schemes							*)
(*======================================================================*)
structure SMLSch =
struct

(*----------------------------------------------------------------------*)
(* The type scheme datatype						*)
(*----------------------------------------------------------------------*)
datatype TypeScheme = TypeScheme of TyVar.TyVar list * SMLTy.Type

end

