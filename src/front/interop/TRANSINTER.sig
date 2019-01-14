(*======================================================================*)
(* Translate interop types into SML types				*)
(*======================================================================*)
signature TRANSINTER =
sig

val classToML : Syntax.longid -> SMLTy.Type

(*----------------------------------------------------------------------*)
(* Given an external class identifier, return its info using SML types.	*)
(* This information is cached.						*)
(*----------------------------------------------------------------------*)
val lookupExtClass : Syntax.longid -> InterOpTypes.ClassDef option

(*----------------------------------------------------------------------*)
(* Given the SML-typed info about a class, return an environment	*)
(* that appropriately models its static members as value bindings.	*)
(*----------------------------------------------------------------------*)
val extClassToStruct : InterOpTypes.ClassDef -> Env.Env

end
