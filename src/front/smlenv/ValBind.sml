(*======================================================================*)
(* Value bindings: value variables, value constructors, exception       *)
(* constructors.							*)
(*									*)
(* We extend The Definition with interop bindings for static fields,	*)
(* static methods, and constructors. These can be overloaded.		*)
(*======================================================================*)
structure ValBind =
struct

datatype Bind = 
  VarSch   of SMLSch.TypeScheme
| ConSch   of SMLSch.TypeScheme * SMLTy.DatDef
| ExTy     of SMLTy.Type * TyName.TyName
| Special  of SMLTy.Type * Syntax.symbol option (* if absent, constructor *)

end
