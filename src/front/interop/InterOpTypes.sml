(*======================================================================*)
(* Interop class definitions as used in SML type checking		*)
(*======================================================================*)
structure InterOpTypes =
struct

(*----------------------------------------------------------------------*)
(* Class field: name, modifiers, type and final static value info.      *)
(*----------------------------------------------------------------------*)
type FieldInfo = 
{
  name : Syntax.symbol,
  flags : Symbol.Set.set,
  ty : SMLTy.Type,
  value : Constants.constant option
}

(*----------------------------------------------------------------------*)
(* Class method: name, modifiers, argument types and (optional) result  *)
(* type.                                                                *)
(*----------------------------------------------------------------------*)
type MethodInfo = 
{
  name : Syntax.symbol,
  flags : Symbol.Set.set,
  argtys : SMLTy.Type list,
  resty : SMLTy.Type option
}

(*----------------------------------------------------------------------*)
(* The full class definition.						*)
(*----------------------------------------------------------------------*)
type ClassDef =
{
  longid : Syntax.longid,
  flags : Symbol.Set.set,
  super : SMLTy.Type option,
  interfaces : SMLTy.Type list,
  fields : FieldInfo list,
  methods : MethodInfo list
}

(*----------------------------------------------------------------------*)
(* The type of a method, constructor or field. The first component is   *)
(* its defining class and the second is its SML type.			*)
(*----------------------------------------------------------------------*)
type MemberType = SMLTy.Type * SMLTy.Type

end














