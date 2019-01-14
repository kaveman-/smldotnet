(*======================================================================*)
(* COM+ method, field and class definitions.				*)
(*=============a=========================================================*)
structure RTResult =
struct


type Attribute = VMTy.Type list * VMTy.Type * Word8Vector.vector
(*----------------------------------------------------------------------*)
(* A field has a name, a type and some flags. 				*)
(*----------------------------------------------------------------------*)
type Field = 
{ name : Id.id, ty : VMTy.Type, flags : Symbol.Set.set, value : int option }

(*----------------------------------------------------------------------*)
(* A method has a name, argument types, optional result type, flags	*)
(* and code (empty for abstract methods), types for locals, and max.	*)
(* stack size.								*)
(*----------------------------------------------------------------------*)
type Method = 
{ 
  name : Id.id,
  override : Id.id option, 
  attributes: Attribute list,
  args : (Id.id * VMTy.Type) list, 
  resty : VMTy.Type option,
  flags : Symbol.Set.set, 
  code : RTInstrs.Instrs,
  stack : int, 
  locals : (MILTerm.BoundVar list * VMTy.Type) list 
}

(*----------------------------------------------------------------------*)
(* A class has a name, superclass, flags, fields and methods.		*)
(* Also: a comment for debugging purposes.				*)
(*----------------------------------------------------------------------*)
type Class =
{ name : Longid.longid * int (* nesting depth >= 0 *),
  attributes: Attribute list,
  flags : Symbol.Set.set,
  methods : Method list,
  fields : Field list, 
  super : VMTy.Type,
  interfaces : VMTy.Type list }

end