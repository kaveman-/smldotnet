(*======================================================================*)
(* ML types, as per The Definition, with interop extensions.		*)
(*======================================================================*)
signature SMLTY = 
sig

(*----------------------------------------------------------------------*)
(* Source types. The precise representation is hidden. Instead...	*)
(*----------------------------------------------------------------------*)
type Type

(*----------------------------------------------------------------------*)
(* ...use proj to normalise and obtain the following datatype.		*)
(*    Var r								*)
(*      type variable, possibly substituted for 			*)
(*    Con(c, tys)							*)
(*      type constructor parameterised on tys				*)
(*    Rec r								*)
(*      possibly open record type 					*)
(*    Fun(ty1, ty2)							*)
(*      function type							*)
(*    Ref ty								*)
(*      reference type (special: always admits equality)         	*)
(*    Array ty                                                          *)
(*      array type (special: always admits equality)                    *)
(*    Class c								*)
(*      internal class types                          			*)
(*----------------------------------------------------------------------*)
(* SL: withtype isn't legal in signatures
We don't need it for method anyway, but inline Row.
*)

(*----------------------------------------------------------------------*)
(* Class method: name, modifiers, argument types and (optional) result  *)
(* type.                                                                *)
(*----------------------------------------------------------------------*)
type Method =
{
  flags : Symbol.Set.set,
  name  : Syntax.symbol,
  ty    : Type
}

datatype TypeC = 
  Var of TyVarOrType ref
| Con of TyName.TyName * Type list
(* | Rec of Row ref *)
| Rec of (Type Symbol.Map.map * RowVarOrRow ref option) ref
| Fun of Type * Type
| Ref of (Type * Type)
| Array of Type
| Class of ClassType

(*----------------------------------------------------------------------*)
(* Type variable indirection						*)
(*----------------------------------------------------------------------*)
and TyVarOrType =
  TyVar of TyVar.TyVar
| Type of Type

(*----------------------------------------------------------------------*)
(* Internal class types							*)
(*----------------------------------------------------------------------*)
and ClassType =
  MLClass of
  {
    tyname : TyName.TyName,	(* Stamp *)
    flags : Symbol.Set.set,	(* Qualifiers *)
    super : Type,		(* Superclass *)
    interfaces : Type list,     (* Interfaces *)
    initargty : Type option,	(* Type of argument to single constructor *)
    methods : Method list	(* Methods *)
  }

(*----------------------------------------------------------------------*)
(* Row auxiliary type							*)
(*----------------------------------------------------------------------*)
and RowVarOrRow = 
  RowVar of TyVar.TyVar 
  (* really a row variable: only sensible sorts are Any and Eq *)
(* Row of Row *)
| Row of (Type Symbol.Map.map * RowVarOrRow ref option)

(*----------------------------------------------------------------------*)
(* Class method: name, modifiers, argument types and (optional) result  *)
(* type.                                                                *)
(*----------------------------------------------------------------------*)
(*
withtype Method = 
{
  flags : Symbol.Set.set,
  name  : Syntax.symbol,
  ty    : Type
}
*)

(*----------------------------------------------------------------------*)
(* A record row								*)
(*----------------------------------------------------------------------*)
(*
and Row = Type Symbol.Map.map * RowVarOrRow ref option
*)
type Row = Type Symbol.Map.map * RowVarOrRow ref option

(*----------------------------------------------------------------------*)
(* Datatype environments: a list of lists (strongly-connected 		*)
(* components) each consisting of					*)
(*   the type variable arguments to the type constructor		*)
(*   the stamp associated with the type constructor			*)
(*   the constructor environment: types (or NONE) for each constructor	*)
(*   or an empty map if this is a replicated datatype                   *)
(*----------------------------------------------------------------------*)
type DatDef = TyVar.TyVar list * TyName.TyName * Type option Symbol.Map.map
type DatEnv = DatDef list list

(*----------------------------------------------------------------------*)
(* A realisation is the type-name version of `substitution'.		*)
(* A renaming is a special case.                                        *)   
(*----------------------------------------------------------------------*)
type TypeFcn = TyVar.TyVar list * Type
type Realisation = TypeFcn TyName.Map.map

(*----------------------------------------------------------------------*)
(* Various type constructor functions					*)
(*----------------------------------------------------------------------*)
val tyVarType  : TyVar.TyVar -> Type
val funType    : Type*Type -> Type
val recType    : (Syntax.symbol*Type) list -> Type
val consType   : Type list * TyName.TyName -> Type
val baseType   : TyName.TyName -> Type
val tupleType  : Type list -> Type
val refType    : (Type*Type) -> Type
val addressType  : Type -> Type

(* ref modifiers *)
val fieldRefType  :  (Type*Type)->Type
val staticRefType  :  (Type*Type)->Type
val addressRefType  : Type
val heapRefType  : Type

val arrayType  : Type -> Type
val classType  : ClassType -> Type

(*----------------------------------------------------------------------*)
(* Destructor functions: get the type of a particular field in a record	*)
(* type (and its position), and get the content type of a ref type.	*)
(*----------------------------------------------------------------------*)
val fieldType     : Type*Syntax.symbol -> Type*int*int
val fromTyVar     : Type -> TyVar.TyVar option
val fromTyVarRef  : Type -> TyVarOrType ref option
val fromFunType   : Type -> (Type * Type) option
val fromConsType  : Type -> (Type list * TyName.TyName) option
val fromRefType   : Type -> (Type * Type) option
val fromAddressType : Type -> Type option
val fromArrayType : Type -> Type option
val fromProd      : Type -> Type list option
val fromClassType : Type -> ClassType option
val fromRecType   : Type -> (Syntax.symbol*Type) list option

(*----------------------------------------------------------------------*)
(* Determine the sort of a type or list of types			*)
(* If the first argument is true, then the sorts of all type variables  *)
(* are taken to be TySort.Eq (used for datatype equality status)        *)
(*----------------------------------------------------------------------*)
val sort           : bool -> Type -> TySort.Sort
val sortList       : bool -> Type list -> TySort.Sort

(*----------------------------------------------------------------------*)
(* Free type variables and type names 					*)
(*----------------------------------------------------------------------*)
val tyvars         : Type -> TyVar.Set.set
val tynames   	   : Type -> TyName.Set.set

val occurs	   : TyVarOrType ref -> Type -> bool

(*----------------------------------------------------------------------*)
(* Apply a substitution							*)
(*----------------------------------------------------------------------*)
val appSubst 	   : (TyVar.TyVar*Type) list -> Type -> Type
val eq             : Type*Type -> bool
val compare        : Type*Type -> order
val appRealisation : Realisation -> Type -> Type
val renameType     : TyName.Renaming -> Type -> Type
 
(*----------------------------------------------------------------------*)
(* Pretty-print a type.							*)
(* A type name environment is required for pretty-printing the tynames. *)
(*----------------------------------------------------------------------*)
val tyvarsToString : TyVar.TyVar list -> string
val toString	   : Type -> string
val toStringWith   : (TyName.TyName -> string) -> (Type -> string)
val openTypeToString: Type -> string
val openTypeToStringWith : (TyName.TyName -> string) -> (Type -> string)
val DEtoString     : DatEnv -> string
val realisationToString : Realisation -> string

val transRealisation : 
  (TyVar.TyVar list * TyName.TyName * Type -> 'a) ->
  Realisation -> 'a list

val findCon: 
  TyName.TyName * Type option Symbol.Map.map * Syntax.symbol -> 
  (int * Type option) option

val pickler : Type Pickle.PU
val classTypePickler : ClassType Pickle.PU
val datDefPickler : DatDef Pickle.PU 

val inj : TypeC -> Type
val proj : Type -> TypeC

(*----------------------------------------------------------------------*)
(* Useful predicates							*)
(*----------------------------------------------------------------------*)
(* Is this an internal or external class type? *)
val isClass : Type -> bool

(* Is this a class type or array type? *)
val isReference : Type -> bool

end

