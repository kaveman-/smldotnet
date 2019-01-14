(*======================================================================*)
(* MIL types: keep everything abstract so that they can be implemented  *)
(* efficiently.                                                         *)
(*======================================================================*)
signature MILTY =
sig

(*----------------------------------------------------------------------*)
(* The abstract data type for MIL value and computation types.    	*)
(*----------------------------------------------------------------------*)
type Type 

(*----------------------------------------------------------------------*)
(* A Kind describes a type in the same way that a type describes a term *)
(* Kinds are currently the following:                                   *)
(*    Any         any value type;                                       *)
(*    Eq          any type admitting equality;                          *)
(*    Bound ty    a representative type for monomorphisation.		*)
(* Future extension: arrows on kinds.                                   *)
(*----------------------------------------------------------------------*)
datatype Kind = Any | Eq | Bound of Type

(*----------------------------------------------------------------------*)
(* Expose the outermost constructors only				*)
(*----------------------------------------------------------------------*)
datatype TypeC =     
  Var       of Var.Var                  (* Term-bound tyvar (from supply) *)
| Deb       of int                      (* Type-bound tyvar (de Brujn index)*)
| Prod      of Type list                (* Product *)
| Sum       of Type list list		(* Arity-raised sum *)
| Con       of Type list                (* Universal constructor *)
| Exn       of Exn.Exn * Type list 	(* Exception *)
| Refty     of Type list * Type         (* Mutable type with descriptor *)
| Vector    of Type                     (* Immutable vector type *)
| Array     of Type                     (* Mutable array type *)
| Arrow     of Type list * CmpType      (* Arity-raised arrow type *)
| Forall    of Kind list * Type         (* Polymorphic type *)
| Mu        of int * (TyName.TyName * Type) list          (* i'th projection from fixpoint *)
| Closure   of int option * Type list   (* Closure type *)
| App       of Type * Type list         (* Type constructor application *)
| Abs       of Kind list * Type         (* Type constructor abstraction *)
| Tyname    of TyName.TyName            (* Imported type or named datatype *)
and CmpType = CmpType of Effect.Effect * Type list

(*----------------------------------------------------------------------*)
(* init {debug=true}  uses an enum to represent datatype tags           *)
(* init {debug=false} uses int to represent datatype tags               *)
(* sumTagType() returns the tag type                                    *)
(*----------------------------------------------------------------------*)
val init : {debug:bool} -> unit
val sumTagType : unit -> Type

(*----------------------------------------------------------------------*)
(* Type constructor functions. 						*)
(* There are two varieties of type variables:                           *)
(*   (1) Those that are bound in a term (i.e. by big lambda)            *)
(*       These are just generated from a variable supply.               *)
(*   (2) Those that are bound in a type (by a quantifier, a mu, or a    *)
(*       lambda). These are de Brujn indices (0 = innermost binder).    *)
(*       In the following constructs:                                   *)
(*         abs ([K_1,...,K_n], ty)                                      *)
(*         debforall ([K_1,...,K_n], ty)                                *)
(*         mu (i, [ty_1,...,ty_n])                                      *)
(*       the first binder (K_1 or ty_1) is treated as `innermost'.      *)
(*----------------------------------------------------------------------*)
val app   : Type * Type list -> Type        (* Type constructor application *)
val abs   : Kind list * Type -> Type        (* Type constructor abstraction *)
val tyvar : Var.Var -> Type                 (* Term-bound tyvar (var supply)*)
val deb   : int -> Type                     (* Type-bound tyvar (de Brujn) *)
val tyname: TyName.TyName -> Type           (* Imported type *)
val prod  : Type list -> Type               (* Product type *)
val sum   : Type list list -> Type	    (* Flattened sum type *)
val exn   : Exn.Exn * Type list -> Type     (* (ML) Exception type *)
val arrow : Type list * CmpType -> Type     (* Multiple arg arrow type *)
val refty : Type list * Type -> Type        (* Flattened reference type with desriptor *)
val array : Type -> Type                    (* Array type *)
val vector: Type -> Type                    (* Vector type *)
val debforall : Kind list * Type -> Type    (* Polymorphic type, de Brujn *)
val forall:(Var.Var*Kind) list*Type->Type   (* Polymorphic type, named vars *)
val cmp  : Effect.Effect*Type list->CmpType (* Computation type *)
val mu    : int * (TyName.TyName * Type) list -> Type         (* (i+1)'th recursive type *)

(*----------------------------------------------------------------------*)
(* These types only appear late in compilation				*)
(*----------------------------------------------------------------------*)
val con   : Type list -> Type               (* Universal constructor *)
val closure: int option*Type list -> Type   (* Closure type *)
          
(*----------------------------------------------------------------------*)
(* noeffect tys is shorthand for cmp(Effect.none, tys)			*)
(*----------------------------------------------------------------------*)
val noeffect  : Type list -> CmpType
val anyeffect : Type list -> CmpType

(*----------------------------------------------------------------------*)
(* Type deconstructors. 		                          	*)
(*----------------------------------------------------------------------*)
val fromApp       : Type -> (Type * Type list) option
val fromProd      : Type -> Type list option
val fromSum       : Type -> Type list list option
val fromCon       : Type -> Type list option
val fromExn       : Type -> (Exn.Exn * Type list) option
val fromArrow     : Type -> (Type list * CmpType) option
val fromRefty     : Type -> (Type list * Type) option
val fromArray     : Type -> Type option
val fromVector    : Type -> Type option
val fromTyvar     : Type -> Var.Var option
val fromForall    : Type -> (Kind list * Type) option
val fromCmp       : CmpType -> Effect.Effect * Type list
val fromMu        : Type -> (int * (TyName.TyName * Type) list) option
val fromClosure   : Type -> (int option * Type list) option
val fromTyname    : Type -> TyName.TyName option
val fromDeb       : Type -> int option

(*----------------------------------------------------------------------*)
(* Type injection and projection.					*)
(*----------------------------------------------------------------------*)
val inj 	  : TypeC -> Type
val proj	  : Type -> TypeC

(*----------------------------------------------------------------------*)
(* Derived type deconstructors.						*)
(*                                                                      *)
(*   fromProdCon returns SOME tys for any type of the form              *)
(*     prod tys, con tys, exn(e,tys) or closure(i,tys)                  *)
(*   or NONE if not of this form.                                       *)
(*                                                                      *)
(*----------------------------------------------------------------------*)
val fromProdCon   : Type -> Type list option

(*----------------------------------------------------------------------*)
(* Finite maps for domain types and pairs of types.			*)
(*----------------------------------------------------------------------*)
structure Map     : ORD_MAP where type Key.ord_key = Type
structure PairMap : ORD_MAP where type Key.ord_key = Type*Type

(*----------------------------------------------------------------------*)
(* Substitute types for (term-bound) type variables.			*)
(*----------------------------------------------------------------------*)
val subst         : Type Var.Map.map -> Type -> Type

(*----------------------------------------------------------------------*)
(* Given an initial map from types to types (with no free type-bound    *)
(* tyvars), construct a memoizing function that applies this            *)
(* recursively to a type.                                               *)
(*----------------------------------------------------------------------*)
val replace       : Type Map.map -> (Type -> Type)
val replaceAndRename : Type Map.map * int -> (Type -> Type)

(*----------------------------------------------------------------------*)
(* Given a function whose domain is types, construct a memoized version	*)
(*----------------------------------------------------------------------*)
val memoize       : ((Type -> 'a) -> (Type -> 'a)) -> (Type -> 'a)
val memoizeBinary : ((Type*Type -> 'a) -> (Type*Type -> 'a)) -> (Type*Type -> 'a)

val memoizeContextual : 
  ((Kind Var.Map.map -> Type -> 'a) ->
   (Kind Var.Map.map -> Type -> 'a)) ->
  (Kind Var.Map.map -> Type -> 'a)

(*----------------------------------------------------------------------*)
(* Equality. Constant time because of hash-consing.                	*)
(*----------------------------------------------------------------------*)
val eq            : Type * Type -> bool

(*----------------------------------------------------------------------*)
(* Subtyping, induced by inclusion on effects.				*)
(*----------------------------------------------------------------------*)
val sub           : Type * Type -> bool
val subCmp        : CmpType * CmpType -> bool
val erase         : Type -> Type

(*----------------------------------------------------------------------*)
(* Substitute bounds for tyvars						*)
(*----------------------------------------------------------------------*)
val forceBounds   : Kind Var.Map.map -> Type -> Type

(*----------------------------------------------------------------------*)
(* Subclassing test (used for exceptions). This is *not* the same as    *)
(* subtyping. Returns NONE for "don't know" or "can't be bothered".     *)
(*----------------------------------------------------------------------*)
val subClass      : Type * Type -> bool option

(*----------------------------------------------------------------------*)
(* Unfold a recursive type.           					*)
(*----------------------------------------------------------------------*)
val unfold        : int * (TyName.TyName * Type) list -> Type

(*----------------------------------------------------------------------*)
(* Free (term-bound) type variables. Constant time because of memoize. 	*)
(*----------------------------------------------------------------------*)
val tyvars        : Type -> Var.Set.set		
val dtyvars	  : Type -> IntSet.set

(*----------------------------------------------------------------------*)
(* Union the effects in two computation types.				*)
(* The result types are taken from the second argument.                 *)
(*----------------------------------------------------------------------*)
val unionCmpTypes : CmpType * CmpType -> CmpType
val cmpTypePlus   : CmpType * Effect.Effect -> CmpType

(*----------------------------------------------------------------------*)
(* Pretty-printing							*)
(*----------------------------------------------------------------------*)
val toString      : Type -> string
val cmpToString   : CmpType -> string
val boundTyVarToString : Var.Var * Kind -> string

val pTy : Type -> NewPretty.DOC
val pCmpTy : CmpType -> NewPretty.DOC
val pBoundTyVar : Var.Var * Kind -> NewPretty.DOC

(*----------------------------------------------------------------------*)
(* Hash table info: each bucket with the unmasked hash codes and types. *)
(* Use only for diagnostics.                                            *)
(*----------------------------------------------------------------------*)
val stats : unit -> (word*Type) list list

end