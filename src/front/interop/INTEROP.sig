(*======================================================================*)
(* Various operations on types used for inter-operability		*)
(*======================================================================*)
signature INTEROP =
sig

(* True if this is a user-defined value type *)
val isValueType    : SMLTy.Type -> bool

(* True if this is a ref of a user-defined value type *)
val isValueTypeRef   : SMLTy.Type -> bool

(* True if this is one of the pre-defined primitive interop types *)
val isPrimitive    : SMLTy.Type -> bool

(*------------------------------------------------------------------------*)
(* Is this type an (external) Enum Type and what is its underlying type?  *)
(*------------------------------------------------------------------------*)
val isEnumType     : SMLTy.Type -> SMLTy.Type option

(* True if this is an interop type:
     primitive 
     class
     class option
     T array (for interop T)
     T array option (for interop T) *)
val isInterop      : SMLTy.Type -> bool


(*----------------------------------------------------------------------*)
(* Given a predicate for exportability of internal class types, return  *)
(* an 'exportable' predicate for interop types.                         *)
(* An interop type is exportable if it is:                              *)
(*   primitive; or                             		                *)
(*   <ty> where <ty> is an exportable class type; or			*)
(*   <ty> option where <ty> is an exportable class type; or             *)
(*   <ty> array where <ty> is exportable; or				*)
(*   <ty> array option where <ty> is exportable.                        *) 
(*precondition: isInterop ty                                            *)
(*----------------------------------------------------------------------*)
val isExportable   : (TyName.TyName -> bool) -> (SMLTy.Type -> bool)

(*----------------------------------------------------------------------*)
(* Given a predicate for exportability of internal class types, return  *)
(* an 'importable' predicate for interop types.                         *)
(* An interop type is importable if it is:                              *)
(*   primitive; or                             		                *)
(*   <ty> option where <ty> is an exported class type; or               *)
(*   <ty> array option where <ty> is importable.                        *) 
(*precondition: isInterop ty                                            *)
(*----------------------------------------------------------------------*)
val isImportable   : (TyName.TyName -> bool) -> (SMLTy.Type -> bool)

(*----------------------------------------------------------------------*)
(* Given a predicate for exportability of internal class types, return  *)
(* an 'exportable' predicate for SML class types                        *)
(* returns NONE if predicate holds, o/w SOME error                      *)
(************************************************************************)
val isExportableClassType : (TyName.TyName -> bool) -> SMLTy.ClassType -> string option 

(*----------------------------------------------------------------------*)
(* What's the superclass of this class type?				*)
(*----------------------------------------------------------------------*)
val super 	   : SMLTy.Type -> SMLTy.Type option

(*----------------------------------------------------------------------*)
(* Is this type an interface type?					*)
(*----------------------------------------------------------------------*)
val isInterface	   : SMLTy.Type -> bool

(*----------------------------------------------------------------------*)
(* What are the interfaces of this class type?				*)
(*----------------------------------------------------------------------*)
val interfaces     : SMLTy.Type -> SMLTy.Type list

(*----------------------------------------------------------------------*)
(* Extends/implements relation						*)
(*----------------------------------------------------------------------*)
val sub            : SMLTy.Type * SMLTy.Type -> bool

(*----------------------------------------------------------------------*)
(* Interop reference widening and narrowing.				*)
(* Only valid for interop types.					*)
(*----------------------------------------------------------------------*)
val refWidens 	   : SMLTy.Type * SMLTy.Type -> bool
val refNarrows	   : SMLTy.Type * SMLTy.Type -> bool

(*----------------------------------------------------------------------*)
(* Method argument widening (refWidens + identity on all types)		*)
(*----------------------------------------------------------------------*)
val argWidens	   : SMLTy.Type * SMLTy.Type -> bool

(*----------------------------------------------------------------------*)
(* List types for static public fields in the specified class with the	*)
(* specified name.							*)
(*----------------------------------------------------------------------*)
val getStaticFields   : 
  SMLTy.Type * Syntax.symbol -> (SMLTy.Type*Constants.constant option) list

(*----------------------------------------------------------------------*)
(* List types for static public methods in the specified class with the	*)
(* specified name.							*)
(*----------------------------------------------------------------------*)
val getStaticMethods  : SMLTy.Type * Syntax.symbol -> SMLTy.Type list

(*----------------------------------------------------------------------*)
(* List types for non-static public fields inherited by the specified   *)
(* class type with the specified name. The types are paired with their	*)
(* defining class (necessarily external).				*)
(*----------------------------------------------------------------------*)
val getFields : SMLTy.Type * Syntax.symbol -> InterOpTypes.MemberType list

(*----------------------------------------------------------------------*)
(* List flags and types for non-static public methods                   *)
(* inherited by the specified                                           *)
(* class type with the specified name. The types are paired with their	*)
(* defining class.							*)
(*----------------------------------------------------------------------*)
val getMethods : SMLTy.Type * Syntax.symbol -> (Symbol.Set.set * InterOpTypes.MemberType) list

(*----------------------------------------------------------------------*)
(* List member types for accessible constructors in the specified 	*)
(* class type. 								*)
(*----------------------------------------------------------------------*)
val getConstructors  : (Symbol.Set.set -> bool) -> SMLTy.Type -> InterOpTypes.MemberType list

(*----------------------------------------------------------------------*)
(* Return true if first arg is a more specific method type than second. *)
(*----------------------------------------------------------------------*)
val moreSpecific : InterOpTypes.MemberType * InterOpTypes.MemberType -> bool

(*----------------------------------------------------------------------*)
(* Return true if the specified argument type can be passed to the 	*)
(* specified method type.						*)
(*----------------------------------------------------------------------*)
val argsMoreSpecific : SMLTy.Type * InterOpTypes.MemberType -> bool

(*----------------------------------------------------------------------*)
(* Given a list of method types, return the unique most specific type.  *)
(*----------------------------------------------------------------------*)
val mostSpecific : InterOpTypes.MemberType list->InterOpTypes.MemberType option

(*----------------------------------------------------------------------*)
(* Given a method function type, deconstruct to multiple args and 	*)
(* optional result.							*)
(*----------------------------------------------------------------------*)
val unpackMethTy : SMLTy.Type -> SMLTy.Type list * SMLTy.Type option

val getInheritedMethods: (SMLTy.Type * bool)-> (SMLTy.Type*InterOpTypes.MethodInfo) list

(* get flags of a class *)
val getClassFlags: SMLTy.Type -> Symbol.Set.set

val methTy : InterOpTypes.MethodInfo -> SMLTy.Type
val fieldTy : SMLTy.Type -> InterOpTypes.FieldInfo -> SMLTy.Type * Constants.constant option


(*----------------------------------------------------------------------*)
(* List types for non-static public members inherited by the specified  *)
(* class type. The types are paired with their name                     *)
(* defining class (necessarily external).				*)
(* NOTE: only intended for language service use				*)
(*----------------------------------------------------------------------*)
val getMembers : SMLTy.Type -> (Syntax.symbol * InterOpTypes.MemberType) list


end
