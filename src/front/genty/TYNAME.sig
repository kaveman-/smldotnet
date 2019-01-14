(*======================================================================*)
(* ML type names (Section 4.1, p16 Defn)				*)
(* The identity of an ML type name consists of:				*)
(*   - the entity from which they were generated			*)
(*   - an integer unique within the tynames generated from that entity  *)
(* Also associated with a type name are:				*)
(*   - a longid used for pretty printing				*)
(*   - its equality status						*)
(*									*)
(* Type names are extended to represent internal and external classes.	*)
(*======================================================================*)
signature TYNAME = 
sig

(* Abstract types including name supply for stamps *)
type TyName and Supply

(*----------------------------------------------------------------------*)
(* Three kinds of equality status determine whether an application of 	*)
(* the type name admits equality:					*)
(*   NotEq => never admits equality 					*)
(*            (e.g. real, datatype 'a susp = C of unit -> 'a)		*)
(*   RefEq => always admits equality ('a ref and 'a array)		*)
(*   Eq => admits equality if its type arguments do			*)
(*----------------------------------------------------------------------*)
datatype EqStatus = NotEq | RefEq | Eq

(*----------------------------------------------------------------------*)
(* Sets of type names (Section 4.2, p17 Defn)				*)
(* and finite maps whose domains are type names.                        *)
(*----------------------------------------------------------------------*)
structure Set : ORD_SET where type Key.ord_key = TyName 
structure Map : ORD_MAP where type Key.ord_key = TyName

(*----------------------------------------------------------------------*)
(* Renaming of type names         					*)
(*----------------------------------------------------------------------*)
type Renaming = TyName Map.map
val rename        : Renaming -> TyName -> TyName

(*----------------------------------------------------------------------*)
(* Equivalence, hashing and pickling 	   		                *)
(*----------------------------------------------------------------------*)
val eq            : TyName * TyName -> bool
val hash          : TyName -> word
val pickler       : TyName Pickle.PU
val mapPickler    : 'a Pickle.PU -> 'a Map.map Pickle.PU

(*----------------------------------------------------------------------*)
(* Return the equality status of a type name				*)
(*----------------------------------------------------------------------*)
val equality      : TyName -> EqStatus

(*----------------------------------------------------------------------*)
(* Creation of new ML type names					*)
(*----------------------------------------------------------------------*)
(* The initial supply for a particular module *)
val initial       : Entity.Ref -> Supply

(* Generate a fresh name *)
val fresh         : Longid.longid*EqStatus -> Supply -> TyName*Supply
val freshRec      : (Longid.longid*EqStatus) list -> Supply -> TyName list*Supply
val freshClass    : Longid.longid -> Supply -> TyName*Supply

(* Freshen an existing name, changing only its stamp *)
val freshen       : Longid.longid -> TyName * Supply -> TyName*Supply
val temp          : Longid.longid list * EqStatus -> TyName list

val newEquality   : TyName -> EqStatus -> TyName

(*----------------------------------------------------------------------*)
(* Creation of external type names					*)
(* [externalXXX(id,longid,d)]:                                          *)
(* - [XXX] determines type of equality to use                           *)
(* - [id] is the assembly                                               *)
(* - [longid]<>[] is the fully qualified name, including namespaces and *)
(*   enclosing classes                                                  *)
(* - [d]>=0 is the class nesting depth, or number of enclosing classes  *)
(*@FUTURE: use a pair of longids instead of [longid,d]                  *)
(*----------------------------------------------------------------------*)
val external	  : Id.id * Longid.longid * int -> TyName
val externalEq    : Id.id * Longid.longid * int -> TyName
val externalVal	  : Id.id * Longid.longid * int -> TyName
val externalValEq : Id.id * Longid.longid * int -> TyName

(*----------------------------------------------------------------------*)
(* Predicates								*)
(*----------------------------------------------------------------------*)
(* True if this is an external or internal class type (not struct/primitive) *)
val isClass       : TyName -> bool
val isExternal    : TyName -> bool
val fromExternal  : TyName -> (Id.id * Longid.longid * int) option

(*----------------------------------------------------------------------*)
(* Convert a type name into a string for printing, qualifying with 	*)
(* the stamp if "showStamps" is set.                                    *)
(*----------------------------------------------------------------------*)
val toString	  : TyName -> string

(*----------------------------------------------------------------------*)
(* Was a tyname generated earlier than the point specified?     	*)
(*----------------------------------------------------------------------*)
val earlier       : TyName * Supply -> bool

val longid        : TyName -> Longid.longid

(* nesting depth of an external type >= 0 *)
val depth        : TyName -> int

end
