(*======================================================================*)
(* Interop package manager. Purpose: to maintain class paths and a map	*)
(* of the packages and classes visible through the class paths.		*)
(*======================================================================*)
signature PACKAGEMANAGER =
sig

type ClassDir

(* SL: withtype illegal in signatures, inline it *)
(*
datatype Package = Package of Env
withtype Env = 
  {
    classes : ClassDir Symbol.Map.map,
    packages : Package ref Symbol.Map.map
  }
*)
datatype Package = Package of 
{
    classes : ClassDir Symbol.Map.map,
    packages : Package ref Symbol.Map.map
}
type Env = 
  {
    classes : ClassDir Symbol.Map.map,
    packages : Package ref Symbol.Map.map
  }

(*----------------------------------------------------------------------*)
(* Set the class paths							*)
(*----------------------------------------------------------------------*)
val setLib : string list -> unit
val setReferences : string list -> unit (* resolve reference to filename and stamp *)
val initReferences : ((*file*)string * (*stamp*) string) 
	             list -> unit (* using supplied filename and stamp *)

(*----------------------------------------------------------------------*)
(* Get the class paths (for display purposes only)			*)
(*----------------------------------------------------------------------*)
val getReferences : unit -> string list
val getStampedReferences : unit -> (string * string) list

(*----------------------------------------------------------------------*)
(* Load a class, given its directory reference.				*)
(*----------------------------------------------------------------------*)
val getClass :
  Syntax.longid ->
  InterOpTypes.ClassDef option

(*----------------------------------------------------------------------*)
(* Given a class, determine its origin (assembly)			*)
(*----------------------------------------------------------------------*)
val getOrigin :
  Syntax.longid ->
  string option

(*----------------------------------------------------------------------*)
(* Return SOME false if we're sure that the class has no constructors, 	*)
(* is abstract or is an interface (i.e. can't be instantiated).		*)
(* Return SOME true if the class has a constructor and is not abstract  *)
(* and is not an interface. (i.e. can be instantiated).			*)
(* Return NONE if we don't know the answer or the class does not exist. *)
(*----------------------------------------------------------------------*)
val hasConstructor :
  Syntax.longid ->
  bool option

val isValueType :
  Syntax.longid ->
  bool option

val isEnum :
  Syntax.longid ->
  bool option

val getAssembly :
  Longid.longid ->
  Id.id option

val hasEquality :
  Longid.longid -> 
  bool option

(* depth of the possibly nested class *)
val getDepth :
  Longid.longid -> 
  int option

(*----------------------------------------------------------------------*)
(* Runtime (ie. CLR/Java) specific Package Info                         *)
(* associated with entities on the class paths                          *)
(* (realised at declaration of PackageManager)                          *)
(*----------------------------------------------------------------------*)

type PackageInfo

val getPackageInfo : 
  string -> 
  PackageInfo option

(*----------------------------------------------------------------------*)
(* Get at the whole environment						*)
(*----------------------------------------------------------------------*)
val getTop :
  unit -> Package

(*----------------------------------------------------------------------*)
(* Pretty-printing of the package datatype, used for diagnostics only.	*)
(*----------------------------------------------------------------------*)
val packageToString : Package -> string

end

