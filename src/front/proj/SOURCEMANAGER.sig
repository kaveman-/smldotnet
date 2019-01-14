(*======================================================================*)
(* Source file manager							*)
(*======================================================================*)
signature SOURCEMANAGER =
sig

(*----------------------------------------------------------------------*)
(* Conversion of signature, structure and functor identifiers to file   *)
(* names works as follows:                                              *)
(* 1. the identifier is looked up in !translation; this produces a      *)
(*    filename which is searched for first in the directories in        *)
(*    !basisPath and then in !projPath.                                 *)
(* 2. if it's not in !translation, each directory in !basisPath and     *)
(*    !projPath is searched for a filename constructed from the         *)
(*    identifer with extension given .sig or .sml as appropriate.       *)
(* IMPORTANT: more than one entity can map onto the same physical file. *)
(* (useful when multiple top-level decs are in the file). 		*) 
(*----------------------------------------------------------------------*)

val translation : string Entity.Map.map ref  (* default is empty    *)

(*----------------------------------------------------------------------*)
(* Paths for project and basis files.					*)
(* Each entry in the path is a pair (dir,strids)			*)
(* where dir is a directory and strids is a list of structure 		*)
(* identifiers that are attached to parsed file as 			*)
(*   local open <strids> in ... end					*)
(* Typically strids = preOpenedStructures = ["Datatypes","Basis"]       *)
(*----------------------------------------------------------------------*)
val preOpenedStructures : string list
val projPath    : (string*string list) list ref  
val basisPath   : (string*string list) list ref

(*----------------------------------------------------------------------*)
(* Cache timestamps from all directories in !basisPath and !projPath.   *)
(* Return true if successful.                               		*)
(*----------------------------------------------------------------------*)
val sync        : unit -> bool

(*----------------------------------------------------------------------*)
(* Return the file ref associated with a particular entity, together	*)
(* with the strids that should be opened implicitly.			*)
(*----------------------------------------------------------------------*)
val fileRefFor : Entity.Ref -> (Entity.FileRef * Syntax.longid list) option

end





