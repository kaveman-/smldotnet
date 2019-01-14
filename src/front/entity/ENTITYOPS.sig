(*======================================================================*)
(* Operations on entity references (compilation units).			*)
(*======================================================================*)
signature ENTITYOPS =
sig

(* Describe an entity for user messages... *)
val description : Entity.Ref -> string
val typeToString : Entity.Type -> string

(* ...and optionally (if showFileRef is set) present the file ref *)
val descriptionWithFile : Entity.Ref * Entity.FileRef -> string

(* This is just for diagnostic purposes *)
val toString : Entity.Ref -> string

(* Equality on entities and file refs *)
val eq : Entity.Ref * Entity.Ref -> bool
val fileRefEq : Entity.FileRef * Entity.FileRef -> bool

(* Pickler for entities anmd  *)
val pickler : Entity.Ref Pickle.PU
val fileRefPickler : Entity.FileRef Pickle.PU

end

