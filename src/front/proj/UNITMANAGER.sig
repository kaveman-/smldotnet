signature UNITMANAGER =
sig

type Cache = (UnitTypes.Entry * Entity.FileRef option) Entity.Map.map

val pickleMIL : bool ref 
val cache : Cache ref
val lookup : Entity.Ref * Entity.FileRef -> UnitTypes.Entry option
val update : Entity.Ref * Entity.FileRef option * UnitTypes.Entry -> unit

val reset : unit -> unit

end