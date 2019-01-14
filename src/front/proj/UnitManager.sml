(*======================================================================*)
(* Unit of compilation manager.						*)
(*======================================================================*)
structure UnitManager :> UNITMANAGER =
struct

open UnitTypes

val persist = Controls.add true "env.persistObj"

(*----------------------------------------------------------------------*)
(* Cached results with the timestamped file reference of the source     *)
(* from which they were	derived. If no ref is present then the result   *)
(* is out of date.                                                      *)
(*----------------------------------------------------------------------*)
type Cache = (Entry * Entity.FileRef option) Entity.Map.map
val cache = ref (Entity.Map.empty : Cache)

(*----------------------------------------------------------------------*)
(* The subdirectory used to persist compiled units.			*)
(*----------------------------------------------------------------------*)
val unitdir = RuntimeNames.objdir

(*----------------------------------------------------------------------*)
(* What's the compiled unit directory associated with this file?	*)
(*----------------------------------------------------------------------*)
fun unitCacheDir filename =
let
  val dir = OS.Path.dir filename
in
  OS.Path.joinDirFile {dir=dir,file=unitdir}
end

(*----------------------------------------------------------------------*)
(* What's the name of the object file for this entity/fileref pair?	*)
(*----------------------------------------------------------------------*)
fun unitFileName (entity : Entity.Ref, fileref as (filename, _)) = 
let
  val {dir,file} = OS.Path.splitDirFile filename
in
  OS.Path.joinDirFile {dir=OS.Path.joinDirFile {dir=dir,file=unitdir},
    file=file ^ "." ^ EntityOps.typeToString (#1 entity) ^ "." ^ Id.toString (#2 entity)}
end

(*
local 
  open Pickle
in
  fun makePickler () =
  let 
    val (abstr,ty,cty) = MILPickle.make ()
    val tynameTysPickler = TyName.mapPickler ty

    val strentryPickler =
    wrap (fn (E, supply, term, tynameTys) =>
             { E = E, supply = supply,
               term = term, tynameTys = tynameTys },
          fn {E, supply, term, tynameTys} =>
             (E, supply, term, tynameTys))
    (quadruple (EnvOps.envPickler, Var.supplyPickler, abstr, tynameTysPickler))
  val pickler =
  alttag (fn Sig _ => 0 | Str _ => 1 | Fun _ => 2)
  [
    wrap (Sig, fn Sig x => x) EnvOps.sigPickler,
    wrap (Str, fn Str x => x) strentryPickler,
    wrap (Fun, fn Fun x => x) EnvOps.funPickler
  ]
  in
    pair (wrap (Time.fromSeconds o Int32.toLarge, 
                Int32.fromLarge o Time.toSeconds) int32, pickler)
  end
end (* of local open Pickle *)

*)
local 
  open Pickle
in
  val pickleMIL = ref true
  fun makePickler () =
  let 

    val milPickler = 
        if !pickleMIL
	then let val (abstr,ty,cty) = MILPickle.make ()
	         val tynameTysPickler = TyName.mapPickler ty
	     in	
	         quadruple (Var.supplyPickler, 
		            Var.supplyPickler, 
                            abstr, 
                            tynameTysPickler)
	     end	
	else wrap (fn () => (Var.initial,
                             Var.initial,
	                     ([],MILTerm.Triv[]),
	                     TyName.Map.empty),
	           fn _ => ())
	           unit

    val strentryPickler =
    wrap (fn (E, (supply, tyvarsupply, term, tynameTys)) =>
             { E = E, supply = supply, tyvarsupply = tyvarsupply,
               term = term, tynameTys = tynameTys },
          fn {E, supply, tyvarsupply, term, tynameTys} =>
             (E, (supply, tyvarsupply, term, tynameTys)))
    (pair (EnvOps.envPickler,milPickler))
	                                      
  val pickler =
  alttag (fn Sig _ => 0 | Str _ => 1 | Fun _ => 2)
  [
    wrap (Sig, fn Sig x => x) EnvOps.sigPickler,
    wrap (Str, fn Str x => x) strentryPickler,
    wrap (Fun, fn Fun x => x) EnvOps.funPickler
  ]
  in
    pair (wrap (Time.fromSeconds o Int32.toLarge, 
                Int32.fromLarge o Time.toSeconds) int32, pickler)
  end
end (* of local open Pickle *)



(* Format number: hi-byte is version no., lo-byte is C0mpilation unit *)

val debugFlag = Controls.add true "debug.pickle"

fun makePersister name = 
  Pickle.persist (Word8Vector.fromList [0wxA8, 0wxC0], name, makePickler())

(*----------------------------------------------------------------------*)
(* Serialize dependency information for a single entity			*)
(*----------------------------------------------------------------------*)
fun serialize (entity, fileref : Entity.FileRef, entry) =
if not (Controls.get persist) then ()
else
PrintManager.process ("Saving object file for " ^ 
  EntityOps.toString entity, false) (fn () =>
let
  val dir = unitCacheDir (#1 fileref)
  val (pickle,_) = makePersister (unitFileName (entity, fileref))
  val _ = 
    if OS.FileSys.access (dir,[]) then ()
    else OS.FileSys.mkDir dir
in  
  pickle (#2 fileref, entry)
    handle Pickle.Pickle s => 
	      PrintManager.println ("[Warning: error writing object file for " ^ EntityOps.toString entity ^ ": " ^s ^ "]")
         | e =>
	      (PrintManager.println ("[Warning: error writing object file for " ^ EntityOps.toString entity ^ ": " ^
				     exnName e ^ " " ^exnMessage e ^ "]"))
end)

(*----------------------------------------------------------------------*)
(* Deserialize dependency information for a single source file		*)
(*----------------------------------------------------------------------*)
fun deserialize (entity, fileref as (filename, time)) =
if not (Controls.get persist) then NONE
else
let
  val fname = unitFileName (entity, fileref)
in
  if OS.FileSys.access(fname, [])
  then 

  PrintManager.process 
  ("Reading object file for " ^ EntityOps.toString entity, false)
  (fn () =>
  let
    val (_,unpickle) = makePersister fname
  in
    SOME (unpickle ())
      handle Pickle.Unpickle s => 
        (PrintManager.println
        ("[Warning: error reading object file for " ^ 
        EntityOps.toString entity ^ ": " ^ s ^ "]"); NONE)
      | e => 
        (PrintManager.println
        ("[Warning: error reading object file for " ^ 
        EntityOps.toString entity ^ ": " ^ exnMessage e ^ "]"); NONE)
  end)

  else NONE
end

(*----------------------------------------------------------------------*)
(* Clear the cache (this is benign).					*)
(*----------------------------------------------------------------------*)
fun reset () = cache := Entity.Map.empty

(*----------------------------------------------------------------------*)
(* Look up an entry, reading from the serialized version if necessary.	*)
(*----------------------------------------------------------------------*)
fun lookup (entity, fileref) =
  case Entity.Map.find(!cache, entity) of
    NONE =>  
    (case deserialize (entity, fileref) of
      NONE => 
      NONE

    | SOME (time, entry) =>
      if EntityOps.fileRefEq (fileref, (#1 fileref, time)) 
      then 
        (cache := Entity.Map.insert(!cache, entity, (entry, SOME fileref));
         SOME entry)
      else NONE
    )

  | SOME (entry, SOME fileref') =>
    if EntityOps.fileRefEq(fileref, fileref') then SOME entry else NONE

    (* Must be primitive *)
  | SOME (entry, NONE) =>
    SOME entry

fun update (entity, filerefopt, entry) =
(
  cache := Entity.Map.insert(!cache, entity, (entry, filerefopt));
  case filerefopt of
    NONE => ()
  | SOME fileref => serialize (entity, fileref, entry)
)

end (* of struct *)

