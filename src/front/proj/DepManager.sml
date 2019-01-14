(*======================================================================*)
(* Dependency information manager.					*)
(*======================================================================*)
structure DepManager :> DEPMANAGER =
struct

(*----------------------------------------------------------------------*)
(* Reduced parse tree cache: a map from file names to pairs of the form *)
(*   (reduced parse tree, timestamp of parsed file).			*)
(*----------------------------------------------------------------------*)
type Cache = (SmallSyntax.Dec * Time.time) StringMap.map
val cache = ref (StringMap.empty : Cache)

val dumpDep = Controls.add false "dep.dump"
val persist = Controls.add true "env.persistDep"

datatype Result =
  NotFound		
| ParseError
| Success of SmallSyntax.DecItem * Entity.FileRef

(*----------------------------------------------------------------------*)
(* The subdirectory used to persist reduced parse trees.		*)
(*----------------------------------------------------------------------*)
val depdir = RuntimeNames.depdir

(*----------------------------------------------------------------------*)
(* What's the dependency cache directory associated with this file?	*)
(*----------------------------------------------------------------------*)
fun depCacheDir filename =
let
  val dir = OS.Path.dir filename
in
  OS.Path.joinDirFile {dir=dir,file=depdir}
end

(*----------------------------------------------------------------------*)
(* What's the name of the dependencies file for this source file?	*)
(*----------------------------------------------------------------------*)
fun depFileName filename = 
let
  val {dir,file} = OS.Path.splitDirFile filename
in
  OS.Path.joinDirFile {dir=OS.Path.joinDirFile {dir=dir,file=depdir},
    file=file}
end

(* Format number: hi-byte is version no., lo-byte is DEpendency info *)
fun makePersister filename = 
  Pickle.persist (Word8Vector.fromList [0wx04,0wxDE], filename, SmallSyntax.Pickle.dec)

(*----------------------------------------------------------------------*)
(* Serialize dependency information for a single source file		*)
(*----------------------------------------------------------------------*)
fun serialize (filename, smalldec) =
if not (Controls.get persist) then ()
else
PrintManager.process ("Saving dependency information for " ^ filename, false) (fn () =>
let
  val dir = depCacheDir filename
  val _ = 
    if OS.FileSys.access (dir,[]) then ()
    else OS.FileSys.mkDir dir
  val (pickle,_) = makePersister (depFileName filename)
in
  pickle smalldec handle Pickle.Unpickle s => 
        (PrintManager.println 
        ("[Warning: error writing dependency information for " ^ 
        filename ^ ": " ^ s ^ "]"); ())
end)

(*----------------------------------------------------------------------*)
(* Deserialize dependency information for a single source file		*)
(*----------------------------------------------------------------------*)
fun deserialize (filename,time) =
if not (Controls.get persist) then NONE
else
let
  val depname = depFileName filename
in
  if OS.FileSys.access(depname, [])
  then 

  (* If dependency file is dated wrt source file, don't read it *)
  if Time.>(time, OS.FileSys.modTime depname)
  then NONE
  
  else
  PrintManager.process 
  ("Reading dependency information for " ^ filename, false)
  (fn () =>
  let
    val (_,unpickle) = makePersister depname
  in
      SOME (unpickle ())
      handle Pickle.Unpickle s => 
        (PrintManager.println 
        ("[Warning: error reading dependency information for " ^ 
        filename ^ ": " ^ s ^ "]"); NONE)
  end)
  else NONE
end

(*----------------------------------------------------------------------*)
(* Clear the dependency cache (this is benign).				*)
(*----------------------------------------------------------------------*)
fun reset () = cache := StringMap.empty

(*----------------------------------------------------------------------*)
(* Given a bunch of (reduced) top-level decs, find the one appropriate  *)
(* to this entity.							*)
(*----------------------------------------------------------------------*)
fun findDec (entity as (etype,eid), smalldec, sourcefileref, longids) =
let
  open SmallSyntax

  fun add strexp = 
      if List.null longids then strexp else StrLet([Open longids], strexp)

  fun addLocal decitem = 
      Local([Open longids],[decitem])

  fun match (Structure bindings) =
      if etype = Entity.Str
      then 
      let 
        fun loop [] = NONE
          | loop ((id,strexp)::rest) =
            if Symbol.equal(id, eid) then SOME (Structure [(id,add strexp)])
            else loop rest
      in
        loop bindings
      end
      else NONE

    | match (Signature bindings) =
      if etype = Entity.Sig
      then 
      let 
        fun loop [] = NONE
          | loop ((id,sigexp)::rest) =
            if Symbol.equal(id, eid) then SOME (addLocal(Signature [(id,sigexp)]))
            else loop rest
      in
        loop bindings
      end
      else NONE

    | match (Functor bindings) =
      if etype = Entity.Fun 
      then
      let 
        fun loop [] = NONE
          | loop ((id,spec,strexp)::rest) =
            if Symbol.equal(id, eid) then SOME (addLocal(Functor [(id,spec,add strexp)]))
            else loop rest
      in
        loop bindings
      end
      else NONE

    | match (Local(_,[dec])) = match dec

    | match _ = NONE

  and loop [] = 
      (PrintManager.println ("[Warning: source file " ^ #1 sourcefileref ^
       " has no binding for  " ^ EntityOps.description entity ^ "]"); NotFound)

    | loop (d::ds) =
      case match d of
        NONE => loop ds
      | SOME d => 
        Success (d, sourcefileref)
in 
  loop smalldec
end
  
(*----------------------------------------------------------------------*)
(* Get dependency information, reparsing if source file has changed.	*)
(*----------------------------------------------------------------------*)
fun dep entity =
case SourceManager.fileRefFor entity of
  NONE =>
  (PrintManager.println ("[Warning: source file not found for " ^ 
    EntityOps.description entity ^ "]"); NotFound)

| SOME (sourcefileref as (sourcefile,sourcetime), longids) =>
  let
    fun updateCache smalldec =
    (
      cache := StringMap.insert(!cache, sourcefile, (smalldec, sourcetime));
      PrintManager.dump dumpDep (fn prln =>
        (prln ("Dependency info for " ^ EntityOps.toString entity ^ ":");
         prln (SmallSyntax.decToString smalldec)))
    )

    fun reparse () =
      case ParseManager.parse sourcefileref of
        ParseManager.Success (dec,_) =>
        let
          val smalldec = SyntaxConvert.convert dec
        in
          updateCache smalldec;
          serialize (sourcefile, smalldec);
          findDec (entity, smalldec, sourcefileref, longids)
        end

      | _ =>
        ParseError
  in
    case StringMap.find(!cache, sourcefile) of

      (* If it's in the cache then only re-parse if it's out of date *)
      SOME (smalldec, cachetime) =>

      (* Has it changed? *)
      if Time.>(sourcetime, cachetime) orelse Time.<(sourcetime,cachetime)
      then reparse ()
      else findDec (entity, smalldec, sourcefileref, longids)

      (* If it's not in the cache then first look for dependency file *)
    | NONE =>
      case deserialize sourcefileref of
        NONE => 
        reparse ()

      | SOME smalldec =>
        (updateCache smalldec;
        findDec (entity, smalldec, sourcefileref, longids))
  end
    
end

