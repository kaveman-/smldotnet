(*======================================================================*)
(* Source file manager.  						*)
(* See signature for more details.                                      *)
(*======================================================================*)
structure SourceManager :> SOURCEMANAGER =
struct

(*@TODO: put in Runtime.sml; make configurable *)
val preOpenedStructures = ["Datatypes", "Basis"]

(*----------------------------------------------------------------------*)
(* Path for basis and project source files.				*)
(* See signature for details.						*)
(*----------------------------------------------------------------------*)
val basisPath = ref ([] : (string*string list) list)
val projPath  = ref ([] : (string*string list) list)

(*----------------------------------------------------------------------*)
(* Extensions for the four kinds of SML entity				*)
(*----------------------------------------------------------------------*)
fun typeToExt kind =
case kind of
(* SL: or *)
(*
  (Entity.Sig | Entity.FunSig) => "sig"
| (Entity.Str | Entity.Fun) => "sml"
*)
  Entity.Sig => "sig"
| Entity.FunSig => "sig"
| Entity.Str => "sml"
| Entity.Fun => "sml"

| Entity.Assembly => "dll"

(*----------------------------------------------------------------------*)
(* Entity translation       						*)
(*----------------------------------------------------------------------*)
val translation = ref (Entity.Map.empty : string Entity.Map.map) 

(*----------------------------------------------------------------------*)
(* Entries in paths are of two kinds:					*)
(*   directories, for which file timestamp info is cached		*)
(*   files, for which the AST and timestamp are recorded.		*)
(*@FUTURE: don't store the whole AST, just the list of entities.	*)
(*----------------------------------------------------------------------*)
datatype PathInfo = 
  Dir of Time.time StringMap.map
| File of Syntax.Dec * Time.time

type PathEntry = 
  { name : string, strids : string list, info : PathInfo }

(*----------------------------------------------------------------------*)
(* Entry info for the basis and project paths.				*)
(* Absolute file names (from translation) are maintained separately     *)
(*----------------------------------------------------------------------*)
val basisdirinfo = ref ([] : PathEntry list)
val projdirinfo = ref ([] : PathEntry list)
val absfileinfo = ref (StringMap.empty : Time.time StringMap.map)

(*----------------------------------------------------------------------*)
(* Gather datestamp information about a particular path.		*)
(*----------------------------------------------------------------------*)
fun getStampsForPath (path, description) =
let

fun listDir ([], result) = 
    SOME (rev result)

  | listDir ((name,strids)::names, result) =
    if OS.FileSys.access(name, [])
    then
      (* Is it a directory? *)
      if OS.FileSys.isDir name
      then   
      let
        val dirstream = OS.FileSys.openDir name
        fun make result =
          case OS.FileSys.readDir dirstream of
            NONE => 
            result

          | SOME s => 
            let
              val fname = OS.Path.joinDirFile { dir = name, file = s }
            in
              if not (OS.FileSys.access(fname, [])) 
	      orelse OS.FileSys.isDir fname
              then make result
              else make (StringMap.insert(result, s, FileOps.getTime fname))
            end

        val d = (make StringMap.empty) handle e => 
                (OS.FileSys.closeDir dirstream; raise e)
      in
        OS.FileSys.closeDir dirstream;
        listDir (names, { name=name, strids=strids, info=Dir d } :: result)
      end    

      (* It's not a directory; parse whole file *)
      else 
      let
        val time = FileOps.getTime name
      in
        case ParseManager.parse (name, time) of
          ParseManager.Success (dec, sourcemap) =>
          listDir (names,  
            { name=name, strids=strids, info=File(dec,time) } :: result)

        | _ =>
          (PrintManager.println ("File " ^ name ^ " does not parse.\n"); NONE)
      end
    else
    (
      PrintManager.println
        ("Directory or file " ^ name ^ " in " ^ description ^ " does not exist\n");
      NONE
    )
in
  listDir (path, [])
end

(*----------------------------------------------------------------------*)
(* Gather datestamps for the absolute files in the entity map		*)
(*----------------------------------------------------------------------*)
fun getStampsForFiles () =
let
  fun gather (filename, result) =
    if OS.Path.isAbsolute filename
    then StringMap.insert(result, filename, FileOps.getTime filename)
    else result
in
  absfileinfo := Entity.Map.foldl gather StringMap.empty (!translation)
end


(*----------------------------------------------------------------------*)
(* Update the datestamp info.						*)
(*----------------------------------------------------------------------*)
fun sync () =
  PrintManager.process ("Checking timestamps on source files...", false)
  (fn () =>
    (getStampsForFiles ();
    (case (getStampsForPath (!basisPath, "basis path")) of
        NONE => false
      | SOME b =>
        (basisdirinfo := b;
        case getStampsForPath (case !projPath of [] => [(OS.FileSys.getDir(), preOpenedStructures)] | p => p, "source") of
          NONE => false
        | SOME p => 
          (projdirinfo := p; true))))
    handle 
      OS.SysErr(s,_) => 
      (PrintManager.println s; false)

    | IO.Io { name, function, cause } =>
      (PrintManager.println (exnMessage cause ^ " in " ^ name); 
       false)
  )

(*----------------------------------------------------------------------*)
(* Try each directory/archive in the list for a filename		*)
(*----------------------------------------------------------------------*)
fun find entity [] filename = NONE
  | find entity ({name,info,strids}::dirs) filename =
    case info of
      Dir times =>
      let           
         val fullname = OS.Path.joinDirFile {dir = name,file = filename}
      in
        case StringMap.find(times, filename) of
          NONE => 
          (SOME ((fullname, OS.FileSys.modTime fullname), strids)
          handle OS.SysErr _ => find entity dirs filename)

        | SOME time =>
          SOME ((fullname, time), strids)
      end

    | File (dec, time) =>
      case SyntaxCheck.find (dec, entity) of
        NONE => find entity dirs filename
      | SOME _ => SOME ((name, time), strids)
      

(*----------------------------------------------------------------------*)
(* Find a file for this entity reference.       			*)
(*----------------------------------------------------------------------*)
fun fileRefFor (entity as (etype, id)) =
  (*..................................................................*)
  (* Apply the translation to get a single filename or add the        *)
  (* appropriate extensions to the entity name given.                 *)
  (*..................................................................*)
  case Entity.Map.find(!translation, entity) of

    (* It's not in the map so search for it on the path *)
    NONE => 
    let 
      val filename = OS.Path.joinBaseExt 
        {base = Id.toString id, ext = SOME (typeToExt etype) }
    in
      case find entity (!basisdirinfo @ !projdirinfo) filename of
        SOME (result, strids) =>
        SOME (Entity.makeFileRef result, 
          map (ListOps.singleton o Id.fromString) strids)

      | NONE => 
        NONE
    end

  | SOME name => 
    case StringMap.find(!absfileinfo, name) of
      NONE => NONE
    | SOME time => 
        let val {dir,...} = OS.Path.splitDirFile(name)
            val dir = String.map Char.toLower (OS.Path.mkCanonical dir)
	    (* determine preopened structures from presence on basis path *)
            (*@HACK: we need to normalise the paths...*)
	    val preOpenedStrs =  List.foldr 
                             (fn ((basisdir,opened),preopened) => 
                                if (String.map Char.toLower (OS.Path.mkCanonical(basisdir))) =  
                                   dir
	                        then opened
                                else preopened)
	                    preOpenedStructures  (* the default *)
	                    (!basisPath)
        in
		SOME (Entity.makeFileRef (name, time),      
	  	   map (ListOps.singleton o Id.fromString) preOpenedStrs)

        end
 


fun sourcePath root [] = 
    (projPath := []; 
     PrintManager.println "Source path cleared"; 
     OS.Process.success)

  | sourcePath root dirs =
let
  fun addDirs ([], acc, true) = OS.Process.failure
    | addDirs ([], acc, false) =
      (projPath :=  
        map (fn x => (x, preOpenedStructures)) (rev acc) @ !projPath;
       OS.Process.success)
    | addDirs ((dir,SOME s)::dirs, acc, fail) =
      (PrintManager.println 
       ("invalid argument " ^ dir ^ " = " ^ s ^ " to source command (argument is a binding, should be a directory)");
       OS.Process.failure)
    | addDirs ((dir,NONE)::dirs, acc, fail) =
      case FileOps.normalizeDir root dir of
        Result.Success normdir =>
        addDirs (dirs, normdir::acc, fail)

      | Result.Failure message =>
        (PrintManager.println message; addDirs (dirs, acc, true))
in
  addDirs (dirs, [], false)
end

val _ = Commands.add "source"
{
  act = sourcePath,
  query = fn () => "source:" ^ Pretty.simpleVec ",\n" #1 (!projPath),
  syntax = "<dir>,...,<dir>",
  help = "source <dir>,...,<dir>\n  Set path to search for sources\n\
         \source\n  Restore default (look in working directory)\n\
         \source?\n  Query setting"
}

(* Create a mapper command X that processes the syntax 
     X <id>=<file>,...,<id>=<file>
     X <id>?
*)
fun makeMapper command kind =
{
  query = fn () =>
    command ^ ":" ^ Pretty.simpleVec ",\n" 
     (fn ((kind,id),name) => Id.toString id ^ "=" ^ name) 
     (Entity.Map.listItemsi 
       (Entity.Map.filteri (fn ((kind',_),_) => kind=kind') (!translation))),

  act = fn root => 
  fn [] =>
   (
     PrintManager.println (command ^ ": mapping cleared");
     translation := Entity.Map.mapPartiali (fn ((kind',_),f) =>
       if kind=kind' then NONE else SOME f) (!translation);
     OS.Process.success
   )
 
   | [(id,NONE)] =>
    if String.sub(id, size id - 1) <> #"?"
    then 
    (
      PrintManager.println (command ^ ": expected = following " ^ id);
      OS.Process.failure
    )
    else 
    let val id = String.extract(id, 0, SOME (size id - 1))
    in
      sync();
      case fileRefFor (kind,Id.fromString id) of
        NONE => 
        PrintManager.println (command ^ " " ^ id ^ " does not exist")

      | SOME ((filename,time),_) => 
        PrintManager.println (command ^ " " ^ id ^ "=" ^ filename);

      OS.Process.success
    end

  | args =>
    let
      fun processArg ((id,nameopt),(m,success)) = 
      case nameopt of
        NONE => 
        (
          PrintManager.println (command ^ ": expected = or ? following " ^ id);
          (m, false)
        )

      | SOME name =>
        case FileOps.normalizeFile root name of
          Result.Failure message =>
          (
            PrintManager.println (command ^ " " ^ id ^ ": " ^ message);
            (m, false)
          )

        | Result.Success normname =>
          (Entity.Map.insert(m, (kind,Id.fromString id), normname), success)
    in
      case foldl processArg (!translation,true) args of
        (_,false) => OS.Process.failure
      | (m,true) => (translation := m; OS.Process.success)
    end,
  
  syntax = "<id>=<file>,...,<id>=<file>",
  help = command ^ " <id>=<file>,...<id>=<file>\n\
                   \  Extend file mapping for top-level " ^ command ^ "s\n" ^ 
         command ^ " <id>?\n\
                   \  Query mapping of particular " ^ command ^ "\n" ^
         command ^ "?  \n\
                   \  Query entire mapping\n" ^
         command ^ "\n\
                   \  Clear entire mapping" 
}

val _ = Commands.add "structure" (makeMapper "structure" Entity.Str)
val _ = Commands.add "functor" (makeMapper "functor" Entity.Fun)
val _ = Commands.add "signature" (makeMapper "signature" Entity.Sig)


end