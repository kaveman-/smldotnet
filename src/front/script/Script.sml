(*======================================================================*)
(* Loading and parsing of scripts					*)
(*======================================================================*)
structure Script :> SCRIPT =
struct

(*----------------------------------------------------------------------*)
(* Given a full file name, read the file and parse it.			*)
(*----------------------------------------------------------------------*)
fun loadFile fullname =
(let
  val s = FileOps.readText fullname ^ "\n"
in
  case ParseCommand.parse CommandSyntax.Script (fullname,s) of
    (sm,ParseCommand.Failure errors) =>
    (ErrorManager.printErrors (sm,errors); NONE)

  | (sm,ParseCommand.Success commands) => 
    SOME(sm,commands)
end) handle IO.Io _ => NONE

(*----------------------------------------------------------------------*)
(* Search the script path for a named script and parse the first found. *)
(*----------------------------------------------------------------------*)
fun load name =
let
  val { base, ext } = OS.Path.splitBaseExt name

  val name = case ext of
    NONE => 
    if OS.FileSys.access(name, [])
    andalso OS.FileSys.isDir name
    then OS.Path.joinDirFile { dir = name, file = 
      OS.Path.joinBaseExt { base = "sources", ext = SOME RuntimeNames.scriptExt } }
    else OS.Path.joinBaseExt { base = name, ext = SOME RuntimeNames.scriptExt }

  | SOME ext =>
    name

  fun fail () = 
  (ErrorManager.printErrorString ("Cannot find script called " ^ name); NONE)
in
  if OS.FileSys.access(name, [])
  then loadFile name
  else fail ()
end

end
