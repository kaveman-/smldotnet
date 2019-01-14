(*======================================================================*)
(* Manage the executables						*)
(*======================================================================*)
structure TargetManager :> TARGETMANAGER =
struct

datatype TargetKind =
  Exe    (* Executable *)
| Lib    (* Library *)
| Module (* Module *)

(*----------------------------------------------------------------------*)
(* State:								*)
(*   default name (used in absence of /out)				*)
(*   name (as set by /out)						*)
(*   kind (as set by /target)						*)
(* Default name = NONE indicates that there was an error.		*)
(*----------------------------------------------------------------------*)
val defaultName = ref (NONE : string option)
val name = ref (NONE : string option)
val targetKind = ref Exe

(* Abort compilation; target has not been generated *)
fun abort () = defaultName := NONE

(* Set the kind and default name *)
fun setKind kind = targetKind := kind
fun setDefaultName name = defaultName := SOME name

fun setName root relname = 
let
  val fullname = FileOps.normalizePath root relname
in
  case OS.Path.splitBaseExt fullname of
      { base,ext=NONE } => (name := SOME base; OS.Process.success)
    | { base,ext=SOME ext } =>
      let val lext = String.map Char.toLower ext
      in
        if lext=RuntimeNames.executableExt
        then (name := SOME base; targetKind := Exe; OS.Process.success)
        else if lext=RuntimeNames.libExt
        then (name := SOME base; targetKind := Lib; OS.Process.success)
        else (PrintManager.println "Invalid extension"; OS.Process.failure)
      end
end

val _ = Commands.add "target"
{
  act = fn root => 
  fn [(arg,NONE)] =>
  (
    case arg of
      "library" => (targetKind := Lib; OS.Process.success)
    | "exe" => (targetKind := Exe; OS.Process.success)
    | _ => (PrintManager.println 
        "target: invalid target type: expected 'exe' or 'library'"; 
        OS.Process.failure)
  )
   | _ => (PrintManager.println 
     "target: invalid syntax: expected 'target:exe' or 'target:library'"; 
     OS.Process.failure),

  query = fn () =>
  case !targetKind of
    Exe => "target:exe"
  | Lib => "target:library",

  syntax = "(exe | library)",

  help = "target exe\n\
         \  Build an executable (default)\n\
         \target library\n\
         \  Build a library"
}

  
val _ = Commands.add "out" 
{
  act = fn root =>
          fn [(arg,NONE)] => setName root arg
           | [] => (name := NONE; OS.Process.success)
           | _ => (PrintManager.println "Target expects a single parameter"; OS.Process.failure),
  query = fn () => "target:" ^ getOpt(!name, ""),
  syntax = "[<file>]",
  help = "out <target>\n  Set base name of target\n\
         \out <target>.dll\n  Set name of target (library)\n\ 
         \out <target>.exe\n  Set name of target (executable)\n\
         \out\n  Restore default (= main exported structure id)\n\
         \out?\n  Query setting"
}

  
fun getInfo () =
case !defaultName of
  NONE => NONE
| SOME dname =>
  let
    val (fullname,name) = 
      case !name of
        NONE => (dname,dname)
      | SOME name =>
        let
    	  val { dir,file } = OS.Path.splitDirFile name
        in
          (name,file)
        end
    val ext = case !targetKind of
                Lib => RuntimeNames.libExt 
              | Exe => RuntimeNames.executableExt
  in
    SOME 
    { 
      kind = !targetKind,
      out = OS.Path.joinBaseExt { base=fullname, ext=SOME ext },
      asm = OS.Path.joinBaseExt { base=fullname, ext=SOME RuntimeNames.assemblerExt },
      assembly = name,
      module = OS.Path.joinBaseExt { base=name, ext=SOME ext }
    }
  end

val _ = Commands.add "run"
{
  act = fn root => 
  fn args => 
  case getInfo () of
    NONE => 
    (
      PrintManager.println "run: nothing has been compiled successfully"; 
      OS.Process.failure
    )

  | SOME { kind = Exe, out, ... } =>
    RuntimeEnv.run { program = out, args = Pretty.simpleVec " " #1 args }

  | SOME _ =>
    (
      PrintManager.println ("run: cannot execute a library"); 
      OS.Process.failure
    ),

  query = fn () => 
  case getInfo () of
    NONE => "run: nothing has been compiled successfully"
  | SOME { kind = Exe, ... } => "run: ready for execution"
  | SOME { kind = Dll, ... } => "run: cannot execute a library",

  syntax = "<args>",
  help = "run <args>\n\
         \  Run most recently compiled executable"
}

end
