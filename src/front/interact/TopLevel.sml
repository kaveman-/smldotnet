(*======================================================================*)
(* Top-level loop							*)
(*======================================================================*)
structure TopLevel :> TOPLEVEL =
struct

    (* force registration of transformations by mentioning their structures *)
    local open Arity TailRec Case Deunit Equality Flatten 
	       Inline Monomorphise FloatHoist FunScope 
	       TypeCommand DeadArgs
    in
    end

(*----------------------------------------------------------------------*)
(* Prompt for command entry.						*)
(*----------------------------------------------------------------------*)
val prompt = "\\ "

structure P = ParseCommand
structure C = CommandSyntax

(*----------------------------------------------------------------------*)
(* Top level interactive loop						*)
(*----------------------------------------------------------------------*)
fun topLoop () =
let
  (* Set up control-C handler *)
  val _ = Interrupt.init ()

  fun loop () =
  let
    val _ = PrintManager.restart ()
    val _ = print prompt
    val lineOpt = TextIO.inputLine TextIO.stdIn
    fun quit () = Interrupt.finish ()
  in
    case lineOpt of
      NONE => quit ()
    | SOME line =>
    let
      val result = P.parse C.Interactive ("stdin",line)
    in
      case result of
        (sm,P.Failure errors) =>
        (ErrorManager.printErrors (sm, errors); loop ())

      | (sm, P.Success commands) =>
        case InterpretCommand.interpret (OS.FileSys.getDir()) print commands of
          InterpretCommand.Quit => quit ()
        | _ => loop ()
    end
  end
in
  loop ()
end

(*----------------------------------------------------------------------*)
(* Entry point for command-line compiler				*)
(*@todo akenn: catch and report exceptions from EnvVars etc.		*)
(*----------------------------------------------------------------------*)
fun entry (name : string, args) =
let
  (* Turn off GC messages *)
  val _ = SMLofNJ.Internals.GC.messages false
in
  (* Make settings based on environment variables *)
  if not (RuntimeEnv.setup NONE) then OS.Process.failure
  else
  let

  (* First print the version banner and copyright notice *)
  val _ = print (Welcome.welcomeMessage ())


  val _ = case OS.Process.getEnv RuntimeNames.lib of
            NONE => (PackageManager.setLib []; true)
          | SOME path => (PackageManager.setLib (Path.pathToList (PathConv.toInternal path)); true)

  (* directories *)
  val src_basis = OS.Path.concat("src","basis")
  val src_basis_closed = OS.Path.concat(src_basis,"closed")
  val src_basis_runtime = OS.Path.concat(src_basis,RuntimeNames.runtime)


  (* Set the basis path *)

  val _ = SourceManager.basisPath := 
    [(OS.Path.concat(RuntimeEnv.getCompilerDir(),src_basis_closed), []),
     (OS.Path.concat(RuntimeEnv.getCompilerDir(),src_basis_runtime),["Datatypes"]),
     (OS.Path.concat(RuntimeEnv.getCompilerDir(),src_basis), ["Datatypes"])]

  (* Extract the command line arguments as a single string *)
  val line = String.concat (map (fn x => x ^ " ") args) ^ "\n"

  (* Interpret boot script *)
    val bindir = RuntimeEnv.getCompilerBinDir()
    val _ = 
     let
      val fullname =
      OS.Path.joinDirFile { dir = bindir, file = 
      OS.Path.joinBaseExt { base = "config", ext = SOME RuntimeNames.scriptExt } }
    in
      if not (OS.FileSys.access(fullname, [])) then ()
      else 
      case Script.load fullname of
        NONE =>
        ()

      | SOME (sm, commands) =>
        ignore (InterpretCommand.interpret bindir print commands)
    end
in
  case P.parse C.CommandLine ("stdin",line) of
    (sm,P.Failure errors) =>
    (ErrorManager.printErrors (sm,errors); OS.Process.failure)

  | (sm,P.Success commands) =>
    let
      val (ids,nonids) = List.partition (fn (_,C.Id id) => true | _ => false) commands
    in
      case InterpretCommand.interpret (OS.FileSys.getDir()) print nonids of
        InterpretCommand.Success => 
        if List.null ids then
          if !Commands.mustQuit then OS.Process.success
          else (topLoop (); OS.Process.success)
        else 
        if Make.setExports (map (fn (_,C.Id id) => (id,NONE)) ids) = OS.Process.success
        then (if Make.make false then OS.Process.success else OS.Process.failure)
        else OS.Process.failure        

      | InterpretCommand.Failure =>
        OS.Process.failure

      | InterpretCommand.Quit =>
        OS.Process.success
    end
end

end

end