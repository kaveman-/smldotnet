(*======================================================================*)
(* Command interpreter for scripting language				*)
(*======================================================================*)
structure InterpretCommand :> INTERPRETCOMMAND =
struct

structure C = CommandSyntax


(*----------------------------------------------------------------------*)
(* Interpret a single command. Return true if successfully completed.	*)
(* Note that "quit" never completes.                                    *)
(*----------------------------------------------------------------------*)
fun interpretOne scope print (loc,command) =
(case command of
  C.RunScript name =>
  let
    fun loop' scope' [] = true
      | loop' scope' (command::commands) = 
        if interpretOne scope' print command 
        then loop' scope' commands
        else false
    val name = FileOps.normalizePath scope name
  in
    case Script.load name of
      NONE => false

    | SOME(sm, commands) =>
      let
        val fullname = SourceMap.fileName sm
      in
        PrintManager.println("[Interpreting commands in file " ^ fullname ^ "...]"); 
       loop' (case OS.Path.dir fullname of "" => scope | dir => dir) commands
      end
  end

| C.Command("quit", _) =>
  false

| C.Command(name, arg) =>
  (case Commands.lookup name of
    SOME {act, query, ...} =>
    (case arg of
      C.Query => (PrintManager.println (query ()); true)
    | C.List args => act scope args = OS.Process.success
    )

  | NONE =>
    case Controls.lookup name of
      [] => (PrintManager.println("No such command: " ^ name); false)
    | bs => 
      case arg of
        C.Bool b => (app (fn (_,bref) => Controls.set (bref, b)) bs; true)
      | C.List [] => (app (fn (_,bref) => Controls.set (bref, true)) bs; true)
      | C.Query => 
  (
    case Controls.lookup name of
      [] => (PrintManager.println("?: " ^ name ^ " is not a valid switch"))
    | bs => 
      (app (fn (s,b) => print ((if Controls.get b then "on " else "off ") ^ s ^ "\n")) bs);

    true
  )

  )

) handle (e as (Fail s)) => (print ("\n!!! COMPILER BUG: Fail \"" ^ s
    ^ "\" raised at "
    ^ Pretty.simpleVec "/" Gen.identity (SMLofNJ.exnHistory e) ^ "\n"); false)
   | e => (print ("\n!!! COMPILER BUG: " ^ exnMessage e
    ^ " raised at "
    ^ Pretty.simpleVec "/" Gen.identity (SMLofNJ.exnHistory e) ^ "\n"); false)

datatype Result = Failure | Success | Quit

fun interpret scope print commands =
let
  fun loop [] = Success
    | loop ((_,C.Command("quit",_))::_) = Quit
    | loop (command::commands) = 
      if interpretOne scope print command 
      then loop commands 
      else Failure
in
  loop commands
end

val _ = Commands.add "cd"
{
  act = fn root => fn [(dir,NONE)] => 
                      if not (OS.FileSys.access(dir, []))
                      then (PrintManager.println ("cd: " ^ dir ^ " does not exist"); OS.Process.failure)
                      else if not (OS.FileSys.isDir dir)
                      then (PrintManager.println ("cd: " ^ dir ^ " is not a directory"); OS.Process.failure)
                      else (OS.FileSys.chDir dir; OS.Process.success)
                    | _ => (PrintManager.println "cd: expected directory parameter"; OS.Process.failure),
  query = fn () => "cd:" ^ OS.FileSys.getDir (),
  syntax = "<directory>",
  help = "cd <directory>\n\
         \  Change working directory\n\
         \cd?\n\
         \  Query working directory"
}

val _ = Commands.add "cmd"
{
  act = fn root => 
  fn [] => OS.Process.success
   | [(cmd,NONE)] => OS.Process.system cmd,
  query = fn () => "",
  syntax = "<arguments>",
  help = "cmd <arguments>\n\
         \  Shell command"
}

end
