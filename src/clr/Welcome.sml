(*======================================================================*)
(* Welcome message including version information			*)
(*======================================================================*)
structure Welcome =
struct

(*val version = "1.0"
  val build = Date.yearDay (Date.fromTimeLocal (Time.now ())) : int
*)

val showTools = Controls.add true "env.showTools"

(*----------------------------------------------------------------------*)
(* Message printed upon entering an interactive session			*)
(* Also put in log.                                                     *)
(*----------------------------------------------------------------------*)
fun welcomeMessage () =
let
  val frameworkVersion = "\n  Running on .NET Framework v" ^ RuntimeEnv.getVersion ()
  val tools = if not (Controls.get showTools) then ""
              else "\n  Using assembler in " ^ RuntimeEnv.getIlasmFileName() ^
                   "\n  Using class list tool in " ^ RuntimeEnv.getClslistFileName() ^
                   "\n  Using metadata import tool in " ^ RuntimeEnv.getGetmetaFileName() ^ 
              (case RuntimeEnv.getPeverifyFileName() of NONE => "" | SOME name => "\n  Using verifier tool in " ^ name)
in
  RuntimeNames.compiler ^ " " ^ Version.versionstring ^ " " ^ Version.buildstring ^ frameworkVersion ^ tools ^ "\n"
end


(* 
  " on " ^ Compiler.architecture ^
  " under " ^ SMLofNJ.SysInfo.getOSName () ^

*)

end
