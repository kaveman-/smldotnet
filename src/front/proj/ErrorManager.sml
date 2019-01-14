(*======================================================================*)
(* Error manager: manage printing of error messages.                    *) 
(*======================================================================*)
structure ErrorManager :> ERRORMANAGER =
struct

val useStderr = Controls.add false "env.useStderr"

fun printErrorString s =
(
  if Controls.get useStderr
  then TextIO.output(TextIO.stdErr, s ^ "\n")
  else ();
  PrintManager.println s
)

fun printErrors (sm, []) = ()
  | printErrors (sm, errors) = Error.print (printErrorString, sm, errors)

end (* of struct *)

