structure Path =
struct

val separator = 
  if SMLofNJ.SysInfo.getOSKind () = SMLofNJ.SysInfo.UNIX
  then #":" else #";"

(*----------------------------------------------------------------------*)
(* Turn a (":" or ";")-separated string into a list of filenames/dirs	*)
(*----------------------------------------------------------------------*)
fun pathToList s = 
  String.tokens (fn c => c=separator) s

(*----------------------------------------------------------------------*)
(* Generate a path in OS-dependent form (":" for Unix, ";" for others)	*)
(*----------------------------------------------------------------------*)
fun listToPath dirs =
  Pretty.simpleVec (str separator) Gen.identity dirs

end

