structure FileOps =
struct

(* Normalize a file or directory name wrt another directory,
   translating slash/backslash to platform-specific version
   and canonicalizing the paths *)
local 
  val separator = String.sub(OS.Path.concat("a","b"), 1)
  val slashes = String.map (fn (#"/") => separator
			     | (#"\\") => separator
			     | c => c)
in
  fun normalizePath root relname =
  let
    val relname = slashes relname
    val root = slashes root
  in
    if OS.Path.isRelative relname
    then OS.Path.mkCanonical (OS.Path.concat(root, relname))
    else OS.Path.mkCanonical relname
  end
end

fun toUnix s =
    let fun forwardslash s = String.map (fn (#"\\") => (#"/") | c => c) s
    in
	if String.size s > String.size "c:\\" andalso
	   String.substring (s, 1, 2)=":\\"
	then
	    "/cygdrive/"^(String.str (String.sub (s, 0)))^
	    (forwardslash (String.extract (s, 2, NONE)))
	else
	    forwardslash s
    end		   

fun toDOS s =
    let fun backslash s = String.map (fn (#"/") => (#"\\") | c => c) s
    in
	if String.size s > String.size "/cygdrive/c" andalso
	   String.isPrefix "/cygdrive/" s then
	    (String.str (String.sub (s, 10)))^":"^
	    (backslash (String.extract (s, 11, NONE)))
	else if String.isPrefix "/" s then
	    "c:\\cygwin"^(backslash s)
	else
	    backslash s
    end		   

(* Obtain timestamp of file if possible without causing IO error.
   There's a problem on Windows 95 (and NT + samba?): time not      
   available if file is open. *)
fun getTime name = (OS.FileSys.modTime name) 
  handle OS.SysErr(s,_) => 
  (print ("\n[Warning: " ^ s ^ " for " ^ name ^ "] "); Time.now ())

(* Normalise this directory relative to the given absolute directory;
   also check for existence *)
fun normalizeDir root reldir =
let
  val dir = normalizePath root reldir
in
  if not (OS.FileSys.access (dir,[]))
  then Result.Failure ("Cannot find directory: " ^ dir)
  else if not (OS.FileSys.isDir dir)
  then Result.Failure ("Not a directory: " ^ dir)
  else Result.Success dir
end
           
(* Normalise this filename relative to the given absolute directory;
   also check for existence *)
fun normalizeFile root relname =
let
  val name = normalizePath root relname
in
  if not (OS.FileSys.access (name,[]))
  then Result.Failure ("No such file: " ^ name)
  else if OS.FileSys.isDir name
  then Result.Failure ("Is a directory: " ^ name)
  else Result.Success name
end
           
fun mkAbsolute path = OS.Path.mkCanonical
                      (if OS.Path.isRelative path 
			   then  OS.Path.concat(OS.FileSys.getDir(),path)
		       else path)

fun tmpName () = OS.FileSys.tmpName ()

(*----------------------------------------------------------------------*)
(* Read a text file to produce a string					*)
(*----------------------------------------------------------------------*)
fun readText name =
    let val f = TextIO.openIn name
        val str = (TextIO.inputAll f) handle e => (TextIO.closeIn f; raise e)
    in
      (TextIO.closeIn f; str)
    end

end
