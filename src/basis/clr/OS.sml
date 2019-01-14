(*======================================================================*)
(* Operating system functionality					*)
(*======================================================================*)
structure OS :> OS where type Process.status = int =
(*@TODO: restore uses of Prim.unsafeValOf *)
struct

local 
  open General Option List Bool
  open Datatypes (*@HACK *)
  val op= = Prim.=
in

(* Use POSIX names here just for convention's sake *)
datatype syserror = noent | acces | exist | notdir
exception SysErr of (string * syserror option) 

fun errorMsg noent = "No such file or directory"
  | errorMsg acces = "Permission denied"
  | errorMsg exist = "File exists"
  | errorMsg notdir = "Not a directory"

fun syserror "noent" = SOME noent
  | syserror "acces" = SOME acces
  | syserror "exist" = SOME exist
  | syserror "notdir" = SOME notdir
  | syserror _ = NONE

fun errorName noent = "noent"
  | errorName acces = "acces"
  | errorName exist = "exist"
  | errorName notdir = "notdir"

fun syserr code = raise SysErr (errorMsg code, SOME code)
fun unknownsyserr message = raise SysErr (message, NONE)


(*----------------------------------------------------------------------*)
(* Paths								*)
(*----------------------------------------------------------------------*)
structure Path :> OS_PATH = 
struct 

exception Path 
exception InvalidArc

(* These are common to both Unix and Windows *)
val parentArc  = ".."
val currentArc = "."

local 
    val op @ = List.@
    infix 9 sub
    val op sub = String.sub
    val substring = String.extract
    val op ^ = String.^

val slashChar = System.IO.Path.DirectorySeparatorChar 

val slash = String.str slashChar

(* We distinguish DOS from other systems because it's the only one with non-empty
   volumes *)
val isDOS = slashChar = #"\\"
val volslash = slash
fun isslash c = c = slashChar


fun splitabsvolrest s =
    if isDOS andalso Int.>=(String.size s, 2) andalso s sub 1 = #":" 
    then
      if Int.>=(String.size s, 3) andalso isslash (s sub 2) then
          (true, substring(s, 0, SOME 2), substring (s, 3, NONE))
      else
          (false, substring(s, 0, SOME 2), substring (s, 2, NONE))
    else
      if Int.>=(String.size s, 1) andalso isslash (s sub 0) then 
          (true, "", substring(s, 1, NONE)) 
      else 
          (false, "", s)

in

fun isAbsolute p = #1 (splitabsvolrest p)

fun isRelative p = not (isAbsolute p);

fun fromString p = 
    case splitabsvolrest p of

        (false, v,   "") => {isAbs=false, vol = v, arcs = []}

      | (isAbs, v, rest) => {isAbs=isAbs, vol = v, 
                             arcs = String.fields isslash rest};

fun isRoot p = 
    case splitabsvolrest p of
        (true, _, "") => true
      | _             => false

fun getVolume p = #2 (splitabsvolrest p)

(*@NOTE: according to the revised Basis, [validVolume{isAbs=isAbs,vol=""}=not(isAbs)] on DOS (only), 
         but this disagrees with all implementations and the convention (AFAIK) that a path 
         with prefix [\\] is rooted (ie. absolute) in DOS *)
fun validVolume{isAbs, vol} = 
  (if isDOS
   then 
     (String.size vol = 2 andalso Char.isAlpha(vol sub 0) andalso vol sub 1 = #":") 
      orelse vol = ""
   else vol="")

fun toString (path as {isAbs, vol, arcs}) =
    let 
	fun h []        res = res 
          | h (a :: ar) res = h ar (a :: slash :: res)
    in  
        if validVolume{isAbs=isAbs, vol=vol} then 
            case (isAbs, arcs) of

                (false, []         ) => vol
              | (false, "" :: _    ) => raise Path
              | (false, a1 :: arest) => 
                    String.concat (vol :: List.rev (h arest [a1]))

              | (true,  []         ) => vol ^ volslash
              | (true, a1 :: arest ) => 
                    String.concat (List.rev (h arest [a1, volslash, vol])) 
        else
            raise Path
    end;


fun concat (p1, p2) =
    let fun stripslash path = 
            if isslash (path sub (Int.-(String.size path, 1))) then
                substring(path, 0, SOME(Int.-(String.size path, 1)))
            else path
    in
        if isAbsolute p2 then raise Path
        else
            case splitabsvolrest p1 of
                (false, "",   "") => p2
              | (false, v,  path) => v ^ stripslash path ^ slash ^ p2
              | (true,  v,  ""  ) => v ^ volslash ^ p2
              | (true,  v,  path) => v ^ volslash ^ stripslash path ^ slash ^ p2
    end;



fun getParent p =
    let open List
	val {isAbs, vol, arcs} = fromString p 
	fun getpar xs = 
	    rev (case rev xs of
		     []              => [parentArc]
		   | [""]            => if isAbs then [] else [parentArc]
		   | ""   :: revrest => parentArc :: revrest
		   | "."  :: revrest => parentArc :: revrest
		   | ".." :: revrest => parentArc :: parentArc :: revrest
		   | last :: revrest => revrest)
    in
        case getpar arcs of 
            []   => 
                if isAbs then toString {isAbs=true, vol=vol, arcs=[""]}
                else currentArc
          | arcs => toString {isAbs=isAbs, vol=vol, arcs=arcs}
    end;



fun mkCanonical p =
    let val {isAbs, vol, arcs} = fromString p 
        fun mkCanonicalArc arc = if isDOS then String.map Char.toLower arc else arc
        fun backup []          = if isAbs then [] else [parentArc]
          | backup (".."::res) = parentArc :: parentArc :: res
          | backup ( _ :: res) = res
        fun reduce arcs = 
            let fun h []         []  = if isAbs then [""] else [currentArc]
                  | h []         res = res
                  | h (""::ar)   res = h ar res
                  | h ("."::ar)  res = h ar res
                  | h (".."::ar) res = h ar (backup res)
                  | h (a1::ar)   res = h ar (mkCanonicalArc a1 :: res)
            in h arcs [] end
    in
        toString {isAbs=isAbs, vol=vol, arcs=List.rev (reduce arcs)}
    end;



fun parentize []      = []
  | parentize (_::ar) = parentArc :: parentize ar;

fun mkRelative {path=p1, relativeTo=p2} =
    case (fromString p1, fromString (mkCanonical p2)) of
        (_ ,                {isAbs=false,...}) => raise Path
      | ({isAbs=false,...}, _                ) => p1
      | ({vol=vol1, arcs=arcs1,...}, {vol=vol2, arcs=arcs2, ...}) =>
            let fun h [] [] = ["."]
                  | h a1 [] = a1
                  | h [] a2 = parentize a2
                  | h (a1 as (a11::a1r)) (a2 as (a21::a2r)) =
                    if a11=a21 then h a1r a2r
                    else parentize a2 @ (if arcs1 = [""] then [] else a1)
            in
                if Prim.=(vol1, vol2) 
                then toString {isAbs=false, vol="", arcs=h arcs1 arcs2}
                else raise Path 
            end;


fun mkAbsolute {path=p1, relativeTo=p2} =
    if isRelative p2 then raise Path
    else if isAbsolute p1 then p1
    else mkCanonical(concat(p2, p1));

fun isCanonical p = mkCanonical p = p;

fun joinDirFile {dir, file} = concat(dir, file)

fun splitDirFile p =
    let open List
        val {isAbs, vol, arcs} = fromString p 
    in
        case rev arcs of
            []            => 
                {dir = toString {isAbs=isAbs, vol=vol, arcs=[]}, file = ""  }

          | arcn :: farcs => 
                {dir = toString {isAbs=isAbs, vol=vol, arcs=rev farcs}, 
                 file = arcn}

    end
fun dir s  = #dir (splitDirFile s);
fun file s = #file(splitDirFile s);

fun joinBaseExt {base, ext = NONE}    = base
  | joinBaseExt {base, ext = SOME ex} = base ^ "." ^ ex;

fun splitBaseExt s =
    let val {dir, file} = splitDirFile s
        open Substring 
        val (fst, snd) = splitr (fn #"." => false | _ => true) (all file)
    in 
        if isEmpty snd         (* dot at right end     *) 
           orelse isEmpty fst  (* no dot               *)
           orelse size fst = 1 (* dot at left end only *) 
            then {base = s, ext = NONE}
        else 
            {base = joinDirFile{dir = dir, 
                                file = string (trimr 1 fst)},
             ext = SOME (string snd)}
    end;

fun ext s  = #ext  (splitBaseExt s);
fun base s = #base (splitBaseExt s);

(*@TODO: review *)
fun toUnixPath p = String.map (fn c => if c = slashChar then #"/" else c) p
fun fromUnixPath p = String.map (fn c => if c = #"/" then slashChar else c) p

end

end

(*----------------------------------------------------------------------*)
(* File system								*)
(*----------------------------------------------------------------------*)
structure FileSys :> OS_FILE_SYS = 
struct 

local 
  exception Security = System.Security.SecurityException
  exception IO = System.IO.IOException
  exception FileNotFoundException = System.IO.FileNotFoundException
  exception DirectoryNotFoundException = System.IO.DirectoryNotFoundException
in

(* We use file enumerators for directory streams. In fact, they're more
   general and allow wildcards etc. *)
type dirstream = System.Collections.IEnumerator 

(* Open a new directory stream; convert exceptions appropriately *)
fun openDir (s : string) = 
  let val entries   = 
      (valOf (System.IO.DirectoryInfo(s).#GetFileSystemInfos()))
      :> System.Array
  in 
      valOf (entries.#GetEnumerator())
  end
  handle Security => syserr acces 
       | IO => syserr noent 
       | _ => unknownsyserr "Cannot open directory"


(* Obtain the next file name from the directory stream *)
fun readDir (f : dirstream) =
  if f.#MoveNext()
  then  SOME((* Prim.unsafe*)valOf((valOf(f.#get_Current()):>System.IO.FileSystemInfo).#get_Name()):> string)
  else NONE
  handle _ => syserr noent


(* Rewind the directory stream using an enumerator method *)
fun rewindDir (f : dirstream) = f.#Reset() 

(* Close the stream: a no-op *)
fun closeDir (f : dirstream) = ()

(* Change the current directory, converting exceptions appropriately *)
fun chDir (s:string) =
  System.Environment.set_CurrentDirectory(s) 
  handle Security => syserr acces | IO => syserr noent | _ => unknownsyserr "Cannot change directory"

(* Get the name of the current directory *)
fun getDir () = 
  (*Prim.unsafeValOf*) valOf (System.Environment.get_CurrentDirectory())

(* Create a new directory, converting exceptions appropriately *)
fun mkDir (s:string) = 
  (System.IO.Directory.CreateDirectory(s);())
  handle Security => syserr acces | IO => syserr noent | _ => unknownsyserr "Cannot make directory"

(* Remove a directory, converting exceptions appropriately *)
fun rmDir (s:string) =
  System.IO.Directory.Delete(s)
  handle Security => syserr acces | IO => syserr noent | _ => unknownsyserr "Cannot remove directory"

(* Tests whether s is a directory *)
fun isDir (s:string) = 
  System.IO.Directory.Exists(s)
  handle Security => syserr acces | IO => syserr noent | _ => unknownsyserr "Cannot test directory"

(* Tests whether s is a symbolic link; always false in unexceptional cases *)
fun isLink (s:string) = 
  if System.IO.File.Exists(s) orelse System.IO.Directory.Exists(s)
      then false
  else syserr noent
  handle Security => syserr acces | IO => syserr noent | _ => unknownsyserr "Cannot test link"

fun readLink (s:string) = syserr acces

(*@TODO: fullPath and realPath take record arguments in the most recent (non-web) basis spec *)
(* Convert a relative path into an absolute one *)
fun fullPath (s:string) = 
  if System.IO.Directory.Exists(s) orelse System.IO.File.Exists(s)
      then Path.mkCanonical((*Prim.unsafeValOf*) valOf(System.IO.Path.GetFullPath(s)))
  else syserr noent
  handle Security => syserr acces 
       | IO => syserr noent 
       | _ => unknownsyserr "Cannot determine full path"

fun realPath (s:string) =
  if Path.isAbsolute s
  then fullPath s
  else Path.mkRelative {path=fullPath s, relativeTo=fullPath (getDir ())}

val epochFileTime = PrimUtils_.epoch.#ToFileTime()

fun modTime (s:string) = 
    let
	val info = if System.IO.File.Exists(s) 
		       then System.IO.FileInfo(s) :> System.IO.FileSystemInfo
		   else if System.IO.Directory.Exists(s) 
			    then System.IO.DirectoryInfo(s) :> System.IO.FileSystemInfo
			else raise IO
	val _ = info.#Refresh()
	val ticks = LargeInt.-(info.#get_LastWriteTime().#ToUniversalTime().#ToFileTime(),
			       epochFileTime)
    in
(* 	Time.fromMicroseconds(LargeInt.div(ticks,10:LargeInt.int)) *)
(*@BUG: we use resolution of seconds to agree with NJ (and Moscow?) times *)
	Time.fromSeconds(LargeInt.div(ticks,10000000:LargeInt.int)) 
    end
    handle Security => syserr acces 
	 | FileNotFoundException => syserr noent 
	 | _ => unknownsyserr "Cannot determine file modification time"


(*@TODO: review *)
fun setTime (s:string, topt) = 
let
  val info = if System.IO.File.Exists(s) 
			   then System.IO.FileInfo(s) :> System.IO.FileSystemInfo
		       else if System.IO.Directory.Exists(s) 
				then System.IO.DirectoryInfo(s) :> System.IO.FileSystemInfo
			    else raise IO
  val _ = info.#Refresh()
in
  case topt of NONE => (info.#set_LastWriteTime(System.DateTime.get_Now()))
(*@BUG: we use resolution of seconds to agree with NJ (and Moscow?) times *)
(*       | SOME ticks => (info.#set_LastWriteTime(PrimUtils_.epoch.#AddTicks(Prim.mul(Time.toMicroseconds(ticks),10)))) *)
       | SOME ticks => (info.#set_LastWriteTime(PrimUtils_.epoch.#AddTicks(Prim.mul(Time.toSeconds(ticks),10000000))))
    
end
handle Security => syserr acces 
     | IO => syserr noent 
     | _ => unknownsyserr "Cannot set file modification time"

(*@TODO: review *)
(* directories don't support get_Length *)
fun fileSize (s:string) =
  let
      val info = if System.IO.File.Exists(s) then System.IO.FileInfo(s) 
		 else raise IO
      val _ = info.#Refresh()
  in
      info.#get_Length()
  end
  handle Security => syserr acces 
       | IO => syserr noent 
       | _ => unknownsyserr "Cannot determine file size"


fun remove (s:string) =
  System.IO.File.Delete(s)
  handle Security => syserr acces 
       | FileNotFoundException => syserr noent
       | _ => unknownsyserr "Cannot delete file"

fun rename {old, new} =
  System.IO.Directory.Move(old,new) (*NB: acc. to doc, works for files too *)
  handle Security => syserr acces
       | FileNotFoundException => syserr noent
       | DirectoryNotFoundException => syserr noent
       | _ => unknownsyserr "Cannot rename file or directory"


(*@TODO: tmpName diverges from Basis Spec in actually creating the file *)  
fun tmpName () = 
    Path.concat(Path.currentArc,Path.file(valOf (System.IO.Path.GetTempFileName())))
    handle Option => unknownsyserr "Cannot create tmp file"

datatype access_mode
       = A_READ
       | A_WRITE
       | A_EXEC

local (*@TODO: Review*)
    (* conversions between CLI access permissions and words and access_mode *)
    structure Permissions = System.Security.Permissions
    fun toWord (Permissions.FileIOPermissionAccess i) = Word.fromInt(i)
    fun fromWord w = Permissions.FileIOPermissionAccess (Word.toInt w) 
    fun fromMode A_EXEC = 0w0 (*@TODO: what should this mean on the CLI, if anything? *)
      | fromMode A_READ = toWord(Permissions.FileIOPermissionAccess.Read)
      | fromMode A_WRITE  = toWord(Permissions.FileIOPermissionAccess.Write)
in
fun access (s:string, modes) =  
let
  val (exists,isDir) = if System.IO.File.Exists(s) 
			   then (true,false) 
		       else if System.IO.Directory.Exists(s) 
				then (true,true)
			    else (false,false)
  fun test [] = true
  |   test modes = 
      let
	  val f = if isDir 
		  then System.IO.DirectoryInfo(s):>System.IO.FileSystemInfo
		  else System.IO.FileInfo(s):>System.IO.FileSystemInfo 
    	  val fullname = valOf(f.#get_FullName())
          val access = fromWord(List.foldl (fn (mode,w) => Word.orb(w,fromMode(mode))) 0w0 modes)
          val p = System.Security.Permissions.FileIOPermission(access,fullname)
      in
          (p.#Demand();true handle _ => false)
      end 
in
  exists andalso (test modes)
end
end

(*@TODO: improve *)
type file_id = string (* the canonical full path to the file or directory *)

(*@TODO: review *)
fun fileId (s:string) = 
let
  val info = if System.IO.File.Exists(s) 
		 then System.IO.FileInfo(s) :> System.IO.FileSystemInfo
	     else if System.IO.Directory.Exists(s) 
		      then System.IO.DirectoryInfo(s) :> System.IO.FileSystemInfo
		  else raise IO
  val _ = info.#Refresh()
in
  Path.mkCanonical(valOf(info.#get_FullName()))
end
handle Security => syserr acces 
     | IO => syserr noent 
     | _ => unknownsyserr "Cannot determine file id"


fun hash (fid:file_id) = Word.fromInt (fid.#GetHashCode())

val compare = String.compare

end (* of local open *)

end


(*----------------------------------------------------------------------*)
(* Processes								*)
(*----------------------------------------------------------------------*)
structure Process :> OS_PROCESS where type status = int  =
struct

  val exitActions = Prim.ref ([] : (unit -> unit) list)
  val inExit = Prim.ref false

  type status = int
  val success = 0
  val failure = 1

  exception WE = System.ComponentModel.Win32Exception
  fun system (command:string) = 
	   let 	
	       val (cmd,args) = let val (sub1,sub2) = Substring.splitl (not o Char.isSpace) (Substring.all command)
				in (Substring.string sub1,Substring.string sub2)
				end
	       val p = System.Diagnostics.Process()
	       val psi = System.Diagnostics.ProcessStartInfo()
	       val () = psi.#set_FileName(cmd)
	       val () = psi.#set_Arguments(args)
(*@TODO: these two not available in ROTOR *)
               val () = psi.#set_CreateNoWindow(true) 
               val () = psi.#set_UseShellExecute(false) 
(*               val () = psi.#set_RedirectStandardOutput(true) *)
	       val () = p.#set_StartInfo(psi)
	   in
	       p.#Start() handle WE => raise SysErr(String.^("not found: ", cmd), NONE);
	       p.#WaitForExit();
	       p.#Refresh();
               let val status = p.#get_ExitCode() 
	       in
		   p.#Dispose(); (* YUK! *)
		   0 (* status *)
	       end
	   end


  fun loop () = loop ()

  fun atExit f = 
    if !inExit then ()
    else exitActions := f :: !exitActions

  fun terminate status = 
    (System.Environment.Exit(status); loop ())

  fun exit status = 
    if !inExit then raise General.Fail "exit entered"
    else
    (
      inExit := true;
      List.app (fn f => (f ()) handle _ => ()) (!exitActions); 
      terminate status
    )

  fun getEnv key = System.Environment.GetEnvironmentVariable(key:string)


end (* of local open *)

end (* of struct *)

structure IO = struct type iodesc = unit end

end
