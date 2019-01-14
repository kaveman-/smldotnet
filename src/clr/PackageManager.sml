(*======================================================================*)
(* CLR package manager.  						*)
(* See signature for more details.                                      *)
(*======================================================================*)
structure PackageManager :> 
            PACKAGEMANAGER 
              where type PackageInfo = {name: string,
					version: string option,
                                        publickeytoken: string option
					}
= 
struct

fun quote s = "\""^s^"\""

val showClassDecode = Controls.add false "env.showClassDecode"

type Assembly = {assemblyFile:string, (* canonical filename *)
		 name:string,         (* dotted name of the assembly *)
		 stamp:string,        (* time stamp *)
		 publickeytoken:string option, 
		 version:string option}

(* environment var LIB (shared with C#) *)
val lib = ref ([]: string list)

(* optional libpath *)
val libPath = ref ([]: string list)

(* referenced assemblies *)
val references = ref ([] : Assembly list)

val _ = Commands.add "lib"
{
  act = fn root => fn dirs =>
    (if null dirs then libPath := []
     else libPath := map (FileOps.normalizePath root o #1) dirs @ (!libPath); OS.Process.success),
  query = fn () => Pretty.simpleVec ",\n" Gen.identity (!libPath),
  syntax = "<dir>,...,<dir>",
  help = "lib <dir>,...,<dir>\n  Extend path to search for libraries\n\
         \lib\n  Restore default (look in working directory)\n\
         \lib?\n  Query setting"
}

type ClassDir = 
  { assemblyFile:string, assemblyName:string, instantiable:bool, valuetype:bool, enum:bool, equality:bool, name:string}
datatype Package = Package of
  { classes : ClassDir Symbol.Map.map,
    packages : Package ref Symbol.Map.map 
  }
type Env =
  { classes : ClassDir Symbol.Map.map,
    packages : Package ref Symbol.Map.map 
  }

val emptyPackage = 
  Package 
  { 
    classes = Symbol.Map.empty, 
    packages = Symbol.Map.empty 
  }

val topPackage = 
  ref emptyPackage

(*----------------------------------------------------------------------*)
(* Pretty-print a package						*)
(*----------------------------------------------------------------------*)
fun packageToString p =
let
  fun ps (depth, Package { classes, packages }) =
  Pretty.newline depth ^ "{" ^ 
  (if Symbol.Map.numItems classes <> 0 then
    Pretty.newline (depth+1) ^
    Pretty.simpleVec (Pretty.newline (depth+1))
      (fn (c,_) => "class " ^ Id.toString c)
      (Symbol.Map.listItemsi classes) 
   else "") ^
  (if Symbol.Map.numItems packages <> 0 then
    Pretty.newline (depth+1) ^
    Pretty.simpleVec (Pretty.newline (depth+1)) 
      (fn (n,p) => "package " ^ Id.toString n
      ^ " = " ^ ps (depth+1, !p)) (Symbol.Map.listItemsi packages)
   else "") ^
  Pretty.newline depth ^ "}"
in
  ps (0, p)
end
  
(*----------------------------------------------------------------------*)
(* Commands for listing all classes in a module and extracting metadata *)
(*----------------------------------------------------------------------*)
val clslistCommand = "clslist.exe"
val getmetaCommand = "getmeta.exe"

fun encodedFile classname =
let
  val tmpdir = RuntimeEnv.getCompilerToolDir()
in
  OS.Path.joinDirFile { dir = tmpdir, file = classname ^ ".enc" }
end

fun encodedAssembly assemblyFile =
let
  val tmpdir = RuntimeEnv.getCompilerToolDir()
  val filename = case OS.Path.splitDirFile (assemblyFile) of
                     {dir,file} => 
			 case OS.Path.splitBaseExt file of
			     {base,ext} => base
in
  OS.Path.joinDirFile { dir = tmpdir, file = filename ^ ".lst" }
end


(*----------------------------------------------------------------------*)
(* Add a new (empty) package if is not present already			*)
(*----------------------------------------------------------------------*)
fun addPackage (r as ref (Package { packages, classes }), p) =
  case Symbol.Map.find(packages, Id.fromString p) of
    NONE =>
    let
      val r' = ref (Package 
        { classes = Symbol.Map.empty, packages = Symbol.Map.empty })
    in
      r := Package 
        { classes = classes, 
          packages = Symbol.Map.insert(packages, Id.fromString p, r') };
     r'
    end

  | SOME r =>
    r


(*----------------------------------------------------------------------*)
(* Add a new class 							*)
(*----------------------------------------------------------------------*)
fun addClass (r as ref (Package { packages, classes }),c, info) =
    r := 
    Package { classes = Symbol.Map.insert(classes, Id.fromString c, info),
              packages = packages }

fun addEntry (name, info) =
let
  
  val items = String.fields (fn x => x = #"." orelse x = #"+") name

  fun traverse (r, [classname]) = 
      addClass (r, classname, info)
    | traverse (r, p::ps) =
      traverse (addPackage(r, p), ps)
in
  traverse (topPackage, items)
end

(* strip trailing \n from a string *)
fun strip_nl s = String.substring(s,0,String.size(s) - 1)

fun searchPath () =
    OS.FileSys.getDir()::RuntimeEnv.getSysDir():: !libPath @ (!lib);

fun fileExists filename = 
    ((* PrintManager.print ("\n Probing " ^filename); *)
     OS.FileSys.access(PathConv.toExternal filename,[OS.FileSys.A_READ])) handle _ => false

fun fileExistsInDir file dir =  let val filename = OS.Path.joinDirFile{dir=dir,file=file}
				in
				    fileExists filename
				end

fun findReference assemblyRef = 
    if OS.Path.isAbsolute(assemblyRef) 
    then (if fileExists(assemblyRef) 
	  then SOME (OS.Path.mkCanonical assemblyRef)
	  else NONE)
    else case OS.Path.splitDirFile assemblyRef of 
	  {dir="",file} => 
	      let val file = 
                 case OS.Path.splitBaseExt file of
		   {base=base,ext= SOME "dll"} => file
		 | {base=base,ext= SOME "exe"} => file
		 | {base=base,ext= _} => OS.Path.joinBaseExt{base=file,ext=SOME "dll"}
              in  case List.find (fileExistsInDir file) (searchPath()) of
		    SOME dir => SOME(OS.Path.mkCanonical(OS.Path.joinDirFile{dir=dir,file=file}))
		  | NONE => NONE
	      end
	| {dir=_,file} => findReference(OS.Path.mkAbsolute{path=assemblyRef, relativeTo=OS.FileSys.getDir()})

(*----------------------------------------------------------------------*)
(* Set a path: first close the existing archives, then open new ones.   *)
(* Finally recalculate the whole package map.				*)
(*----------------------------------------------------------------------*)
fun setPath (r,stampedAssemblyFiles) =
let
  val acc = ref []
  fun addComClasses (assemblyFile,stamp)=
  let

    val filename = encodedAssembly assemblyFile
    fun readField (optional,expectedDesc,f) = 
      case TextIO.inputLine f of
        NONE => (PrintManager.println ("Error: descriptor line missing when decoding assembly file " ^ filename); NONE)
      | SOME d =>
        if strip_nl d <> expectedDesc 
        then (PrintManager.println ("Error: descriptor line incorrect (expected " ^ expectedDesc ^ ", got " ^ strip_nl d ^ ") when decoding assembly file " ^ filename); NONE)
        else case TextIO.inputLine f of
          NONE => (PrintManager.println ("Error: line missing when decoding assembly file " ^ filename); NONE)
        | SOME "null\n" => (if optional then NONE else (PrintManager.println ("Error: illegal null for " ^ expectedDesc ^ "  when decoding assembly file " ^ filename); NONE))
        | SOME s => SOME (strip_nl s)

    fun nofile () =
            if RuntimeEnv.runHelper { program = RuntimeEnv.getClslistFileName(), args =
   				      quote (PathConv.toExternal assemblyFile) ^ " " ^ stamp,
				      out = PathConv.toExternal filename } =
		OS.Process.success 
            then dodecode true
            else NONE

    (*@BUG: returning unit option value instead of bool option compiles incorrectly under sml.net *)
    and dodecode second =
	if OS.FileSys.access(filename, []) 
	    then
		let
		    val f = TextIO.openIn filename
		in
		    case decodeAssembly(f) of
			NONE => 
			    (TextIO.closeIn f; if second then NONE else nofile ())
		      | SOME {assemblyFile=assemblyFile',stamp=stamp',continue} => 
			    if assemblyFile=(PathConv.toInternal assemblyFile') andalso 
	                       stamp = stamp'
			       then (continue();TextIO.closeIn f; SOME true)
			    else (TextIO.closeIn f; if second then NONE else nofile ())
		end
	else nofile ()
    and decodeAssembly f =
    let
      val s = TextIO.inputLine f
    in
      if s<>SOME "clslist 9\n"
      then NONE
      else 
      let val assemblyFile' = valOf(readField(false,"assemblyFile",f))
	  val stamp' = valOf(readField(false,"stamp",f))
	  fun continue () =
	  let 
	  val name = valOf(readField(false,"name",f))
	  val publickeytoken = readField(true,"publickeytoken",f)
          val version = readField(true,"version",f)
	  fun loop () =
            case TextIO.inputLine f of
              NONE => ()
            | SOME s =>
              let
                val toks = String.tokens Char.isSpace s
	      in
		  case toks of
		      [] => ()
		    | [s,qs] => 
                      (addEntry (s, {assemblyFile = assemblyFile,
				     assemblyName = name,
				     instantiable = String.sub(qs,0) = #"C", 
                                     equality = String.sub(qs,1) = #"=",
                                     enum = String.sub(qs,2) = #"E",
				     valuetype = String.sub(qs,3) = #"V",
				     name = s}); loop ())
		    | [s] =>
		      (addEntry (s, {assemblyFile = assemblyFile, 
				     assemblyName = name,
				     instantiable = true,
				     valuetype = false, 
				     enum = false, 
				     equality = false,
				     name = s}); loop ())
		    | _ => loop ()
	      end
	  in
	      acc:={assemblyFile=assemblyFile,name=name,publickeytoken=publickeytoken,version=version,stamp=stamp}::(!acc);
	      loop ()
	  end;	
      in
	 SOME  {assemblyFile=assemblyFile',
		stamp=stamp',
		continue = continue}
      end
    end
  in
       case dodecode false of 
	NONE => Debug.fail "PackageManager: can't decode referenced assembly"
      | SOME true => ()
  end
   
in
  List.app addComClasses stampedAssemblyFiles;
  r := List.rev (!acc)
end      


fun setLib paths = lib := paths;
fun setLibPath paths = libPath:=paths
fun getLibPath () = !libPath

fun initReferences stampedAssemblyFiles = 
	setPath (references,stampedAssemblyFiles)

fun setReferences assemblyRefs = 
	let val stampedAssemblyFiles = 
	    List.foldr (fn (assemblyRef,acc) =>   
	                case findReference (assemblyRef) of
			   NONE => (PrintManager.println ("Error: could not locate assembly: " ^ assemblyRef);
	                            acc)
	                 | SOME assemblyFile => 
	                            (assemblyFile,Time.toString(OS.FileSys.modTime assemblyFile))
	                            ::acc) [] assemblyRefs
	in	
		setPath(references,
	        	stampedAssemblyFiles)
	end	

fun getReferences() = map #assemblyFile (!references)

fun getStampedReferences() = 
    map (fn {assemblyFile,stamp,...} => (assemblyFile,stamp)) (!references)

val _ = Commands.addShort ("reference", "r")
{
  act = fn root => 
  fn dirs =>  if null dirs 
	      then (setReferences []; 
		    PrintManager.println "References cleared";
		    PrintManager.println "Warning: all projects *require* at least \"reference mscorlib.dll, System.dll\"";
		    OS.Process.success)
              else (setReferences (map #1 dirs @ getReferences()); 
		    OS.Process.success),(*@TODO: don't ignore errors *)

  query = fn () => Pretty.simpleVec ",\n" #assemblyFile (!references),
  syntax = "<file>,...,<file>",
  help = "reference <file>,...,<file>\n\
         \  Extend list of referenced assemblies\n\
         \reference?\n\
         \  Query list of referenced assemblies\n\
         \reference\n\
         \  Reset list of referenced assemblies\n"
}


fun longidToClass longid =
let
  val jid::jlongid = map (UString.toMLString o Symbol.toUString) longid
  val jstrs = 
    jid :: foldr (fn (jid,jstrs) => "/" :: jid :: jstrs) [] jlongid
  val jclassname = String.concat jstrs
in
  jclassname ^ ".class"
end

fun getAssemblyStamp assemblyFile = 
   let val assemblyOpt = 
   	 List.find (fn assembly => #assemblyFile(assembly) = assemblyFile) (!references)
   in case assemblyOpt of
	  SOME {name,publickeytoken,version,assemblyFile,stamp} => 
             stamp
        | NONE => (Debug.fail ("PackageManager: can't find stamp of assembly " ^ assemblyFile))
   end


(*----------------------------------------------------------------------*)
(* Decode a class							*)
(*----------------------------------------------------------------------*)
fun getClass longid =
let
(*  val name = longidToClass longid *)

  fun traverse (ref (Package { classes, packages }), [id]) =
      (
      case Symbol.Map.find(classes, id) of
        NONE => NONE
      | SOME { assemblyFile, assemblyName, name, ... } =>
        let
	  val stamp = getAssemblyStamp assemblyFile
          val filename = encodedFile name
          
          fun nofile () =
            if RuntimeEnv.runHelper { program=RuntimeEnv.getGetmetaFileName(), args = quote name ^ " " ^
				      quote (PathConv.toExternal assemblyFile) ^
		                      " " ^ stamp, out = PathConv.toExternal filename } = 
              OS.Process.success 
            then dodecode true
            else NONE

          and dodecode second =
          if OS.FileSys.access(filename, []) 
          then
            case Decoder.decodeClass (longid, filename) of
              NONE => 
	      (if second then NONE else nofile ())
           | SOME {assemblyFile=assemblyFile',stamp=stamp',info} => 
	      if assemblyFile=(PathConv.toInternal assemblyFile') andalso stamp = stamp' 
		  then SOME info
	      else (if second then NONE else nofile ())
          else nofile ()
        in
          PrintManager.process 
            ("Decoding class " ^ name, Controls.get showClassDecode) 
            (fn () => dodecode false)
        end
      )

    | traverse (ref (Package { classes, packages }), id::ids) =
      case Symbol.Map.find(packages, id) of
        SOME r' => traverse(r', ids)
      | NONE => NONE
in
  traverse (topPackage, longid)
end

(*@TODO: factor traversal! *)

fun hasConstructor longid =
let
  fun traverse (ref (Package { classes, packages }), [id]) =
      Option.map #instantiable (Symbol.Map.find(classes,id))

    | traverse (ref (Package { classes, packages }), id::ids) =
      case Symbol.Map.find(packages, id) of
        SOME r' => traverse(r', ids)
      | NONE => NONE
in
  traverse (topPackage, longid)
end

fun isValueType longid =
let
  fun traverse (ref (Package { classes, packages }), [id]) =
      Option.map #valuetype (Symbol.Map.find(classes,id))

    | traverse (ref (Package { classes, packages }), id::ids) =
      case Symbol.Map.find(packages, id) of
        SOME r' => traverse(r', ids)
      | NONE => NONE
in
  traverse (topPackage, longid)
end

fun isEnum longid =
let
  fun traverse (ref (Package { classes, packages }), [id]) =
      Option.map #enum (Symbol.Map.find(classes,id))

    | traverse (ref (Package { classes, packages }), id::ids) =
      case Symbol.Map.find(packages, id) of
        SOME r' => traverse(r', ids)
      | NONE => NONE
in
  traverse (topPackage, longid)
end

fun getDepth longid =
let
  fun traverse (ref (Package { classes, packages }), [id]) =
      Option.map ((Substring.foldl(fn (#"+",n) => n+1  
				    | (_,n) => n) 0)
		  o
		  Substring.all
		  o #name)
      (Symbol.Map.find(classes,id))
    | traverse (ref (Package { classes, packages }), id::ids) =
      case Symbol.Map.find(packages, id) of
        SOME r' => traverse(r', ids)
      | NONE => NONE
in
  traverse (topPackage, longid)
end


fun hasEquality longid =
let
  fun traverse (ref (Package { classes, packages }), [id]) =
      Option.map #equality (Symbol.Map.find(classes,id))

    | traverse (ref (Package { classes, packages }), id::ids) =
      case Symbol.Map.find(packages, id) of
        SOME r' => traverse(r', ids)
      | NONE => NONE
in
  traverse (topPackage, longid)
end

fun getAssembly longid =
let
  fun traverse (ref (Package { classes, packages }), [id]) =
      Option.map (Id.fromString o #assemblyName) (Symbol.Map.find(classes,id))

    | traverse (ref (Package { classes, packages }), id::ids) =
      case Symbol.Map.find(packages, id) of
        SOME r' => traverse(r', ids)
      | NONE => NONE
in
  traverse (topPackage, longid)
end

fun getOrigin longid =
let
  fun traverse (ref (Package { classes, packages }), [id]) =
      Option.map #assemblyName (Symbol.Map.find(classes,id))

    | traverse (ref (Package { classes, packages }), id::ids) =
      case Symbol.Map.find(packages, id) of
        SOME r' => traverse(r', ids)
      | NONE => NONE
in
  traverse (topPackage, longid)
end

(*----------------------------------------------------------------------*)
(* CLR specific type of package (ie. assembly) info                     *)
(*----------------------------------------------------------------------*)

(*@TODO: consider adding full public key (ilasm .publickey) *)
type PackageInfo = {name: string,
		    publickeytoken: string option,
	            version: string option}
	
fun getPackageInfo assemblyFile = 
   let val assemblyOpt = 
   	 List.find (fn assembly => #assemblyFile(assembly) = assemblyFile) (!references)
   in case assemblyOpt of
	  SOME {name,publickeytoken,version,assemblyFile,stamp} => 
             SOME {name=name,publickeytoken=publickeytoken,version=version}
        | NONE => NONE
   end



fun getTop () = !topPackage

end

