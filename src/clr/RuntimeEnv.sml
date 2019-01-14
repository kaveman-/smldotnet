(*======================================================================*)
(* Info about the runtime environment					*)
(* See signature for more details.                                      *)
(*======================================================================*)
structure RuntimeEnv :> RUNTIMEENV
= 
struct

val getsysdirCommand = "getsysdir.exe"

val frameworkVersion = ref ""
val compilerDir = ref ""
val compilerBinDir = ref ""
val compilerToolDir = ref "" (* compilerBinDir ^ "\" ^ frameworkVersion *)
val runtimeSysDir = ref ""
val runtimeVersion = ref ""
val setupWasRun = ref false

val getmetaFileName = ref ""
val clslistFileName = ref ""
val ilasmFileName = ref ""
val peverifyFileName = ref (NONE : string option)
val compilerIlasm = ref false

fun failWith message = (PrintManager.println message; false)

(* If set, display a message when helper executables are run *)
val showHelperExecs = Controls.add false "env.showHelpers"

fun quote s = "\"" ^s^ "\""

(* Execute a managed program with specified arguments *)
fun run { program, args } =
let
  (* If defined, SMLNETRUN is used to execute managed code *)
  val prefix = 
    case OS.Process.getEnv "SMLNETRUN" of
      NONE => ""
    | SOME s => s ^ " "
  val command = prefix ^ program ^ " " ^ args
  val result = OS.Process.system command
in
  if result = OS.Process.success 
  then ()
  else PrintManager.println ("Error occurred while running " ^ command ^ "\n");
  result
end

(* Execute a managed helper program *)
fun runHelper { program, args, out } =
let
  val args = args ^ " " ^ quote out
in
  if Controls.get showHelperExecs 
  then PrintManager.println ("[" ^ program ^ " " ^ args ^ "]") else ();
  run { program=program, args=args }
end

fun trySet (r : string ref, s : string) =
  if OS.FileSys.access(s, [])
  then (r := s; true)
  else failWith ("Cannot find " ^ s)

fun tryGet r =
  if !setupWasRun then !r
  else raise Fail "RuntimeEnv.tryGet: setup has not been run successfully"


fun writeFile(dst,s) = 
    let val os = TextIO.openOut dst 
    in
        TextIO.output(os,s);
        TextIO.closeOut(os);
        true
    end handle _ => (PrintManager.println("Failed to write file " ^ dst); false)
        
fun copyFile(src,dst) = 
    let val is = BinIO.openIn src
        val v = BinIO.inputAll(is)
        val os = BinIO.openOut(dst)
    in	
        BinIO.output(os,v);
        BinIO.closeIn(is);
        BinIO.closeOut(os);
        true
    end
       handle _ => (PrintManager.println("Failed to copy file " ^ src ^ " to " ^ dst); false)



fun setupRuntimeInfo arg =
  case arg of
    SOME {SMLNETPATH,FrameworkDir,FrameworkVersion} =>
    (frameworkVersion := FrameworkVersion;
     runtimeVersion := String.substring(FrameworkVersion,1,String.size(FrameworkVersion) - 1);
     trySet(compilerDir, PathConv.toInternal SMLNETPATH) andalso
     trySet(compilerBinDir, OS.Path.concat(!compilerDir, "bin")) andalso
     trySet(runtimeSysDir, PathConv.toInternal(OS.Path.concat(FrameworkDir,FrameworkVersion))))
  | NONE =>
  (* First see if we've got an SMLNETPATH setting *)
  case OS.Process.getEnv RuntimeNames.compilerDir of
    NONE => 
    failWith ("Startup script failed to set " ^ RuntimeNames.compilerDir)

  | SOME dir =>
    (
      trySet (compilerDir, PathConv.toInternal dir) andalso
      trySet (compilerBinDir, OS.Path.concat(!compilerDir, "bin")) andalso
      ((* use environment variables FrameworkDir and FrameworkVersion *)
       (case (OS.Process.getEnv RuntimeNames.frameworkDir,
              OS.Process.getEnv RuntimeNames.frameworkVersion) of
          (SOME dir,SOME version) =>
           (runtimeVersion := String.substring(version,1,String.size(version) - 1);
            frameworkVersion := version;
            trySet(runtimeSysDir,PathConv.toInternal(OS.Path.concat(dir,version))))
         | _ => false)
       orelse
       (* use getsysdir *)
       let
         val filename = OS.Path.joinDirFile { dir = !compilerBinDir, file = RuntimeNames.getsysdirCommand^".out" }
       in
        if OS.Process.success <> runHelper { program = OS.Path.joinDirFile { dir = !compilerBinDir, file = RuntimeNames.getsysdirCommand },
                                             args = "", 
                                             out = PathConv.toExternal filename }
        then failWith "Cannot obtain runtime version and system directory"
        else
        let
	  val f = TextIO.openIn filename
        in
          case (TextIO.inputLine f,TextIO.inputLine f) of
            (NONE,_) => (TextIO.closeIn f; failWith "Cannot obtain FrameworkDir")
          | (_,NONE) => (TextIO.closeIn f; failWith "Cannot obtain FrameworkVersion")
          | (SOME first,SOME second) => 
            ( TextIO.closeIn f;
              let (* remove trailing newline *)
                  val dir = String.substring(first,0,String.size(first) - 1)
                  val version = String.substring(second,0,String.size(second) - 1)
              in
                  trySet(runtimeSysDir, PathConv.toInternal (OS.Path.concat(dir,version))) andalso
                  (runtimeVersion := String.substring(version,1,String.size(version) - 1);
                   frameworkVersion := version;
                   true)
              end
            )
       end
       end)
   )
   

(*----------------------------------------------------------------------*)
(* Determine if the SML.NET distribution comes with ilasm that supports *)
(* linespans and multiple source files per method.                      *)
(* If not, use default ilasm, and translate line(span) instructions     *)
(* to simple line instructions, producing                               *)
(* a single dummy source file that contains all the mentioned sources.  *)
(*----------------------------------------------------------------------*)
  fun setupIlasm() =
    let
	val filename1 = OS.Path.joinDirFile { dir = !compilerBinDir, file=RuntimeNames.assemCommand^".exe" }
        val filename2 = OS.Path.joinDirFile { dir = !runtimeSysDir, file=RuntimeNames.assemCommand^".exe"}
        val filename3 = OS.Path.joinDirFile { dir = OS.Path.concat(!runtimeSysDir, OS.Path.concat("sdk","bin")), file=RuntimeNames.assemCommand^".exe"}
    in
      (* For Whidbey, and for pre-Whidbey runtimes without a SML.NET-compiler-shipped ilasm, 
         just use ilasm as picked up from the system directory *)
      if !runtimeVersion >= "2" orelse not (OS.FileSys.access(filename1,[]))
      then
      (
        compilerIlasm := false;
	trySet (ilasmFileName, if OS.FileSys.access(filename2, []) then filename2 else filename3)
      )
     
      (* Otherwise, attempt to pick up the prerelease version from the compiler binary directory *)
      (* But to do this, we need to copy across a config file from the runtime system directory where its ilasm lives *)
      else
      (
        compilerIlasm := true;
        let val tool = OS.Path.joinDirFile{dir = !compilerToolDir, file = RuntimeNames.assemCommand^".exe"}
        in (if OS.FileSys.access(tool,[]) 
            then true
            else
               copyFile(filename1,tool)
               andalso
               writeFile(tool ^ ".config",
                         "<?xml version =\"1.0\"?>\
                         \\n<configuration>\
                         \\n<startup>\
                         \\n<requiredRuntime safemode=\"true\" imageVersion=\"" ^ !frameworkVersion ^ "\" version=\"" ^ !frameworkVersion ^ "\"/>\
                         \\n</startup>\
                         \\n</configuration>")
           )
           ;  
           trySet (ilasmFileName,tool)
        end
      )
  end

  fun setupPeverify() =
  let
    val filename1 = OS.Path.joinDirFile { dir = !runtimeSysDir, file=RuntimeNames.verifyCommand^".exe"}
    val filename2 = OS.Path.joinDirFile { dir = OS.Path.concat(!runtimeSysDir, OS.Path.concat("sdk","bin")), file=RuntimeNames.verifyCommand^".exe"}
    val filename3 = OS.Path.joinDirFile { dir = OS.Path.concat(!runtimeSysDir, OS.Path.concat("..","tools")), file=RuntimeNames.verifyCommand^".exe"}
  in
    if OS.FileSys.access(filename1,[])
    then peverifyFileName := SOME filename1
    else
    if OS.FileSys.access(filename2,[])
    then peverifyFileName := SOME filename2
    else
    if OS.FileSys.access(filename3,[])
    then peverifyFileName := SOME filename3
    else peverifyFileName := NONE;
    true
  end

fun setupToolsInfo() =
  let 
      val toolDir = OS.Path.joinDirFile{dir= !compilerBinDir,file= !frameworkVersion}
      val _ = if OS.FileSys.access(toolDir,[]) andalso OS.FileSys.isDir(toolDir) then () else OS.FileSys.mkDir(toolDir)
      fun compileTool tool = 
          let val out = OS.Path.joinDirFile{dir=toolDir,file=tool^".exe"}
          in  if OS.FileSys.access(out,[]) then true
              else 
                  let
                      val src = OS.Path.joinDirFile{dir=OS.Path.concat(!compilerDir,OS.Path.concat("src","clr")),file=tool^".cs"}
                      val csc = OS.Path.joinDirFile{dir= !runtimeSysDir,file="csc.exe"}
                      val options = if !runtimeVersion < "2" then "" else " /d:WHIDBEY "
                      val cmd = csc ^ options ^" /out:" ^ quote out ^ " " ^ quote src
                  in
                      (OS.Process.system cmd = OS.Process.success 
                       handle _ => (PrintManager.println ("Error occurred while running " ^ cmd ^ "\n");false))
                      andalso
                      writeFile(out^".config",
                               "<?xml version =\"1.0\"?>\
                               \\n<configuration>\
                               \\n<startup>\
                               \\n<supportedRuntime version=\"" ^ !frameworkVersion ^"\" />\
                               \\n</startup>\
                               \\n</configuration>")
                  end
          end
  in  
  trySet(compilerToolDir,toolDir) andalso
  compileTool "getmeta" andalso
  compileTool "clslist" andalso
  trySet(getmetaFileName, 
         OS.Path.joinDirFile { dir = toolDir, file = "getmeta.exe"}) andalso
  trySet(clslistFileName,
         OS.Path.joinDirFile { dir = toolDir, file = "clslist.exe"})
  end handle _ => (PrintManager.println ("Could no set up tools info\n");false)

fun setup arg = setupRuntimeInfo arg andalso setupToolsInfo() andalso setupPeverify() andalso setupIlasm() andalso (setupWasRun := true; true)


fun getSysDir() = tryGet runtimeSysDir
fun getCompilerDir() = tryGet compilerDir
fun getCompilerBinDir() = tryGet compilerBinDir
fun getCompilerToolDir() = tryGet compilerToolDir
fun getIlasmFileName() = tryGet ilasmFileName
fun getPeverifyFileName() = tryGet peverifyFileName
fun getClslistFileName() = tryGet clslistFileName
fun getGetmetaFileName() = tryGet getmetaFileName
fun getVersion() = tryGet runtimeVersion
fun getCompilerIlasm() = tryGet compilerIlasm

end



