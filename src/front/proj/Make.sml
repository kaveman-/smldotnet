structure Make :> MAKE =
struct

  val opts = ref 
    ["linkpresimp", "simp1", "funscope", 
     "arity1", "simp2", 
     "eq", "simp3", 
     "mono", "simp4", 
     "units", "simp5", 
     "flat", "simp6",
     "arity2", "tailrec", "simp7", 
     "inline", "funscope", "simp8", "floathoist", "case", "deadargs", "tailrec2", "lastsimp"]

  val listFiles = Controls.add false "env.saveScript"

  fun complete (projname, entities, classnames) =
    let
(*......................................................................*)
(* Merge all modules into one huge term and simplify it.                *)
(*......................................................................*)
      val (e,supply,classnames) = 
        PrintManager.process ("Linking modules", true)
          (fn () => Link.link ((Entity.Str, Id.fromString "_export")::entities,
            classnames))
    in
      PrintManager.process ("Compiling whole program", true)
      (fn () =>
      let

        (* enable lineBeta rewrite iff debug flag is false *)

        val debug = CompileAll.debug 

        val lineBeta = Simplify.lineBeta
        
        val savedLineBeta = Controls.get(lineBeta)

        val _ = Controls.set(lineBeta,not(Controls.get debug))

        val _ = MILTy.init{debug=Controls.get debug}
        
        (* Apply a succession of transformations *)
        val (e,supply) = Opts.apply (!opts) Var.Map.empty (e,supply)

        (* Closure-convert the result *)
        val convresult = PrintManager.process ("Closure converting", false)
          (fn () => ClosConv.conv e)
        
        (* restore lineBeta flag *)
        val _ = Controls.set(lineBeta,savedLineBeta)

    in
      (*@TODO: check that program isn't just a top-level raise *)
      if true (* CheckExn.check (#clinit convresult) *)
      then
        PrintManager.process ("Generating code", false)
          (fn () => CompileAll.compile (projname,classnames) convresult)
      else 
      (PrintManager.println "Error: exception always raised at top-level.";
      false)
    end)
    end

  val exports = ref ([] : (string list * string) list)
  val mainclass = ref (NONE : string option)

  fun getDeps () = 
  case !exports of
    es as ((rootstrid::_,_)::_) =>
    (case SyntaxDep.sync (map (hd o #1) es) of
      NONE => 
      NONE

    | SOME info =>
      SOME (rootstrid, info))

  | _ => 
    (PrintManager.println "Exports not set."; NONE)

  fun make clean = 
  (let
       fun finish success = 
       (
         Debug.finish (); 
         if not success then TargetManager.abort () 
         else PrintManager.println("Compilation succeeded: output in " ^
                                   #out (valOf(TargetManager.getInfo())));
         success
       )
       val timer = Timer.startCPUTimer ()
       val _ = mainclass := NONE
   in
    Commands.mustQuit := true;
    Controls.reset ();
    Debug.start ();
    Debug.print (Welcome.welcomeMessage ());
    case getDeps () of
      NONE => (BuildManager.serialize NONE; (*crusso: persist paths to disk for vs *)
               finish false)
    | depsOpt as 
      SOME (rootstrid, info as { order, ... }) =>
     (BuildManager.serialize (SOME info); (*crusso: persist paths to disk for vs *)
      case SepComp.make info of
        SepComp.Failure => finish false
      | result =>
        case ProcessExports.process (!exports) of
          NONE => 
          finish false
        | SOME (tynames, mainclassopt) =>
          let           
            val _ = mainclass := mainclassopt
            val _ = TargetManager.setDefaultName rootstrid
            val targetExists = case TargetManager.getInfo() of
                                NONE => false (* should be impossible *)
                              | SOME{out,...}=> OS.FileSys.access(out,[])
            val done = case result of
                           SepComp.NoChange =>
                           if targetExists andalso not(clean)
                           then true 
                           else complete(rootstrid,order,tynames)
                         | success => complete (rootstrid, order, tynames)
          in            
            if Controls.get PrintManager.showTime 
            then (PrintManager.printTime "\nTotal compilation time: " timer)
            else ();
            if Controls.get listFiles 
            then 
            let
              (* Only save non-Basis files by testing the number of pervasives structures *)
              (* dragged in (yuck) *)
              val fileFor = (fn SOME ((s,_),[_,_]) => SOME s | _ => NONE) o 
                            SourceManager.fileRefFor
              val f = TextIO.openOut ("files")
              val _ = TextIO.output (f, 
                Pretty.simpleVec "\n" 
                  (fn s => "use \"" ^ CharVector.map (fn #"\\" => #"/" | c => c) s ^ "\";")
                (rev (List.mapPartial fileFor order)))
              val _ = TextIO.closeOut f
            in
              ()
            end 
            else ();
            finish done
          end)
   end) handle e => (Debug.finish(); raise e)

  fun getMainClass () = !mainclass

(*----------------------------------------------------------------------*)
(* Do syntatic checks on list of exports of form                        *)
(*   strid[=classname], ...                                             *)
(* ensuring that:                                                       *)
(*   (1) classname's are distinct                                       *)
(*@FUTURE: generalise strid to longstrid                                *)
(*----------------------------------------------------------------------*)
fun parseExports args =
let
  fun gather [] = SOME []
    | gather ((classty,classname)::rest) =
      (case String.fields (fn c => c = #".") classty of
           longid as [strid] => 
               (case gather rest of
                    NONE => NONE
                  | SOME result => 
                        SOME ((longid,getOpt(classname,strid))::result))
         | _ => NONE (*@TODO: support |longid|>1 *))

  fun removeDupLongids ([], result) = rev result
    | removeDupLongids ((longid,name)::rest, result) = 
      if List.exists (fn (longid',_) => longid=longid') result
      then removeDupLongids (rest, result)
      else removeDupLongids (rest, (longid,name)::result)
in
  case gather args of
    NONE => NONE
  | SOME [] => 
        (PrintManager.println("exports cleared");
         SOME [])
  | SOME newexports =>     (* add exports *)
    let
      val exports = removeDupLongids (newexports @ !exports, [])
    in
      case Dups.duplicateStrings (map #2 exports) of
        [] => SOME exports
      | ds =>
        (PrintManager.println("Duplicate class names: " ^ Pretty.simpleVec "," 
         Gen.identity ds); NONE)
    end
end

fun setExports args =
 (case parseExports args of
    NONE => OS.Process.failure
  | SOME result =>
   (exports := result; OS.Process.success)
 )

val _ = Commands.add "export"
{
  act = fn root => setExports,
  query = fn () => "export:" ^ Pretty.simpleVec "," (fn (longid,classname) =>
          Longid.toString (map Id.fromString longid) ^ "=" ^ classname) 
          (!exports),
  syntax = "<strid>[=<classname>],...,<strid>[=<classname>]",
  help = "export <strid>[=<classname>],...,<strid>[=classname>]\n\
         \  extend list of exported structures with their exported class names\n\
         \export \n  Clear export list\n\
         \export? \n  Query export list"

}

fun makeAction clean = if make clean then OS.Process.success else OS.Process.failure

val _ = Commands.add "make"
{
  act = fn root => 
        fn [] => makeAction false
         | _ => 
  (PrintManager.println ("make: unexpected parameters"); OS.Process.failure),
  query = fn () => "",
  syntax = "",
  help = "make\n\
         \  Make target, recompiling and relinking if necessary"
}

val _ = Commands.add "remake"
{
  act = fn root => 
        fn [] => makeAction true
         | _ => 
  (PrintManager.println ("remake: unexpected parameters"); OS.Process.failure),
  query = fn () => "",
  syntax = "",
  help = "remake\n\
         \  remake target, recompiling if necessary but always relinking"
}

end
