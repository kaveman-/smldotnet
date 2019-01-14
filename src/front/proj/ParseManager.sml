(*======================================================================*)
(* Parse manager: obtain source files from SourceManager, parse them    *)
(* and check syntactic restrictions.                                    *) 
(*======================================================================*)
structure ParseManager :> PARSEMANAGER =
struct

(*----------------------------------------------------------------------*)
(* Parse tree cache: a map from file names to triples of the form       *)
(*   (parse tree, location map, timestamp of parsed file).		*)
(*									*)
(* @FUTURE: to trade space against time, only keep parse trees 		*)
(* of most recently accessed files.			              	*)
(*----------------------------------------------------------------------*)
type Cache = (Syntax.Dec * SourceMap.sourcemap * Time.time) StringMap.map
val cache = ref (StringMap.empty : Cache)

datatype Result =
  NotFound
| Fail
| Success of Syntax.Dec * SourceMap.sourcemap

(*----------------------------------------------------------------------*)
(* Kill the cache							*)
(*----------------------------------------------------------------------*)
fun reset () = cache := StringMap.empty

(*----------------------------------------------------------------------*)
(* Do the parse and update the cache.					*)
(*----------------------------------------------------------------------*)
fun doparse (fileref as (file,time)) =
  let
    val s = FileOps.readText file
  in
    PrintManager.process ("Parsing " ^ file, true)
    (fn () =>
      let
        val { AST, errors, sourcemap } = Parse.parse_string (file, s, false)
      in
        ErrorManager.printErrors (sourcemap, errors);
        if List.exists Error.isSerious errors
        then Fail
        else         
          PrintManager.process ("Checking", false)
          (fn () =>
          let
            val { AST, errors } = 
            SyntaxCheck.check { AST = valOf AST, sourcemap = sourcemap }
          in
            ErrorManager.printErrors (sourcemap,errors); 
            if List.exists Error.isSerious errors 
            then Fail
            else
            let
              val topbind =  () (* if isbasis then AST else addBasis AST *)
            in
              cache := StringMap.insert(!cache, file, (AST, sourcemap, time));
              Success (AST, sourcemap)
            end
          end)
      end)
    end

(*----------------------------------------------------------------------*)
(* Return an up-to-date AST for the fileref specified. Reparse if	*)
(* necessary.								*)
(*----------------------------------------------------------------------*)
fun parse (fileref as (file,time))  =
  case StringMap.find(!cache, file) of

    (* If it's not in the cache then we must re-parse *)
    NONE =>
    doparse fileref    

  | (* If it's in the cache then only re-parse if the timestamps differ *)
    SOME (dec, sourcemap, cachetime) =>
    if Time.<(time, cachetime) orelse Time.>(time, cachetime)
    then doparse fileref
    else Success (dec, sourcemap)
 

end (* of struct *)

