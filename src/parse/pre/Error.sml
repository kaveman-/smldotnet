(*======================================================================*)
(* General error handling stuff						*)
(*======================================================================*)
structure Error  (*@TODO:restore  :> ERROR *) = 
struct

datatype Degree = 
  Warning 
| Serious

datatype Error = 
  Error of Degree * Syntax.Location * string

fun error   (loc, message) = Error(Serious, loc, message)
fun warning (loc, message) = Error(Warning, loc, message)

fun append (Error(degree,loc,str), str') = Error(degree,loc,str ^ str')

fun isSerious (Error (Serious,_,_)) = true
  | isSerious _ = false

(*----------------------------------------------------------------------*)
(* If there are no errors or warnings, do nothing, otherwise print out  *)
(* the list in location order.                                          *)
(*----------------------------------------------------------------------*)

(*@TODO: complete the clr case *)
fun print(prln:string->unit,s:SourceMap.sourcemap,errors:Error list) = 
let
   fun print_fn(Error(degree,loc,mess))=
(*
      case RuntimeNames.runtime of 
	  "clr" =>
*)
	      let val {line,col} = SourceMap.decode(s,#left loc)
		  val col = col-1
	      in
		  prln(String.concat[case (* OS.Path.file *)(SourceMap.fileName(s)) of "" => "current" | s => s,
				     "(",
				     Int.toString(line),
				     ",", 
  				     Int.toString(col),
				     "): ",
				     (case degree of Warning => "warning : " | Serious => "error : "),
				     mess
				     ])
	      end
(*
       | _ => prln(String.concat[
				     (case degree of Warning => "Warning " | Serious => "Error "),
					  "at ",
					  PrintLoc.location2string(s,loc),
					  ":",
					  mess
					  ])
*)
	      
   (* Sort the errors by left location.  We delete all messages
      before the first ERROR in a list. *)
   val (_,numbered_errors)=
      List.foldl
         (fn(e,(n,sf)) =>
            (n+1,(e,n)::sf))
         (0,[])
         errors
   
   val sorted_errors=
      ListMergeSort.sort
         (fn((Error(_,x,_),nx),(Error(_,y,_),ny)) =>
            (case Int.compare(#left x,#left y) of
               LESS => false
            |  GREATER => true
            |  EQUAL => (nx<ny)
            ))
            numbered_errors
   
   val final_errors=
   let
      fun drop_lx(l,[])=[]
      |   drop_lx(l,list as ((Error(_,x,_),_)::rest))=
             if #left x = l 
             then 
                drop_lx(l,rest)
             else
                list 
   
      fun acc_errors(sf,[])=sf
      |   acc_errors(sf,(e as Error(Warning,_,_),_)::rest)=
             acc_errors(e::sf,rest)
      |   acc_errors(sf,(e as Error(Serious,x,_),_)::rest)=
             acc_errors(e::sf,drop_lx(#left x,rest))
   in
      List.rev(acc_errors([],sorted_errors))
   end
in
   List.app print_fn final_errors
end
end
