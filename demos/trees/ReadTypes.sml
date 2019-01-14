(* Given some base type pass over all the types in its assembly *)
(* Construct an ML tree representing the subclass relation *)

structure ReadTypes :> 
sig
  val make : string -> string Tree.Tree Result.Result
end =
struct

  val maxBreadth = 5
  val maxDepth = 5
  val dotdotdot = Tree.Node("(...)",[])    
  
  fun make (basetyname:string) =
      (* Look up the base type *)
      case System.Type.GetType basetyname of
	  NONE =>
	      Result.Failure ("No such type '" ^ basetyname ^ "'")
	| SOME basety =>
	      let (* Extract its module and assembly *)
		  val SOME module = basety.#get_Module()
		  val SOME assembly = module.#get_Assembly()
		  (* List all types in the assembly as an array *)
		  val SOME types = assembly.#GetTypes()
		  fun name (t:System.Type) = valOf(t.#get_FullName())
		  
	         (* Construct a list of pairs (t1,t2) such that type t1 extends t2 *)
		  val pairs = Array.foldr (fn (SOME t, pairs) =>
					    case t.#get_BaseType() of
						NONE => pairs
					      | SOME t' => (t,t')::pairs) 
		                            [] 
					    types
		  
		  (* Construct a class hierarchy tree rooted at t *)
		  fun gather 0 t = dotdotdot
                    | gather d t = 
		      Tree.Node(name t,
				gathernodes d maxBreadth
				(List.mapPartial (fn (c,p)=> if p.#Equals(t) 
								 then SOME c 
							     else NONE)
				                  pairs))
		  and gathernodes _ _ [] = []
		    | gathernodes _ 0 _ = [dotdotdot]
		    | gathernodes d b (t::ts) = (gather (d-1) t)::(gathernodes d (b-1) ts)
		           
		      
	      in
		  Result.Success (gather maxDepth basety)
	      end
end (* of struct *)


