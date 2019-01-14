(* Construct an ML tree representing the structure of a directory *)
structure ReadDirs :> 
sig
  val make : string -> string Tree.Tree Result.Result
end =
struct
    
    open System.IO
    val maxBreadth = 5
    val maxDepth = 5
    val dotdotdot = Tree.Node("(...)",[])
    fun trans 0 (d : DirectoryInfo) = dotdotdot
      | trans depth (d : DirectoryInfo) =
	let
	    (* The subdirectories of this node *)
	    val SOME subdirs = d.#GetDirectories()

	    (* Get its name *)
	    val SOME text = d.#get_Name()
	    val truncated = Array.length subdirs > maxBreadth
	in
	    Tree.Node(text, 
		      ArraySlice.foldri (fn (i,SOME d, result) => trans (depth - 1) d::result)
		                        (if truncated then [dotdotdot] else [])
       			                (ArraySlice.slice(subdirs,0,if truncated then SOME maxBreadth else NONE)))
	end

    fun make (dir:string) = 
	Result.Success (trans maxDepth (DirectoryInfo(dir)))
	handle _ => Result.Failure "No such directory"
  
end (* of struct *)