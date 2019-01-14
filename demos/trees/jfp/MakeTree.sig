(*======================================================================*)
(* A variety of tree generators						*)
(*======================================================================*)
signature MakeTree =
sig

(* chain n: a chain of n nodes *)
val chain        : int -> string Tree.Tree

(* the pathological tree described in the JFP paper *)
val pathol       : int -> string Tree.Tree

(* fullbinary n: a full binary tree with n generations *)
val fullbinary   : int -> string Tree.Tree

(* make f: generate a tree in which f is called for each node to determine
   how many children it has. Its argument is the generation number. *)
val make         : (int -> int) -> string Tree.Tree

val count	 : 'a Tree.Tree -> int

end

