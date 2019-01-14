(* The Dominator functor takes an ORD_KEY structure to DOMINATOR 
   structure which computes dominators for
   nodes in a graph whose nodes are given by keys in the ORD_SET.
   *)
signature DOMINATOR=
sig
   type key
   val dominator:(key->key list)->key->key->(key option)
   (* dominator successors root is a function which
      maps a key to its immediate dominator (or NONE if the key is the root or
      is not reachable from the root), given the successors function. *)
end

