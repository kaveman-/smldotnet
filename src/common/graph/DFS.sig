(* DFS:DFS is a functor which takes an ORD_SET to a structure which
   performs depth-first searches *)
signature DFS=
sig
   type key
   val simple_dfs:(key->key list)->(key*'a->'a)->'a->key->'a
   (* simple_dfs neighbours foldfun initial start
      performs a depth-first search on nodes of type key, starting at
      start.  Thus it visits a sequence of nodes start=n_1,...,n_r
      It returns foldfun(n_r,...,(foldfun(n_1,initial))...)
      *)

   val dfs_combined:(key*'a->(key list)*'a)->'a->key->'a
   (* dfs_combined neighbours initial start
      performs a depth-first search, calling neighbours each time it
      visits a node, and letting neighbours accumulate state in 'a,
      which is initially initial; the final state is returned. *)
end
