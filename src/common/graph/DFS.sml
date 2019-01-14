(* DFS:DFS is a functor which takes an ORD_SET to a structure which
   performs depth-first searches *)
functor DFS(M:ORD_SET):>DFS where type key=M.Key.ord_key =
struct
   type key=M.Key.ord_key

   fun simple_dfs neighbours foldfun initial start=
   let
      fun do_fold(node,state as (visited_set,foldfun_state))=
         if M.member(visited_set,node)
         then state
         else
            List.foldl
               do_fold
               (M.add(visited_set,node),foldfun(node,foldfun_state))
               (neighbours node)
   in
      #2(do_fold(start,(M.empty,initial)))
   end

   fun dfs_combined neighbours initial start=
   let
      fun do_fold(node,state as (visited_set,neighbours_state))=
         if M.member(visited_set,node)
         then state
         else let
            val (nhbrs,new_state)=neighbours(node,neighbours_state)
         in
            List.foldl
               do_fold
               (M.add(visited_set,node),new_state)
               nhbrs
         end
   in
      #2(do_fold(start,(M.empty,initial)))
   end
end
