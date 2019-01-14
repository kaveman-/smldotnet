(* The Dominator functor takes an ORD_KEY structure to DOMINATOR 
   structure which computes dominators for
   nodes in a graph whose nodes are given by keys in the ORD_KEY.
   *)
functor Dominator(K:ORD_KEY):>DOMINATOR where type key=K.ord_key =
struct
   type key=K.ord_key
   structure Map=SplayMapFn(K)

   fun dominator successors root=
   let
(* The algorithm used here is the "simple" one explained in
   "A Fast Algorithm for Finding Dominators in a Flowgraph"
   by Thomas Lengauer and Robert Endre Tarjan
   in
   ACM Transactions on Programming Languages and Systems, Volume 1 #1
   (1979) pages 121-141

   There is a sophisticated version too but there does not
   seem to be a huge percentage
   difference and it is rather trickier to program.

   It is far too complicated to be explained in the comments.  However 
   I have tried to avoid its heavy use of arrays by instead encoding each
   vertex with a bundle of refs:
   *)      
      datatype vertex=
         V of
         {source:key, (* the original key associated with this vertex *)
          (* The remaining fields correspond to arrays with the same names in
             the paper. The various optional fields have the same
             function as vertex numbers of 0 in the original. *)
          parent:vertex option, 
          ancestor:vertex option ref,
          label:vertex option ref,
          semi:int ref,
          succ:vertex list ref,
          pred:vertex list ref,
          bucket:vertex list ref,
          dom:vertex option ref (* The dominator; also used as workspace *)
          }
      fun vertex_equal(V v1,V v2)=(#ancestor v1 = #ancestor v2)
   
      (* We define some abbreviations. ' means the raw form; without ' we mean the
         value stripped of refs and options *)
      fun source'(V v)= #source v
      val source=source'
   
      fun parent'(V v)= #parent v
      fun parent v=valOf(parent' v)
   
      fun ancestor'(V v)= #ancestor v
      fun ancestor v=valOf(!(ancestor' v))
   
      fun label'(V v)= #label v
      fun label v=valOf(!(label' v))
   
      fun semi'(V v)= #semi v
      fun semi v= !(semi' v)
   
      fun succ'(V v)= #succ v
      fun succ v= !(succ' v)
   
      fun pred'(V v)= #pred v
      fun pred v= !(pred' v)
   
      fun bucket'(V v)= #bucket v
      fun bucket v= !(bucket' v)

      fun dom'(V v)= #dom v
      fun dom v=valOf(!(dom' v))
   
      fun compress(v:vertex)=
      let
         (* This corresponds to COMPRESS in the paper.  It should never be called except when
            v has an ancestor *)
         val va= ancestor v
         val _=
            (case !(ancestor' va) of
               SOME vaa =>
                 (compress va;
                  if semi(label va)<semi(label v) 
                  then
                     label' v:= SOME(label va)
                  else {} ;
                  ancestor' v := !(ancestor'(va))
                  )
            |  NONE => {}
            )
      in 
         {}
      end
   
      fun eval(v:vertex)=
      (* this corresponds to EVAL in the paper *)
      (case !(ancestor' v) of
         NONE => label v
      |  SOME _ =>
        (compress v;
         label v)
      )
   
      fun link(v:vertex,w:vertex)=
      (* this corresponds to LINK in the paper *)
        ancestor' w:= SOME v
              
      type dfsstate={map:vertex Map.map,n:int,vertices:vertex list}
      fun dfs
         {this:key,this_parent:vertex option,state:dfsstate}=
      (* map_so_far maps keys to vertices.  n is a counter which numbers vertices in the
         order we visit them.  vertices is a list of all vertices in the (reverse) order we visit them;
         this will later be turned into a vector.  dfs returns the new vertex paired with
         the dfsstate after searching. *)
      let
         (* This vertex is unvisited.  Create a new vertex for it and recurse on
            unvisited children. *)
         val new_n= #n(state)+1
         val new_vertex=V 
           {source=this,
            parent=this_parent,
            ancestor=ref NONE,
            label=ref NONE, (* this will shortly be new_vertex *)
            semi=ref new_n,
            succ=ref [],
            pred=ref [],
            bucket=ref [],
            dom=ref NONE 
            }   
         val _= label' new_vertex:=SOME(new_vertex)
      
         val new_map=Map.insert(#map state,this,new_vertex)
         val new_vertices=new_vertex::(#vertices state)
   
         val new_state={map=new_map,n=new_n,vertices=new_vertices}
   
         val succs=successors this
   
         val final_state=
            List.foldl
               (fn (key,state:dfsstate) =>
               let
                  val (vert,next_state)= 
                     (case Map.find(#map state,key) of
                        NONE => dfs {this=key,this_parent=SOME new_vertex,state=state}
                     |  SOME vert => (vert,state)
                     )
                  (* Update succ and pred lists *)
                  val parent_succ=succ' new_vertex
                  val vert_pred=pred' vert
                  val _ = parent_succ:= vert:: !parent_succ
                  val _ = vert_pred:= new_vertex:: !vert_pred
               in
                  next_state
               end
               )                
               new_state
               succs
      in
         (new_vertex,final_state)   
      end

      val (root_vertex,{map=vertex_map,n=n,vertices})=
         dfs{this=root,this_parent=NONE,state={map=Map.empty,n=0,vertices=[]}}
   
      val vertex_array=Vector.fromList(root_vertex::List.rev(vertices)) 
      (* We use root_vertex for vertex[0] (which is never referred to) *)     
          
      fun vertex i=Vector.sub(vertex_array,i)
   
      fun scandown i=if i<2 then {} else
      let
         val w=vertex i
         fun forpreds []={}
         |   forpreds(v::rest)=
         let
            val u=eval v
            val _=
               if semi u<semi w then semi' w := semi u 
               else {} 
         in
            forpreds rest
         end
         val _= forpreds(pred w)

         val b=bucket'(vertex(semi w))
         val _= b:= w:: !b
         val _= link(parent w,w)

         fun forbucket []={}
         |   forbucket(v::rest)=
         let
            val u= eval v
            val _= dom' v:= SOME(if semi u<semi v then u else parent w)
         in
            forbucket rest
         end
         val bpw=bucket'(parent w)
         val _= forbucket(!bpw)
         val _= bpw:=[]
      in
         scandown(i-1)
      end

      val _= scandown n

      fun scanup i=if i>n then {}
      else let
         val w=vertex i
         val _= 
            if vertex_equal(dom w,vertex(semi w)) 
            then {}
            else dom' w:= !(dom'(dom w))
      in
         scanup(i+1)
      end
      val _= scanup 2

      val _= dom'(root_vertex):= NONE

      val first_result_map=
         Map.map 
            (fn v=> 
               (case !(dom' v) of 
                  NONE => root (* this is a dummy value for root which will shortly be removed *)
               |  SOME x => source x
               )
            )
            vertex_map 
      val (result_map,_)=Map.remove(first_result_map,root)
      (* We can now throw away everything apart from result_map *)
   in
      fn k => Map.find(result_map,k)
   end
end

