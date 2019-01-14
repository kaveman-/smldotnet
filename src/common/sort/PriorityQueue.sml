(* PriorityQueue:PRIORITYQUEUE is a structure that implements priority queues for
   pairs int*int.  In a pair (k,n), k is used to key the priority queue.
   n is an identification number.  Identification numbers are allocated from
   0 upwards as items as added in the priority queue.  The queue itself is
   updated by side-effects. *)
structure PriorityQueue:>PRIORITYQUEUE=
struct
   val hint=ref 200 (* initial size of the arrays used *)

   (* We need two dynamic array structures, one for arrays of ints; the other for arrays of
      {key,id} pairs. *)

   structure IArray=IntArray
   structure DIA=DynamicArrayFn(IArray)

   structure KIArray:>MONO_ARRAY where type elem={key:int,id:int} option =
   struct
      open Array
      type elem={key:int,id:int} option
      type vector=elem vector
      type array=elem array
   end
   structure DKIA=DynamicArrayFn(KIArray)

   datatype queue=Q of
   (* The queue is a collection of pairs {key:int,id:int}.  These are stored in two dynamic
      arrays. *)
     {heap:DKIA.array, (* These are the (key,id) pairs, as a conventional binary heap.  NONE
                          is used for empty slots *)
      next_heap:int ref, (* next_heap is an upper bound on the next heap index to use *)
      idpos:DIA.array, (* idpos maps ids to indices in heap.  ~1 is used for empty slots *)
      next_id:int ref (* next_id is the number of the next id to use *)
      }

   exception NotFound

   (* parent1, parent2 and child should ideally be rewritten so that multiplications and
      divisions are done using shifts if appropriate *)
   fun parent1 heappos=2*heappos
   fun parent2 heappos=2*heappos+1
   fun child heappos=heappos div 2

   fun ochild heappos=if heappos=1 then NONE else SOME(child heappos)

   fun lowest_parent(q as Q{heap,...},heappos)=
   (* lowest parent of heappos, or if there is just one parent, that parent, or if there
      are no parents, returns NONE.  *)
   let
      val p1=parent1 heappos
      val ki1=DKIA.sub(heap,p1)
      val p2=parent2 heappos
      val ki2=DKIA.sub(heap,p2)
   in
      (case (ki1,ki2) of
         (SOME {key=k1,...},SOME {key=k2,...})=> if k1<=k2 then SOME p1 else SOME p2
      |  (SOME _,NONE)=>SOME p1
      |  (NONE,SOME _)=>SOME p2
      |  (NONE,NONE)=>NONE
      )
   end

   (* The heap invariant is that if X is a child of Y, then #key(heap[X])<= #key(heap[Y]). *)
   fun bump_down(Q{heap,idpos,...},newkey,heap_pos,id)=
   (* The heap satisfies the invariant.  We are either adding something to the heap, or decreasing
      the key of an element of it.  If we are adding something, heap_pos is the index of a
      currently empty space to use (it should have a non-empty child), and id is the new
      identification number; if changing something, heap_pos is the index of the changed position
      and id the identification number of the changed thing.  newkey is the new key for this
      position.  We modify the heap and idpos array appropriately, leaving the heap satisfying
      the invariant.  next_heap and next_pos are unchanged.
      *)
   let
      (* first, move up the children of heap_pos which are greater than newkey, and find
         the index of the resulting vacant spot *)
      fun move_up pos = (* position to start at *)
      let
         val ch=child pos (* if we are at already at the bottom of the heap, this will be 0. *)
         val ch_ki=
         (* SOME ki pair if the key is > newkey or
            NONE if the key of this is <=newkey *)
            (case DKIA.sub(heap,ch) of
               SOME (kipair as {key,...}) =>
                  if key>newkey
                  then SOME kipair
                  else NONE
            |  NONE => NONE (* we have reached the bottom of the heap *)
            )
      in
         (case ch_ki of
            SOME (kipair as {id,...}) =>
               (DKIA.update(heap,pos,SOME kipair);
                DIA.update(idpos,id,pos);
                move_up ch)
         |  NONE => pos
         )
      end

      val new_pos=move_up heap_pos
      (* Now put the new (id,key) pair in position new_pos *)
      val _=DKIA.update(heap,new_pos,SOME {key=newkey,id=id})
      val _=DIA.update(idpos,id,new_pos)
   in
      {}
   end

   fun bump_up(q as Q{heap,idpos,...},newkey,heap_pos,id)=
   (* this is like bump_down only in reverse (it is used when we are
      increasing the key of an item), and we have to use
      lowest_parent rather than child. *)
   let
      (* first, move down the children of heap_pos which are less than newkey, and find
         the index of the resulting vacant spot *)
      fun move_down pos = (* position to start at *)
      let
         val parent'=lowest_parent(q,pos)
         val p_ki=
         (* SOME ki pair if the key is < newkey or
            NONE if the key of this is >=newkey *)
            (case parent' of
               SOME parent =>
               let
                  val kipair as {key,...}= valOf(DKIA.sub(heap,parent))
               in
                  if key<newkey then SOME kipair else NONE
               end
            |  NONE => NONE (* we have reached the top of the heap *)
            )
      in
         (case p_ki of
            SOME (kipair as {id,...}) =>
               (DKIA.update(heap,pos,SOME kipair);
                DIA.update(idpos,id,pos);
                move_down(valOf parent'))
         |  NONE => pos
         )
      end

      val new_pos=move_down heap_pos
      (* Now put the new (id,key) pair in position new_pos *)
      val _=DKIA.update(heap,new_pos,SOME {key=newkey,id=id})
      val _=DIA.update(idpos,id,new_pos)
   in
      {}
   end


   fun remove(q as Q{heap,idpos,...},{id})=
   (* remove the item id from the heap. *)
   let
      val heappos=DIA.sub(idpos,id)
      val _=if heappos= ~1 then raise NotFound else {}
      val _=DIA.update(idpos,id,~1)

      fun move_down pos=
      (* this is like the move_down function inside bump_up, except that
         we don't check the key.  *)
      let
         val parent'=lowest_parent(q,pos)
         val p_ki=
         (* kipair for the parent, or NONE if there is no parent *)
            (case parent' of
               SOME parent => SOME(valOf(DKIA.sub(heap,parent)))
            |  NONE => NONE (* we have reached the top of the heap *)
            )
      in
         (case p_ki of
            SOME (kipair as {id,...}) =>
               (DKIA.update(heap,pos,SOME kipair);
                DIA.update(idpos,id,pos);
                move_down(valOf parent'))
         |  NONE => pos
         )
      end

      val end_pos=move_down heappos
      val _=DKIA.update(heap,end_pos,NONE)
   in
      {}
   end

   fun empty ()=
   let
      val heap=DKIA.array(!hint,NONE)
      val idpos=DIA.array(!hint,~1)
   in
      Q {heap=heap,idpos=idpos,next_heap=ref 1,next_id=ref 0}
   end

   fun insert(q as Q{heap,idpos,next_heap,next_id},{key})=
   let
      val new_id= !next_id
      val _= next_id:= !next_id+1

      fun full pos=isSome(DKIA.sub(heap,pos)) orelse pos=0

      val heap_pos=
      let
         val nh= !next_heap
      in
         (* if nh's child is full, we use nh (hopefully this is the most common case).
            Otherwise we reduce next_heap as far as we can without making !nh full,
            and then set the heap_pos to the non-full descendant of nh with full child.
            *)
         if full(child nh)
         then (next_heap:= nh+1 ; nh)
         else
            ( (while not(full(!next_heap -1)) do next_heap:= !next_heap -1);
              let
                 fun new_heap_pos pos=
                    if full(child pos) then pos
                    else new_heap_pos(child pos)
                 val nhp=new_heap_pos(!next_heap)
                 val _= if !next_heap=nhp then next_heap:= nhp+1 else {}
              in
                 nhp
              end
              )
      end

      val _=bump_down(q,key,heap_pos,new_id)
   in
      {id=new_id}
   end

   fun bump(q as Q {heap,idpos,...},{id,newkey})=
   let
      val pos=DIA.sub(idpos,id)
      val oldkey= #key(valOf(DKIA.sub(heap,pos)))
   in
      (case Int.compare(newkey,oldkey) of
         GREATER => bump_up(q,newkey,pos,id)
      |  LESS => bump_down(q,newkey,pos,id)
      |  EQUAL => {}
      )
   end

   fun lookup(q as Q {heap,idpos,...},{id})=
   let
      val pos=DIA.sub(idpos,id)
   in
      if pos<0
      then raise NotFound
      else {key= #key(valOf(DKIA.sub(heap,pos)))}
   end

   fun lowest(q as Q {heap,...},n)=
   let
      fun full pos=isSome(DKIA.sub(heap,pos))
      fun poskey pos= #key(valOf(DKIA.sub(heap,pos)))

      fun l(sofar,nfound,[])=sofar
      |   l(sofar,nfound,frontier as hdfrontier::tlfrontier)=
             (* frontier is a list of nonempty heap positions.  Find an element with the
                lowest key, add that to sofar, add its parents to frontier, and recurse *)
      let
         fun lowestrest(before_best,best,considered,remaining)=
            (case remaining of
               [] => (best,List.revAppend(considered,before_best))
            |  hd::tl =>
               if poskey hd<poskey best then
                  lowestrest(
                     List.revAppend(considered,best::before_best),
                     hd,
                     [],
                     tl)
               else
                  lowestrest(before_best,best,hd::considered,tl)
            )
         val (lowest,frontier)=lowestrest([],hdfrontier,[],tlfrontier)
         val nfound=nfound+1
         val sofar=valOf(DKIA.sub(heap,lowest))::sofar
      in
         if nfound=n
         then sofar
         else
         let
            val frontier=
               if full(parent1 lowest)
               then parent1 lowest::frontier
               else frontier
            val frontier=
               if full(parent2 lowest)
               then parent2 lowest::frontier
               else frontier
         in
            l(sofar,nfound,frontier)
         end
      end
   in
      if full(1)
      then l([],0,[1])
      else [] (* heap is empty *)
   end
end
