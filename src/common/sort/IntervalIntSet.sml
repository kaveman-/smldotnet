(* IntervalIntSet implements PARTIALINTSET for intervals *)
structure IntervalIntSet:>PARTIALINTSET where type intitem= {from:int,to:int} =
struct
   type intitem= {from:int,to:int}
   (* The interval is [from,to).  Thus {from=f1,to=t1} intersects with {from=f2,to=t2} if
      f1 is in [f2,t2) or f2 is in (f1,t1) *)
   type intset=intitem list
   val empty=[]
   (* The intervals in the intset should be disjoint, nonempty, and sorted *)
   val is_empty=List.null

   fun singleton (s as {from,to}:intitem) =
      if from>=to
      then []
      else [s]

   datatype IntervalCompare=ILESS|IGREATER|IOVERLAP

   fun compareInterval({from=f1,to=t1},{from=f2,to=t2})=
   (case Int.compare(f1,f2) of
      GREATER => if f1<t2 then IOVERLAP else IGREATER
   |  LESS => if f2<t1 then IOVERLAP else ILESS
   |  EQUAL => IOVERLAP
   )

   fun intersects(l1 as hd1::tl1,l2 as hd2::tl2)=
      (case compareInterval(hd1,hd2) of
         IGREATER => intersects(l1,tl2)
      |  ILESS => intersects(l2,tl1)
      |  IOVERLAP => true
      )
   |   intersects _ = false


   fun intersects_table{contents,eval,cost}=
   let
      (* Right now we use the naive algorithm for this *)
      val contents_vec=Vector.fromList contents
      val vec=Vector.map eval contents_vec
      
      fun c i=
      let
         val (ci,cset)=Vector.sub(vec,i)
     
         val fiddle=if is_empty(cset) then 0 else ~(cost(ci,ci))
      in
         Vector.foldl
            (fn ((di,dset),sf) =>
               if intersects(cset,dset)
               then
                  sf+cost(ci,di)
               else
                  sf
               )
            fiddle
            vec
      end
   in
      List.tabulate(Vector.length contents_vec,
         fn i => (c i,Vector.sub(contents_vec,i))
         )
   end 
      
   fun union(l1,l2)=
   let
      fun inner_union(sofar,l1 as hd1::tl1,l2 as hd2::tl2)=
      (* The answer is sofar@ the union of l1 and l2. *)
         if #from hd1 <= #from hd2
         then build_interval(sofar,hd1,tl1,l2)
         else build_interval(sofar,hd2,tl2,l1)
      |   inner_union(sofar,[],l2)=List.revAppend(sofar,l2)
      |   inner_union(sofar,l1,[])=List.revAppend(sofar,l1)
      and
      (* build_interval(sofar,I,x,y) should only be called when the answer is reverse(sofar)@
         the union of I,x and y; if x is non-empty then #from(hd x)> #to I; and if y is non-empty
         then #from(hd y)>= #from I *)
         build_interval(sofar,I,l1,l2 as hd2::tl2)=
         (case Int.compare(#from hd2,#to I) of
            LESS =>
               if #to hd2 <= #to I
               then
                  build_interval(sofar,I,l1,tl2)
               else
                  build_interval(sofar,{from= #from I,to= #to hd2},tl2,l1)
         | GREATER =>
               inner_union(I::sofar,l2,l1)
         | EQUAL =>
               build_interval(sofar,{from= #from I,to= #to hd2},tl2,l1)
         )
      |   build_interval(sofar,I,l1,[])=List.revAppend(sofar,I::l1)
   in
      inner_union([],l1,l2)
   end
end
