(* DProdIntSet is a functor taking an INTSET and a PARTIALINTSET structure to a PARTIALINTSET
   structure (there is additional code in this file for taking an  INTSET*INTSET to INTSET but
   it is commented out). *)
functor DProdIntSet(structure A:INTSET and B:PARTIALINTSET):>
  PARTIALINTSET where type intitem=A.intitem*B.intitem
=
struct
   type intitem=A.intitem*B.intitem
   type intset=B.intset A.intset'
   (* invariant - all the B.intsets should be nonempty. *)

   val empty=A.empty'
   val is_empty=A.is_empty'

   fun singleton(a,b)=A.singleton'(a,B.singleton(b))
   fun intersects(aset1,aset2)=
   let
      val aalist=A.intersects'(aset1,aset2)
   in
      List.exists
         (fn (_,bset1,bset2)=>B.intersects(bset1,bset2))
         aalist
   end

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
      
   fun union(aset1,aset2)=
      A.union'(aset1,aset2,fn (bset1,bset2)=>B.union(bset1,bset2))

(* remaining code only necessary and possible if we want INTSET and B is an INTSET *)
(*
   type 'a intset'='a B.intset' A.intset'
   fun singleton'((a,b),lab)=A.singleton'(a,B.singleton'(b,lab))
   fun intersects'(aset1,aset2)=
   let
      val aalist=A.intersects'(aset1,aset2)
   in
      List.concat
         (List.map
            (fn (a,bset1,bset2)=>
               List.map
                  (fn (b,lab1,lab2)=>((a,b),lab1,lab2))
                  (B.intersects'(bset1,bset2))
               )
            aalist
            )
   end

   fun union'(aset1,aset2,f)=
      A.union'(aset1,aset2,fn(bset1,bset2)=>B.union'(bset1,bset2,f))
*)
end
