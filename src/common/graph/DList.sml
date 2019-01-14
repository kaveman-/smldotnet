(* DList:DLIST implements doubly-linked lists.  We think of these
   as going from left to right, or alternatively from right to left. *)
structure DList:>DLIST=
struct
(* Implementation: there are surprisingly many choices to be made.
   We use a circular list of items containing a dummy element.  The dlist
   is represented by the dummy element.  The dummy item is distinguished
   by the fact that its value field is NONE. Deleted items are distinguished
   because their left and right fields
   are set to ref NONE; neither will be NONE otherwise.
   All items passed in by the user are assumed not to be the dummy element.
   *)

   datatype 'a item=I of
      {left:'a item option ref,right:'a item option ref,value:'a option}
   datatype 'a dlist=D of 'a item
   (* D is not exported in the signature, otherwise the user might be
      able to get at the dummy item.

      NB - as this is a circular list, the left pointer of the dlist points
      to the rightmost item, and vice-versa! *)

   fun empty {}=let
      val dummy={left=ref NONE,right=ref NONE,value=NONE}
      val dummy_item=I dummy
      val _= #left(dummy) := SOME dummy_item
      val _= #right(dummy):= SOME dummy_item
   in
      D dummy_item
   end

   fun is_same(D(I{left=left1,...}),D(I{left=left2,...}))=(left1=left2)
   (* ASSUMPTION:  if two dlists have is_same false, they have no items
      in common.  Otherwise they should be identical.  This should be true
      for all dlists which can be produced using the functions in the DLIST
      signature. *)


   exception DeletedItem

   fun quick_left(I {left,...})=
      case !left of
         SOME l => l
      |  NONE   => raise DeletedItem

   fun quick_right(I {right,...})=
      case !right of
         SOME l => l
      |  NONE   => raise DeletedItem

(* insert_left and insert_right insert to the left and right of given item;
   currently they aren't in the signature but will be put there if required.
   Right now they are used in add_left and add_right. *)

   fun insert_left(item as I i,value)=let
      val toleftitem as I toleft=quick_left(item)
      val newrec=I
        {left=ref(SOME toleftitem),right=ref(SOME item),value=SOME value}
      val _= #left(i) := SOME newrec
      val _= #right(toleft) := SOME newrec
   in
      newrec
   end

   fun insert_right(item as I i,value)=let
      val torightitem as I toright=quick_right(item)
      val newrec=I
        {left=ref(SOME item),right=ref(SOME torightitem),value=SOME value}
      val _= #right(i) := SOME newrec
      val _= #left(toright) := SOME newrec
   in
      newrec
   end

   fun add_left(D dummy,v)=insert_right(dummy,v)
   fun add_right(D dummy,v)=insert_left(dummy,v)

   (* is_empty tests whether a dlist is empty *)
   fun is_empty(D(dummy_item))=let
      val I next=quick_right(dummy_item)
   in
     (case #value(next) of
(* we can't return "#value(next)=NONE" since requires next to be an eqtype! *)
         NONE   => true
      |  SOME _ => false
      )
   end

   (* insert_right_dlist inserts a dlist to the right of an item,
      clearing the dlist in the process.  It
      is assumed that the item is not in the dlist being inserted,
      or there will be trouble. *)
   fun insert_right_dlist(item as I i,dl as D(dummy_item as I dummy))=
   if is_empty(dl)
   then
      {}
   else
      let
         val left_item as I left=quick_right(dummy_item)
         val right_item as I right=quick_left(dummy_item)
         val to_right_item as I to_right=quick_right(item)
         val _ = #left(to_right):=SOME right_item
         val _ = #right(right):=SOME to_right_item
         val _ = #right(i):=SOME left_item
         val _ = #left(left):=SOME item

         val _ = #left(dummy):=SOME dummy_item
         val _ = #right(dummy):=SOME dummy_item
      in
         {}
      end


   fun transfer_left(D (dummy_item as I dummy),item as I i)=let
      val r_item as I r=quick_right(item)
      val l_item as I l=quick_left(item)
      val _= #left(r):=SOME l_item
      val _= #right(l):=SOME r_item

      val R_item as I R=quick_right(dummy_item)
      val _= #right(dummy):=SOME item
      val _= #left(R):=SOME item
      val _= #left(i):=SOME dummy_item
      val _= #right(i):=SOME R_item
   in
      {}
   end

   fun transfer_right(D (dummy_item as I dummy),item as I i)=let
      val r_item as I r=quick_right(item)
      val l_item as I l=quick_left(item)
      val _= #left(r):=SOME l_item
      val _= #right(l):=SOME r_item

      val L_item as I L=quick_left(dummy_item)
      val _= #left(dummy):=SOME item
      val _= #right(L):=SOME item
      val _= #right(i):=SOME dummy_item
      val _= #left(i):=SOME L_item
   in
      {}
   end

   fun move_left(i)=let
      val res as (I toleft)=quick_left(i)
   in
      case #value(toleft) of
         SOME _ => SOME res
      |  NONE   => NONE
   end

   fun move_right(i)=let
      val res as (I toright)=quick_right(i)
   in
      case #value(toright) of
         SOME _ => SOME res
      |  NONE   => NONE
   end


   fun leftmost(D dummy)=move_right(dummy)
   fun rightmost(D dummy)=move_left(dummy)

   fun valof(I{value=SOME v,...})=v

   fun delete(item as I i)=let
      val toleftitem as I toleft=quick_left(item)
      val torightitem as I toright=quick_right(item)
      val _= #right(toleft):=SOME torightitem
      val _= #left(toright):=SOME toleftitem
      val _= #left(i):=NONE
      val _= #right(i):=NONE
   in
      {}
   end

   fun transfer_left_dlist {from,to as D(to_dummy)}=
      if is_same(from,to)
      then
          raise Fail("transfer_left_dlist called with two identical dlists!")
      else
          insert_right_dlist(to_dummy,from)

   fun app f (D dummy)=let
      fun appp (item as I i)=(case #value(i) of
         NONE=>{}
      |  SOME v=>let
            val _:unit =f(v)
         in
            appp (quick_right item)
         end
         )
   in
      appp (quick_right(dummy))
   end

   fun foldl f initial (D dummy)=let
      fun foldll sofar (item as I i)=(case #value(i) of
         NONE=>sofar
      |  SOME v=>
         foldll (f(v,sofar)) (quick_right(item))
         )
   in foldll initial (quick_right(dummy))
   end

   fun foldr f initial (D dummy)=let
      fun foldrr sofar (item as I i)=(case #value(i) of
         NONE=>sofar
      |  SOME v=>
         foldrr (f(v,sofar)) (quick_left(item))
         )
   in foldrr initial (quick_left(dummy))
   end

   fun toList d=foldr op:: [] d

   fun map f dl =let
     val new_list=empty {}
     val _ = app
       (fn value=>let
           val _=add_right(new_list,f(value))
        in
           {}
        end
        ) dl
   in
      new_list
   end
end
