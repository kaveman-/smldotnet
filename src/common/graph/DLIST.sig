(* DList:DLIST implements doubly-linked lists.  We think of these
   as going from left to right, or alternatively from right to left. *)
signature DLIST=
sig
   type 'a dlist (* the doubly-linked list.  Doubly-linked lists are
                    mutable. *)
   type 'a item (* item in the dlist, from which we can move backwards and
                   forwards *)

   val empty:unit->'a dlist
(* creates an empty dlist (it would not do to have a single value
   empty:'a dlist since dlists are mutable). *)

(* add_left and add_right add items at the left and right ends of a dlist.
   The new item is returned. *)
   val add_left:'a dlist*'a->'a item
   val add_right:'a dlist*'a ->'a item

(* transfer_left and transfer_right move an item to the left and right
   ends of a dlist (this should be slightly quicker than deleting and then
   adding, unless the compiler is very clever). *)
   val transfer_left:'a dlist*'a item->unit
   val transfer_right:'a dlist*'a item->unit

(* transfer_left_dlist transfers a whole dlist "from to the left of the
   dlist "to.  dlist "from is emptied. *)
   val transfer_left_dlist:{from:'a dlist,to:'a dlist}->unit

(* leftmost and rightmost return the leftmost or rightmost item of the
   dlist, or NONE if it's empty *)
   val leftmost:'a dlist->'a item option
   val rightmost:'a dlist->'a item option

   exception DeletedItem

(* move_left and move_right return the result of moving an item left or
   right, or NONE if the item is leftmost or rightmost respectively.
   If the item has been deleted DeletedItem is raised. *)
   val move_left:'a item->'a item option
   val move_right:'a item->'a item option

(* valof returns the value of the item; it will work even if the
   item has been deleted. *)
   val valof:'a item->'a

(* delete deletes the item from its enclosing dlist, or raises DeletedItem
   if the item has been deleted. *)
   val delete:'a item->unit

(* is_same is true if the two dlists are actually the same dlist (this means
   more than them containing the same thing; you can only get two dlists for
   which is_same returns true by creating one dlist and using it twice). *)
   val is_same:'a dlist*'a dlist->bool

   val toList:'a dlist->'a list
(* toList returns the list of elements in the dlist, with the left item
   at the head *)

   val app:('a->unit)->'a dlist->unit
   val foldl:(('a*'b)->'b)->'b->'a dlist->'b
   val foldr:(('a*'b)->'b)->'b->'a dlist->'b
   val map:('a->'b)->'a dlist->'b dlist
(* app, foldl, foldr and map are similar to List.app, foldl, foldr and map *)

end
