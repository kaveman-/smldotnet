(* Intersects is a functor which takes an ORD_KEY to
   INTERSECTS, a structure containing the function intersects which tests if
   two lists contain a common element. *)
signature INTERSECTS=
sig
   type t (* item we can sort *)
   val intersects:t list*t list->bool
   val find_intersects:(t*'a) list*(t*'b) list->(t*'a*'b) list
   (* find_intersects(alist,blist) returns the list containing, for each t such that
      a (t,_) occurs in alist and blist, (t,a,b) where (t,a) is in alist and (t,b) is in blist.
      What happens when t occurs more than once in either list is undefined. *)

   val intersects_sorted:t list*t list->bool
   val find_intersects_sorted:(t*'a) list*(t*'b) list->(t*'a*'b) list
   (* intersects_sorted and find_intersects_sorted are like intersects and find_intersects, but
      they assume their arguments are sorted in increasing order with respect to t. *)
end
