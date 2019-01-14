(* INTSET generalises PARTIALINTSET *)
signature INTSET=
sig
   type intitem
   type intset
   val empty:intset
   val singleton:intitem->intset
   val intersects:intset*intset->bool
   (* Returns true if the two sets intersect; false otherwise *)
   val intersects_table:
     {contents:'c list,
      eval:'c -> ('b*intset),
      cost:'b*'b -> int
      } -> (int*'c) list
   (* Let (c_i,iset_i)= eval(contents[i]).
      Returns list in which ith element is
      (sum_{j\ne i and iset_i intersects iset_j} cost(c_i,c_j),contents[i]).
      *)

   val union:intset*intset->intset
   (* Takes the union of the two sets, returning it.  Once this is done, the two arguments will not
      be referred to again by ORTHOGONALPARTION or anything else *)
   val is_empty:intset->bool

   (* Now similar functions, for labelled sets where we want labelled items plus more control when
      disjoint unions fail *)
   type 'a intset'
   val empty':'a intset'
   val is_empty':'a intset' -> bool

   val singleton':intitem*'a->'a intset'
   val intersects':'a intset' * 'a intset' -> (intitem*'a*'a) list
   (* Like intersects only returns the list of pairs (item,a1,a2) where there is a clash between a1 and
      a2. *)
   val union':'a intset' * 'a intset' * ('a*'a->'a) -> 'a intset'
   (* Like union'.  However, when it comes across a pair (a1,a2) where there is a clash, it
      removes it by applying the function argument to (a1,a2) and using its result. *)
end
