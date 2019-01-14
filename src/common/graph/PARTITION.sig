(* Partition:PARTITION does not use the Graph structure but is in this
   directory because it can be used for many graph algorithms.  It
   is a functor which takes an ORD_MAP indexing things of type elem to
   a structure which manipulates partitions on things of type elem *)
signature PARTITION=
sig
   type elem
   type part (* this corresponds to sets in the partition *)
   type partition (* this corresponds to the partition itself *)
   val new_partition:elem list->partition
      (* create a new partition on the set of elements in the elem list
         where each element is in its own set. *)
   val find_part:partition*elem->part
      (* finds the part containing an element *)
   val union:partition*part*part->partition
      (* returns the partition in which the two parts given (which should
         be parts of this partition) have been replaced by their union
         if they are different; if they are the same nothing is changed. *)
   val union_list:partition*part list->partition
   (* Obvious generalisation of union:

      union_list(partit,p1::p2::rest)=
      let
         val new_partit=union(partit,p1,p2)
      in
         union_list(new_partit,find_part(new_partit,p1)::rest)
      end
      |  union_list(partit,_)=partit
      *)

   val list_part:partition*part->elem list
      (* returns the elements in the part *)
   val list_parts:partition->part list
      (* returns the parts in the partition *)

   structure OrdPart:ORD_KEY where type ord_key = part
   (* this provides an ordering on parts of the same set *)
end
