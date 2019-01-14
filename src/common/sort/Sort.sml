(* Sort is a functor which takes ORD_KEY structures into sorting functions on
   those keys.  ORD_KEY must have the following two members:

   type ord_key
   val compare : (ord_key * ord_key) -> order

   Thus this corresponds to the current definition of ORD_KEY in the
   SML/NJ extensions to the standard basis.  The implementation of the
   functor also uses these extensions, but the purpose of writing these
   functions is that this can be changed later.

   The result of the functor is :SORT rather than :>SORT, but I shall
   change this when SML/NJ implements "where type" correctly.
    *)
functor Sort (K:ORD_KEY) :> SORT where type key=K.ord_key =
struct
   type key=K.ord_key
   val sort=ListMergeSort.sort (fn (x,y)=>(K.compare(x,y)=GREATER))
   fun sort_pairs list=ListMergeSort.sort (fn ((x,_),(y,_))=>(K.compare(x,y)=GREATER)) list
   (* We need to eta expand to avoid a value restriction problem. *)

   val reverse_sort=ListMergeSort.sort (fn (x,y)=>(K.compare(x,y)=LESS))
   fun reverse_sort_pairs list=ListMergeSort.sort (fn ((x,_),(y,_))=>
      (K.compare(x,y)=LESS)) list

end
