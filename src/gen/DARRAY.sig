(* DArray:>DARRAY implements a lightweight dynamic array of integers.
   The default element is 0.
   *)
signature DARRAY=
sig
   type array
   val array:int->array (* creates a new array with the suggested size. *)
   val sub:array*int->int 
   val update:array*int*int->unit
   val clear:array -> unit
   val bound:array -> int
end

