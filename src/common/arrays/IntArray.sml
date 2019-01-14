(* This will get removed when New Jersey implements this part of the standard basis *)
structure IntArray:>MONO_ARRAY where type elem=int =
struct
   open Array
   type elem=int
   type vector=elem vector
   type array=elem array
end
