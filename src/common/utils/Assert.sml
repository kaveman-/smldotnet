(* ASSERT:>Assert contains the exception and the assertion function for
   the Decode structure (it is not defined there to avoid circularity
   problems). *)
structure Assert:>ASSERT=
struct
   exception ClassFile of string 
   (* Thrown if the file does not appear to be a valid class file. *)

   fun assert(b,s)=if not b then raise ClassFile s else {}
   fun must_assert(b,s)=if not b then raise ClassFile s else {}

   fun fail s=raise ClassFile s
end
