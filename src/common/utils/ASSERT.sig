(* ASSERT:>Assert contains the exception and the assertion function for
   the Decode structure (it is not defined there to avoid circularity
   problems). *)
signature ASSERT=
sig
   exception ClassFile of string 
   (* Thrown if the file does not appear to be a valid class file. *)

   val assert:bool*string->unit
   (* raise ClassFile(string) if bool is false. *)

   val must_assert:bool*string->unit
   (* Similar to assert.  However, assert is used for checks which verify
      the class file but are not likely to fail in practice, and
      can be recovered from, while
      must_assert is used for checks which must always be done.
      For example, checks that the class file has the correct
      magic number and does not end prematurely are done with
      must_assert (which should never return if its bool argument is
      false).  
      *)

   val fail:string->'a
   (* Equivalent to must_assert(false,string) *)
end



