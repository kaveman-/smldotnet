(* W8Order:ORD_KEY orders Word8Vectors. *)
structure W8Order:>ORD_KEY where type ord_key=Word8Vector.vector =
struct
   type ord_key=Word8Vector.vector
   fun compare(k,l)=
   let
      fun ci i=
      let
         val ki=Word8Vector.sub(k,i)
         val li=Word8Vector.sub(l,i)
      in
         (case Word8.compare(ki,li) of
            LESS => LESS
         |  GREATER => GREATER
         |  EQUAL => ci(i+1)
         )
      end 
         handle Subscript =>
            Int.compare(Word8Vector.length k,Word8Vector.length l)
   in
      ci 0
   end
end