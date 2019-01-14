(* BytePackML:>BYTEPACKML contains the code for converting a
   Word8Vector into a UString.t *)
structure BytePackML:>BYTEPACKML=
struct
   (* This code is inverse to basis/BytePackJava; both the
      same structure BytePack. *)
   fun pack vec=
   let
      (* construct a reader for the vector *)
      type state=int
      fun reader i=
         (SOME(Word8Vector.sub(vec,i),i+1))
         handle Subscript => NONE

      val vec=BytePack.pack {start=0,reader=reader}
      val contents=BytePack.makeList vec
   in
      UString.fromUnicode(List.rev contents)
   end
end
