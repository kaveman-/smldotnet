(* BytePackML:>BYTEPACKML contains the code for converting a
   Word8Vector into a UString.t *)
signature BYTEPACKML=
sig
   val pack:Word8Vector.vector->UString.t
end
