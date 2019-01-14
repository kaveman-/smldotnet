structure Byte :> BYTE =
struct

    fun byteToChar (w:Word8.word) = Char.chr (Word8.toInt w)
    fun charToByte (c:Char.char) = Word8.fromInt (Char.ord c)
    fun bytesToString (v:Word8Vector.vector) = 
 	let val sb = StringBuffer_.emptyWith (Word8Vector.length v)
	in
	    Word8Vector.app
	      (fn c => StringBuffer_.appendChar(sb,byteToChar c)) 
 	       v;
	    StringBuffer_.toString sb
	end

    fun stringToBytes (s:string) =
	Word8Vector.tabulate(String.size s,
			     fn i => charToByte(String.sub(s,i)))


    fun unpackStringVec slice =
	bytesToString(Word8VectorSlice.vector slice)

		 
    fun unpackString slice =
        bytesToString(Word8ArraySlice.vector slice)

    fun packString (a:Word8Array.array,i,s) =
        if Int.<(i,0) orelse Int.>(Int.+(Substring.size s,i),Word8Array.length a)
	then raise General.Subscript
	else ((Substring.foldr
	      (fn (c,i) => 
	       (Word8Array.update(a,i,charToByte c);
		Int.+(i,1)))
	       i 
	       s);
	      ())
end