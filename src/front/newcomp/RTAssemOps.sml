(*======================================================================*)
(* Various helper functions for emitting assembler			*)
(*======================================================================*)
structure RTAssemOps :> RTASSEMOPS =
struct

(*----------------------------------------------------------------------*)
(* Convert a test into a string appropriate to b* and c* instructions	*)
(*----------------------------------------------------------------------*)
fun testToString test =
case test of
  RTInstrs.ge => "ge"
| RTInstrs.gt => "gt"
| RTInstrs.le => "le"
| RTInstrs.lt => "lt"
| RTInstrs.eq => "eq"
| RTInstrs.ne => "ne"

(*----------------------------------------------------------------------*)
(* Convert runtime numeric constants to the string format expected	*)
(* by ilasm.								*)
(*----------------------------------------------------------------------*)
val tildes = CharVector.map (fn #"~" => #"-" | c => c)
val intToString = tildes o RTInt.toString

fun longToString l = 
let 
  val op div = Word32.div
  val op mod = Word32.mod
  val base = 0wx10 (* 16 *)
  fun toChar w = if w < 0wxA
		     then Char.chr 
			 (Char.ord #"0" + Word32.toInt(w))
		 else  Char.chr (Char.ord (#"a") - 10 + Word32.toInt(w))
  fun trans(w,0,acc) = acc
    | trans(w,n,acc)=
      let val d = w div base
      in trans((w div base),n-1,(toChar (w mod base) :: acc))
      end
in
  (* let them eat hex *)
   case RTLong.toWordPair l of (high,low) =>  
       "0x"^String.implode(trans(high,8,trans(low,8,[])))
end

(*@TODO: replace calls to Real.[isFinite|isNat|signBit] by calls
         to [RTDouble|RTFloat].[isFinite|isNat|signBit] 
*)
local val nanString = "float64(0xFFF8000000000000)"
      val posinfString = "float64(0x7FF0000000000000)"
      val neginfString = "float64(0xFFF0000000000000)"
in
fun doubleToString d = 
     let val r = RTDouble.toReal d
     in
	if Real.isFinite r then 
          (*@BUG: RTDouble.toString may loose precision, eg under NJ
	   Real.toString 91827364509182.0 = "9.18273645092E13",
	   use PackFloat instead? *)
	  (tildes (RTDouble.toString d))
        else if Real.isNan r then	
	     nanString
	else if Real.signBit r then
	     neginfString
        else posinfString
     end
end

local val nanString = "float32(0xFFF8000000000000)"
      val posinfString = "float32(0x7F800000)"
      val neginfString = "float32(0xFF800000)"
in
fun floatToString f = 
    let val r = RTFloat.toReal f
    in	
	if Real.isFinite r then 
          (*@BUG: RTFloat.toString may loose precision *)
	  (tildes (RTFloat.toString f))
        else if Real.isNan r then	
	     nanString
	else if Real.signBit r then
	     posinfString
        else neginfString
    end
end




(*----------------------------------------------------------------------*)
(* Convert an identifier (e.g. field, method, local, class, namespace)	*)
(* to the format required by ilasm, surrounding it by single quotes     *)
(* or using octal format if non-ASCII characters are present.		*)
(*----------------------------------------------------------------------*)
(*@BUG: doesn't work for unicode *)
local (*@TODO: this is terribly inefficient *)
    fun toOct c =  
	let fun i2oct i = Char.chr((Char.ord #"0") + i)
        val i = Char.ord c
	    val h1 = i div 64
	    val i = i mod 64
	    val h2 = i div 8
	    val h3 = i mod 8
	in
		implode([#"\\",i2oct h1,i2oct h2,i2oct h3])
	end	
    fun requiresEscape c = not (Char.isPrint c) orelse c= #"'" orelse c= #"\\"
in
fun stringToString "" = "''"
  | stringToString s =
       if CharVector.exists requiresEscape s
       then "'"^(String.translate (fn c => if requiresEscape c then toOct c else str c) s)^"'"
       else "'"^s^"'"

(*@todo: handle unicode *)
fun UStringToString ustr =
let
in
  stringToString (valOf (UString.toString ustr))
end

val idToString = UStringToString o Id.toUString

end


(*@BUG: doesn't work for unicode *)
local (*@TODO: this is terribly inefficient *)
    fun toHex c =  
	let fun i2hex i = 
            if i < 10 
            then Char.chr ((Char.ord #"0") + i) 
	        else Char.chr ((Char.ord #"A") + (i-10))
        val i = Char.ord c
	    val h1 = i div 4096
	    val i = i mod 4096
	    val h2 = i div 256
	    val i = i mod 256
	    val h3 = i div 16
        val h4 = i mod 16
	in
        implode([i2hex h3, i2hex h4,#" ",i2hex h1,i2hex h2,#" "])
	end	
    fun requiresEscape c = not (Char.isPrint c) orelse c= #"\"" orelse c= #"\\"
in
fun toQString "" = "\"\""
  | toQString s =
        if CharVector.exists requiresEscape s
        then "bytearray ("^ (String.translate toHex s) ^")"
        else "\""^s^"\""

fun UStringToQString us =
let
  val SOME s = UString.toString us (*@TODO: doesn't for work unicode *)
in
  toQString s
end

end

end (* of struct *)
