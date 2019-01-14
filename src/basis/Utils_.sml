(*======================================================================*)
(* Basis utility functions that are target-independent			*)
(*======================================================================*)
structure Utils_ =
struct

local open Option PrimUtils_.Int in

fun radixToBase StringCvt.BIN = 2
  | radixToBase StringCvt.OCT = 8
  | radixToBase StringCvt.DEC = 10
  | radixToBase StringCvt.HEX = 16

fun fromDigit (c, StringCvt.BIN) =
    (case c of #"0" => 0 | #"1" => 1 | _ => ~1)

  | fromDigit (c, StringCvt.OCT) =
    if PrimUtils_.isDigit(c, 8)
    then PrimUtils_.Int.-(PrimUtils_.Char.ord c, PrimUtils_.Char.ord #"0")
    else ~1

  | fromDigit (c, StringCvt.DEC) =
    if PrimUtils_.isDigit(c,10)
    then PrimUtils_.Int.-(PrimUtils_.Char.ord c, PrimUtils_.Char.ord #"0")
    else ~1

  | fromDigit (c, StringCvt.HEX) =
    if PrimUtils_.isDigit(c,16)
    then if PrimUtils_.Char.isDigit(c) 
	 then PrimUtils_.Int.-(PrimUtils_.Char.ord c, PrimUtils_.Char.ord #"0")
	 else let val c = PrimUtils_.Char.toUpper c 
	      in
		  PrimUtils_.Int.+(PrimUtils_.Int.-(PrimUtils_.Char.ord c, (PrimUtils_.Char.ord #"A" )),10)
	      end
    else ~1 

(* sigh - special treatment for completely pointless hexadecimal
   "0x" or "0X".  trim0x removes "0x[digit]" from the stream cs,
   if present *)
fun trim0x getc cs0 =
    case getc cs0 of
         SOME (#"0",cs1) =>
         let
            fun check_dig cs2=
            (case getc cs2 of
                NONE => cs0
            |   SOME(ch,_) =>
                   if PrimUtils_.Char.isHexDigit ch then cs2 else cs0
            )
         in
            (case getc cs1 of
               SOME (#"X",cs2) => check_dig cs2
            |  SOME (#"x",cs2) => check_dig cs2
            |  _ => cs0
            )
         end
      |  _ => cs0

(* Trim white space, 0w prefix, and 0x/0X/0wx/0wX prefix when base=16 *)
fun trim0wx base getc cs0 =
    case getc cs0 of
         SOME (#"0",cs1) =>
         let
            fun check_dig cs2=
            (case getc cs2 of
                NONE => cs0
            |   SOME(ch,_) =>
                if PrimUtils_.Int.>=(fromDigit(ch,base),0) then cs2 else cs0
            )

            fun check_xdig cs2 =
            case getc cs2 of
              SOME(#"X",cs3) => check_dig cs3
            | SOME(#"x",cs3) => check_dig cs3
            | _ => cs0
         in
           case base of StringCvt.HEX =>
           (
             case getc cs1 of
               SOME (#"w",cs2) => 
               check_xdig cs2

             | SOME (#"x",cs2) =>
               check_dig cs2

             | SOME (#"X",cs2) =>
               check_dig cs2
 
             | _ => cs0
           )
           | _ =>
             case getc cs1 of
               SOME (#"w",cs2) => check_dig cs2
             | _ => cs0
         end
      | SOME (c,cs1) =>
        if PrimUtils_.Char.isSpace c
        then trim0wx base getc cs1
        else cs0

      | _ => cs0

(* Trim white space and sign; return false for pos, true for neg *)
fun trimSign getc src =
    case getc src of
      NONE => (false, src)
    | SOME (#"+", src') => (false, src')
    | SOME (#"-", src') => (true, src')
    | SOME (#"~", src') => (true, src')
    | SOME (c, src') =>
      if PrimUtils_.Char.isSpace c
      then trimSign getc src'
      else (false, src)

fun scanString scan s = 
  let val len = PrimUtils_.String.size s
      fun getc i = 
        if PrimUtils_.Int.>=(i, len) then Option.NONE
	else Option.SOME (PrimUtils_.String.sub(s, i), PrimUtils_.Int.+(i, 1))
  in 
    case scan getc 0 of
      Option.NONE => Option.NONE
    | Option.SOME (res, _) => Option.SOME res
  end

fun charscan ML getc src = 
let
  val radix = if ML then StringCvt.DEC else StringCvt.OCT
  val base = if ML then 10 else 8
in
  case getc src of
    SOME (#"\\", src) =>
    (case getc src of
      NONE => NONE
    | SOME (#"a", src) => SOME (#"\a", src)
    | SOME (#"b", src) => SOME (#"\b", src)
    | SOME (#"t", src) => SOME (#"\t", src)
    | SOME (#"n", src) => SOME (#"\n", src)
    | SOME (#"v", src) => SOME (#"\v", src)
    | SOME (#"f", src) => SOME (#"\f", src)
    | SOME (#"r", src) => SOME (#"\r", src)
    | (x as SOME (#"\"", src)) => x
    | (x as SOME (#"\\", src)) => x
    | SOME (#"^", src) =>
      (case getc src of
        NONE => NONE
      | SOME (c, src) =>
        if PrimUtils_.Char.>=(c, #"@") andalso PrimUtils_.Char.<=(c,#"_")
        then SOME (PrimUtils_.Char.chr (PrimUtils_.Int.-(PrimUtils_.Char.ord c, 64)), src)
        else NONE)
    | SOME (#"u", src) => 
      let
        fun checkedResult (src,result)  = 
	    if PrimUtils_.Int.>(result,PrimUtils_.Char.maxOrd) 
            then NONE 
	    else SOME (PrimUtils_.Char.chr result, src)
        fun loop (0, src, result) =
	    checkedResult(src,result)
          | loop (n, src, result) =
            case getc src of
              NONE => NONE 
            | SOME (c, src') =>
              let
                val d = fromDigit(c,StringCvt.HEX)
              in
                if PrimUtils_.Int.<(d, 0) then NONE
                else loop(PrimUtils_.Int.-(n,1), src', 
			  PrimUtils_.Int.+(PrimUtils_.Int.*(result,16), d))
              end
      in
        loop (4, src, 0)
      end

    | (x as SOME (c, src')) =>
      if PrimUtils_.Char.isSpace c then
      let
        fun whileSpace src =
        case getc src of
          NONE => NONE
        | SOME (#"\\", src) => charscan ML getc src
        | SOME (c, src) =>
          if PrimUtils_.Char.isSpace c then whileSpace src else NONE
      in
        whileSpace src'
      end

      else 
      let
        val d = fromDigit(c, radix)
        fun checkedResult (src,result)  = 
	    if (ML andalso PrimUtils_.Int.>(result,255))
	       orelse PrimUtils_.Int.>(result,PrimUtils_.Char.maxOrd)
	    then NONE 
	    else SOME (PrimUtils_.Char.chr result, src)
        fun loop (0, src, result) = 
	      checkedResult(src,result)
          | loop (n, src, result) =
            case getc src of
              NONE => 
		  (* ML escapes must be exactly three decimal digits, 
		     but C escapes can be one to three chars long *)
		  if ML then NONE 
		  else checkedResult(src,result)
            | SOME (c, src') =>
              let
                val d = fromDigit(c, radix)
              in
                if PrimUtils_.Int.<(d,0) 
		then  (if ML then NONE 
		       else checkedResult(src,result))
                else loop(PrimUtils_.Int.-(n,1), src', 
			  PrimUtils_.Int.+(PrimUtils_.Int.*(result,base), d))
              end
        fun cHexLoop (n,src,result) =
            case getc src of
              NONE => if n>0 then checkedResult(src,result) else NONE
            | SOME (c, src') =>
              let
                val d = fromDigit(c,StringCvt.HEX)
		val hexBase = radixToBase(StringCvt.HEX)
              in
                if PrimUtils_.Int.<(d,0) 
		then if n>0 then checkedResult(src,result) else NONE
                else cHexLoop(PrimUtils_.Int.+(n,1), 
			      src', 
			      PrimUtils_.Int.+(PrimUtils_.Int.*(result,hexBase), d))
              end
      in        
        if PrimUtils_.Int.<(d, 0)
        then
          if ML then NONE
          else (case c of
            #"'" => x
          | #"?" => x
          | #"x" => cHexLoop(0,src',0)
          | _ => NONE)
        else loop (2, src', d)
      end)
      
  | (x as SOME(c, src)) => 
    if PrimUtils_.Char.<(c, #" ") orelse PrimUtils_.Char.>=(c, #"\127") then NONE
    else x

  | NONE => NONE
end




fun implode [] = ""
  | implode [c] = PrimUtils_.String.str c
  | implode cs =
    let 
      val sb = StringBuffer_.empty ()
      fun app []  = StringBuffer_.toString sb
        | app (c::cs) = (StringBuffer_.appendChar(sb, c); app cs)
    in
      app cs
    end


(* format a word or integer *)
fun fmt{lt,mod,div,fromInt,toInt,zero} = fn radix => fn w => 
    let val base = fromInt (radixToBase radix)
	val negative = lt(w,zero)
	fun toChar w = if toInt w < 10
		       then PrimUtils_.Char.chr 
			   (PrimUtils_.Int.+(PrimUtils_.Char.ord #"0",
				     toInt w))
		       else PrimUtils_.Char.chr (PrimUtils_.Int.+(PrimUtils_.Int.-(PrimUtils_.Char.ord (#"A"),10), 
								toInt w))
        fun trans(w,acc) = 
	     if  Prim.=(w,zero) then
		 if negative then
		     implode(#"~"::acc)
		 else implode(acc)
	     else
	     let val d = w div base 
	     in if Prim.=(w,zero)
		    then trans((w mod base),acc)
		else trans((w div base),(toChar (w mod base) :: acc))
	     end 
    in
	if Prim.=(w,zero) then "0" else trans(w,[])
    end

(*@BUG: scanWord doesn't check for overflow (unless + does) *) 

fun scanWord {+,*,fromInt} = fn radix => fn getc => fn src =>
let
  val base = radixToBase radix
  val src = trim0wx radix getc src
  fun unsignedscan (n, src) =
  let
    fun done () = SOME (n, src)
  in
    case getc src of
      NONE => 
      done ()
    | SOME (c, src') =>
      let
        val i = fromDigit(c, radix)
      in
        if Prim.lt (i, 0)
        then done ()
        else 
        let
          val next = fromInt i + n*fromInt base
        in
          unsignedscan (next, src')
        end
      end
  end
in
  case getc src of
    NONE => NONE
  | SOME (c, src') =>
    let
      val i = fromDigit(c, radix)
    in
      if Prim.lt (i, 0)
      then NONE
      else unsignedscan (fromInt i, src')
    end
end

(*@NOTE: currently assumes Integer typed is fixed precision *)
fun scanInteger {fromInt,quot,maxInt,minInt,<,>,+,-,~,*} = fn radix => fn getc => fn src =>
let
  val base = fromInt (radixToBase radix)

  fun posScan (n, src) =
  let
    val limit = quot(valOf maxInt, base)

    fun done(n,src) = SOME (if n < fromInt(0) then raise General.Overflow else n, src)

    fun loop (n, src) =
    case getc src of
      NONE => 
      done(n,src)

    | SOME (c, src') =>
      let
        val i = fromInt (fromDigit(c, radix))
      in
        if i < fromInt(0)
        then done(n,src)
        else 
        if n > limit then raise General.Overflow
        else loop (n*base + i, src')
      end
  in
    loop (n, src)
  end

  fun negScan (n, src) =
  let
    val limit = quot(valOf minInt, base)

    fun done(n,src) = SOME (if n > fromInt(0) then raise General.Overflow else n, src)

    fun loop (n,src) =
    case getc src of
      NONE => 
      done(n,src)

    | SOME (c, src') =>
      let
        val i = fromInt (fromDigit(c, radix))
      in
        if i < (fromInt 0)
        then done(n,src)
        else 
        if n < limit then raise General.Overflow
        else loop (n*base - i, src')
      end
  in
    loop (n, src)
  end
    
  val (neg, src) = trimSign getc src
  val src = case radix of 
      StringCvt.HEX => trim0x getc src 
    | _ => src
in
  case getc src of
    NONE => NONE
  | SOME (c,src) =>
    let
      val i = fromInt (fromDigit(c, radix))
    in
      if i < fromInt(0) then NONE
      else
      if neg 
      then negScan (~i, src)
      else posScan (i, src)
    end
end


end
      
end


