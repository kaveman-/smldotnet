(*======================================================================*)
(* Implementation of StringCvt structure				*)
(*======================================================================*)
structure StringCvt :> STRING_CVT =
struct

open Option List PrimUtils_.Int 

datatype radix = 
  BIN 
| OCT 
| DEC 
| HEX

datatype realfmt = 
  SCI of int option	
| FIX of int option   
| GEN of int option 	
| EXACT
                        
type cs = int
                        
type ('a, 'b) reader = 'b -> ('a * 'b) option

fun scanString scan s = 
  let val len = PrimUtils_.String.size s
      fun getc i = if i >= len then NONE 
	           else SOME (PrimUtils_.String.sub(s, i), i+1)
  in 
    case scan getc 0 of
      NONE => NONE
    | SOME (res, _) => SOME res
  end

fun scanList scan cs =
    let fun getc []      = NONE 
	  | getc (c::cr) = SOME (c, cr)
    in case scan getc cs of
	NONE          => NONE
      | SOME (res, _) => SOME res
    end

fun dropl p getc = 
    let fun h src =
	case getc src of
	    NONE          => src
	  | SOME(c, rest) => if p c then h rest else src
    in h end

fun splitl p f src = 
let
  val sb = StringBuffer_.empty ()
  fun gather x =
  case f x of 
    NONE => (StringBuffer_.toString sb, x)
  | SOME (c, x') =>
    if p c then (StringBuffer_.appendChar(sb, c); gather x')
    else (StringBuffer_.toString sb, x)
in
  gather src
end
  
fun skipWS getc = dropl PrimUtils_.Char.isSpace getc

fun takel p getc src = #1 (splitl p getc src)

fun padLeft c n s = 
  let 
    val gap = n - PrimUtils_.String.size s
  in
    if gap <= 0 then s
    else 
    let
      val sb = StringBuffer_.emptyWith n
      fun fill 0 = (StringBuffer_.appendString(sb, s); StringBuffer_.toString sb)
        | fill n = (StringBuffer_.appendChar(sb, c); fill (n-1))
    in
      fill gap
    end
  end
      
fun padRight c n s = 
  let 
    val gap = n - PrimUtils_.String.size s
  in
    if gap <= 0 then s
    else 
    let
      val sb = StringBuffer_.emptyWith n
      fun fill 0 = StringBuffer_.toString sb        
        | fill n = (StringBuffer_.appendChar(sb, c); fill (n-1))
    in
      StringBuffer_.appendString(sb, s); fill gap
    end
  end
      

end
