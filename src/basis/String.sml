(*======================================================================*)
(* Standard basis: structure String.					*)
(*======================================================================*)
structure String :> STRING 
where 
  type string = string and 
  type Char.char = char and 
  type Char.string = string 
= struct

type string = string

structure Char = Char

local 
  open General Bool Option List Int
  val op= = Prim.=
in

open PrimUtils_.String

fun substring(s, i, n) = extract(s, i, SOME n)

(* For concat we use StringBuffer_ but with some special
   cases on 0,1,2 strings for efficiency *)
fun concat [] = ""
  | concat [s] = s
  | concat [s1,s2] = s1 ^ s2
  | concat (s::ss) =
    let 
      val sb = StringBuffer_.fromString s
      fun app [] = StringBuffer_.toString sb
        | app (s::ss) = (StringBuffer_.appendString(sb, s); app ss)
    in
      app ss
    end

fun concatWith sep strings =
let
  fun c [] = ""
    | c [s] = s
    | c [s1,s2] = s1 ^ sep ^ s2
    | c (s::ss) =
      let 
        val sb = StringBuffer_.fromString s
        fun app [] = StringBuffer_.toString sb
          | app (s::ss) = (StringBuffer_.appendString(sb, sep); StringBuffer_.appendString(sb, s); app ss)
      in
        app ss
      end
in
  c strings
end


(* For implode we do a similar thing, using StringBuffer_... *)
fun implode [] = ""
  | implode [c] = str c
  | implode cs =
    let 
      val sb = StringBuffer_.empty ()
      fun app []  = StringBuffer_.toString sb
        | app (c::cs) = (StringBuffer_.appendChar(sb, c); app cs)
    in
      app cs
    end

fun translate f s = 
let
  val sb = StringBuffer_.empty ()
  val finish = size s
  fun tr j = 
      if j=finish 
      then StringBuffer_.toString sb
      else 
      (StringBuffer_.appendString (sb, f (sub(s, j))); tr (j+1))
in
  tr 0
end
  
   
(* For explode we just use a tail-recursive auxiliary function *)
fun explode s =
let 
  fun exp (0, acc) = acc
    | exp (j, acc) = 
      let val j' = j-1
      in
        exp (j', sub(s, j')::acc)
      end
in
  exp (size s, [])
end

      
fun collate cmp (s1, s2) =
    let val n1 = size s1 
	and n2 = size s2
	val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point s1[0..j-1] = s2[0..j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
              let 
                val c1 = sub(s1, j)
                val c2 = sub(s2, j)
              in
		case cmp(c1,c2) of
		    EQUAL   => h (j+1)
                  | result  => result
              end
    in h 0 end;

val op< = fn (s1:string,s2:string) => PrimUtils_.strCompare(s1,s2) < 0
val op> = fn (s1:string,s2:string) => PrimUtils_.strCompare(s1,s2) > 0
val op<= = fn (s1:string,s2:string) => PrimUtils_.strCompare(s1,s2) <= 0
val op>= = fn (s1:string,s2:string) => PrimUtils_.strCompare(s1,s2) >= 0

fun tokens p s = 
  map Substring_.string 
    (Substring_.tokens p (Substring_.all s))
fun fields p s = 
  map Substring_.string 
    (Substring_.fields p (Substring_.all s))


fun fromString s =
let
  val n = size s
in
  if n=0 then SOME s
  else 
  let
    val sb = StringBuffer_.emptyWith n (* At least n characters *)
    fun getc i = if i=n then NONE else SOME(sub(s,i), i+1)
  in
    case Char.scan getc 0 of
      NONE => NONE
    | SOME (c, i) =>
      let
        fun loop i = 
            case Char.scan getc i of
              NONE => SOME (StringBuffer_.toString sb)
            | SOME (c, i) => (StringBuffer_.appendChar(sb, c); loop i)
      in
        StringBuffer_.appendChar(sb, c); loop i
      end
  end
end

fun fromCString s =
let
  val n = size s
in
  if n=0 then SOME s
  else 
  let
    val sb = StringBuffer_.emptyWith n (* At least n characters *)
    fun getc i = if i=n then NONE else SOME(sub(s,i), i+1)
  in
    case Utils_.charscan false getc 0 of
      NONE => NONE
    | SOME (c, i) =>
      let
        fun loop i = 
            case Utils_.charscan false getc i of
              NONE => SOME (StringBuffer_.toString sb)
            | SOME (c, i) => (StringBuffer_.appendChar(sb, c); loop i)
      in
        StringBuffer_.appendChar(sb, c); loop i
      end
  end
end

fun toString s = translate Char.toString s

fun toCString s = translate Char.toCString s

val maxSize = valOf(Int.maxInt)

fun map f s = 
let
  val finish = size s
  val sb = StringBuffer_.emptyWith finish
  fun tr j = 
      if j=finish 
      then StringBuffer_.toString sb
      else 
      (StringBuffer_.appendChar (sb, f (sub(s, j))); tr (j+1))
in
  tr 0
end
 

end

end
