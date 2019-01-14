(*======================================================================*)
(* Raw version of Substring structure without String substructure.      *)
(*======================================================================*)
structure Substring_ =
struct

type substring = string * int * int

local 
  open General List Bool Option Int PrimUtils_.String
  val op= = Prim.=
in

(* Invariant on values (s, i, n) of type substring:
 *                  0 <= i <= i+n <= size s, 
 * or equivalently, 0 <= i and 0 <= n and i+n <= size s.  
 *)

fun base arg = arg
    
val string = PrimUtils_.string

fun extract (s, i, NONE) =
    if 0 <= i andalso i <= size s then (s, i, size s - i)
    else raise General.Subscript
  | extract (s, i, SOME n) =
    if 0 <= i andalso 0 <= n andalso n <= size s - i then (s, i, n)
    else raise General.Subscript

fun substring (s, i, n) = extract(s, i, SOME n)

fun all s = (s, 0, size s)

fun getc (s, i, 0) = NONE
  | getc (s, i, n) = SOME(sub(s, i), (s, Int.+(i,1), n-1))

fun first (s, i, n) = 
    if n = 0 then NONE else SOME (sub(s,i));

fun isEmpty (s, i, n) = n=0;

fun triml k (s, i, n) = 
    if k < 0 then raise Subscript
    else if k > n then (s, i+n, 0) 
    else (s, i+k, n-k);

fun trimr k (s, i, n) = 
    if k < 0 then raise Subscript
    else if k > n then (s, i, 0) 
    else (s, i, n-k);

fun slice ((s', i', n'), i, NONE) =
    if 0 <= i andalso i <= n' then (s', i'+i, n'-i)
    (* If the argument is valid, then so is the result:
     *  0 <= i' <= i'+i <= i'+i + (n'-i) = i'+n' <= size s' *)
    else raise Subscript
  | slice ((s', i', n'), i, SOME n) =    
    if 0 <= i andalso 0 <= n andalso i+n <= n' then (s', i'+i, n)
    (* If the argument is valid, then so is the result:
     *  0 <= i' <= i'+i <= i'+i + n <= i'+n' <= size s' *)
    else raise Subscript

fun splitAt ((s, i, n), k) =
    if k < 0 orelse k > n then raise Subscript
    else ((s, i, k), (s, i+k, n-k));

(* For concat we use StringBuffer_ but with some special
   cases on 0,1 substrings for efficiency (cf String.concat) *)
fun concat [] = ""
  | concat [s] = string s
  | concat ss =
    let 
      val sb = StringBuffer_.empty ()
      fun app [] = StringBuffer_.toString sb
        | app ((s,i,n)::ss) = 
          let
            val finish = i+n
            fun app' j = 
              if j = finish then app ss
              else 
              (StringBuffer_.appendChar (sb, sub(s, j)); app' (j+1))
          in
            app' i
          end
    in
      app ss
    end

fun rest (ss as (s, i, n)) = 
 if n = 0 then ss else (s, i+1, n-1)


fun compare ((s1, i1, n1), (s2, i2, n2)) =
    let val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point (s1, i1, j) = (s2, i2, j) *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
		let val c1 = sub(s1, i1+j)
		    val c2 = sub(s2, i2+j)
		in if Char.<(c1,c2) then LESS
		   else if Char.>(c1,c2) then GREATER
		   else h (j+1)
		end
    in h 0 end;

fun isPrefix s1 (s2, i2, n2) =
    let val stop = if n2 < size s1 then n2 else size s1
	fun h j = (* At this point (s1, 0, j) = (s2, i2, j) *)
	    j = stop orelse sub(s1, j) = sub(s2, i2+j) andalso h (j+1)
    in size s1 <= n2 andalso h 0 end;

fun collate cmp ((s1, i1, n1), (s2, i2, n2)) =
    let val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point (s1, i1, j) = (s2, i2, j) *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
		case cmp(sub(s1, i1+j), sub(s2,i2+j)) of
		    LESS    => LESS
		  | GREATER => GREATER
		  | EQUAL   => h (j+1)
    in h 0 end;

fun foldr f e (s,i,n) = 
    let fun h j res = if j<i then res 
                      else h (j-1) (f (sub(s, j), res))
    in h (i+n-1) e end;

fun explode (s, i, n) =
    let fun h j res = if j<i then res
		      else h (j-1) (sub(s, j) :: res)
    in h (i+n-1) [] end;

fun foldl f e (s,i,n) = 
    let val stop = i+n
        fun h j res = if j>=stop then res 
                      else h (j+1) (f (sub(s,j), res))
    in h i e end;

fun app f ss = foldl (fn (x, _) => f x) () ss

exception Span

fun span ((s, i, n), (s', i', n')) = 
    if i > i'+n' orelse not (s=s') then 
	raise Span
    else
	(s, i, i'+n'-i)

fun translate f (s,i,n) = 
let
  val sb = StringBuffer_.empty ()
  val finish = i+n
  fun tr j = 
      if j=finish 
      then StringBuffer_.toString sb
      else
      (StringBuffer_.appendString (sb, f (sub(s,j))); tr (j+1))
in
  tr i
end

local
    fun scanl chop pred (s, i, n) = 
	let
	    val stop = i+n
	    fun scan j = if j < stop andalso pred(sub(s, j)) 
                         then scan (j+1)
			 else j
	in
	    chop (s, i, n, scan i - i)
	end
    fun scanr chop pred (s, i, n) = 
	let
	    val stop = i-1
	    fun scan j = if j > stop andalso pred(sub(s, j)) 
                         then scan(j-1)
			 else j
	in
	    chop (s, i, n, scan (i+n-1) - i + 1)
	end
in
    fun splitl p = scanl (fn (s, i, n, k) => ((s, i, k), (s, i+k, n-k))) p
    fun splitr p = scanr (fn (s, i, n, k) => ((s, i, k), (s, i+k, n-k))) p
    fun dropl  p = scanl (fn (s, i, n, k) => (s, i+k, n-k))              p
    fun dropr  p = scanr (fn (s, i, n, k) => (s, i, k))                  p
    fun takel  p = scanl (fn (s, i, n, k) => (s, i, k))                  p
    fun taker  p = scanr (fn (s, i, n, k) => (s, i+k, n-k))              p
end (* local *)

fun tokens isDelim ss = 
    let fun findTok ss = dropl isDelim ss
        fun h (remains as (_, _, n)) res = 
	    if n = 0 then List.rev res
	    else
		let val (token, aftertoken) = 
		    splitl (fn c => not(isDelim c)) remains 
		in h (findTok aftertoken) (token :: res) end
    in h (findTok ss) [] end;

fun fields isDelim ss = 
    let fun h ss res = 
	    let val (field, afterfield as (_, _, n)) = 
		splitl (fn c => not(isDelim c)) ss
	    in 
		if n = 0 then List.rev (field :: res)
		else h (rest afterfield) (field :: res) 
	    end
    in h ss [] end;


fun position s (ss as (s', i, n)) =             
    let val len = size s
	fun eq j k = j >= len orelse sub(s, j) = sub(s', k) 
                     andalso eq (j+1) (k+1)
	val stop = i+n-len
	fun cmp k = 
	    if k>stop then	 (* failure: *)
		(ss, (s', i+n, 0))
	    else if eq 0 k then  (* success: *)
		((s', i, k-i), (s', k, n-(k-i)))
	    else cmp(k+1)
    in cmp i end;
	
    (* Above, (eq j k)  means that  (s,j,len-j) = (s',k,len-j), 
           so (eq 0 k)  implies     s = (s', k, len).
       At successful termination, i <= k <= i+n-len, so 0 <= k-i <= n-len, 
       and therefore n >= n-(k-i) >= len >= 0.  It follows that 
       0 <= i <= i + (k-i) = k <= size s'     
           and 
       0 <= k <= k + n-(k-i) = n+i <= size s' (by (s', i, n) being valid),
       so the resulting substrings are valid.
    *)
       
fun scanString scan (s, i, n) =
    let fun getc k = if k >= n then NONE 
		     else SOME (sub(s, i+k), k+1)
    in case scan {getc=getc} i of
	NONE          => NONE
      | SOME (res, _) => SOME res
    end

fun sub((s', i', n'), i) =
  if i<0 orelse i >= n' then raise Subscript
  else PrimUtils_.String.sub(s', i+i')


fun size (_, _, n) = n

end (* local *)

end
