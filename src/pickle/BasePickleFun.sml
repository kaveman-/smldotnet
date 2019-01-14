(*======================================================================*)
(* Pickling combinators							*)
(* Functorized on a means of encoding and decoding a choice [0..n]	*)
(*======================================================================*)
functor BasePickleFun(structure Codec : CODEC) 
: BASEPICKLESIG where type State = Codec.State =
struct

type LocalState = exn list * Codec.State
type State = Codec.State
type Freq = Word32.word

type 'a Pickler = 'a * LocalState -> LocalState
type 'a Unpickler = LocalState -> 'a * LocalState
type 'a PU = 'a Pickler * 'a Unpickler

exception Unpickle = Codec.Unpickle
exception Pickle = Codec.Pickle

val trace = ref false

fun pickle (p,u) value =
let val (_,v) = p (value, ([],Codec.initial))
in
  v
end

fun unpickle (p,u) v =
case u ([], v) of
  (x,(_,_)) => x
  
fun ord n = 
let
  val (enc,dec) = Codec.codec n
  fun dump i = if !trace then print (Word32.toString i ^ "/" ^ Word32.toString n ^ ";") else ()
in
  (
    fn (i,(s,v)) => (s,(dump i; enc (i,v))),
    fn (s,v) => let val (w,v) = dec v in 
      ((dump w; w),(s,v)) end
  )
end

fun ord' freqs = 
let
  val (enc,dec) = Codec.codec' freqs
in
  (
    fn (i,(s,v)) => (s,enc (i,v)),
    fn (s,v) => let val (w,v) = dec v in 
      (w,(s,v)) end
  )
end


val unit : unit PU =
(
  fn ((), s) => s,
  fn s => ((), s)
) 

fun wrap (i,j) (p,u) =
(
  fn (x, s) => p (j x, s),
  fn s => let val (y, s') = u s in (i y, s') end
)

(*@BUG: if int has more than 32 bits *)
val int : int PU = wrap (Word32.toIntX, Word32.fromInt) (ord 0wxffffffff)

fun seq ((p1,u1), f : 'a -> 'b PU) =
(
  fn ((x1,x2),s) => #1 (f x1) (x2, p1 (x1, s)),

  fn s =>
  let
    val (x1,s1) = u1 s
    val (x2,s2) = #2 (f x1) s1
  in
    ((x1,x2),s2)
  end
)
  
fun pair ((p1,u1), (p2,u2)) =
(
  fn ((x1,x2),s) => p2 (x2, p1 (x1, s)),
  fn s =>
  let val (x1,s1) = u1 s            
      val (x2,s2) = u2 s1            
  in ((x1,x2),s2) end
)

fun triple ((p1,u1),(p2,u2),(p3,u3)) =
(
  fn ((x1,x2,x3),s) => p3 (x3, p2 (x2, p1 (x1, s))),
  fn s =>
  let val (x1,s1) = u1 s      
      val (x2,s2) = u2 s1
      val (x3,s3) = u3 s2     
  in ((x1,x2,x3),s3) end
)

fun quadruple ((p1,u1),(p2,u2),(p3,u3),(p4,u4)) =
(
  fn ((x1,x2,x3,x4),s) => p4 (x4, p3 (x3, p2 (x2, p1 (x1, s)))),
  fn s =>
  let val (x1,s1) = u1 s     
      val (x2,s2) = u2 s1
      val (x3,s3) = u3 s2     
      val (x4,s4) = u4 s3
  in ((x1,x2,x3,x4),s4) end
)

fun quintuple ((p1,u1),(p2,u2),(p3,u3),(p4,u4),(p5,u5)) =
(
  fn ((x1,x2,x3,x4,x5),s) => p5 (x5, p4 (x4, p3 (x3, p2 (x2, p1 (x1, s))))),
  fn s =>
  let val (x1,s1) = u1 s     
      val (x2,s2) = u2 s1
      val (x3,s3) = u3 s2     
      val (x4,s4) = u4 s3
      val (x5,s5) = u5 s4
  in ((x1,x2,x3,x4,x5),s5) end
)

fun sextuple ((p1,u1),(p2,u2),(p3,u3),(p4,u4),(p5,u5),(p6,u6)) =
(
  fn ((x1,x2,x3,x4,x5,x6),s) => p6(x6, p5(x5, p4 (x4, p3 (x3, p2 (x2, p1 (x1, s)))))),
  fn s =>
  let val (x1,s1) = u1 s     
      val (x2,s2) = u2 s1
      val (x3,s3) = u3 s2     
      val (x4,s4) = u4 s3
      val (x5,s5) = u5 s4
      val (x6,s6) = u6 s5
  in ((x1,x2,x3,x4,x5,x6),s6) end
)

fun fix F =
let
  fun p x = let val (p',_) = F (p,u) in p' x end
  and u x = let val (_,u') = F (p,u) in u' x end
in
  (p,u)
end

fun fix2 F =
let
  fun p1  x = let val ((f,f'),(g,g')) = F ((p1,p1'),(p2,p2')) in f x end
  and p1' x = let val ((f,f'),(g,g')) = F ((p1,p1'),(p2,p2')) in f' x end
  and p2  x = let val ((f,f'),(g,g')) = F ((p1,p1'),(p2,p2')) in g x end
  and p2' x = let val ((f,f'),(g,g')) = F ((p1,p1'),(p2,p2')) in g' x end
in
  ((p1,p1'),(p2,p2'))
end

fun fix3 F =
let
  fun p1  x = let val ((f,f'),(g,g'),(h,h')) = F ((p1,p1'),(p2,p2'),(p3,p3')) in f x end
  and p1' x = let val ((f,f'),(g,g'),(h,h')) = F ((p1,p1'),(p2,p2'),(p3,p3')) in f' x end
  and p2  x = let val ((f,f'),(g,g'),(h,h')) = F ((p1,p1'),(p2,p2'),(p3,p3')) in g x end
  and p2' x = let val ((f,f'),(g,g'),(h,h')) = F ((p1,p1'),(p2,p2'),(p3,p3')) in g' x end
  and p3  x = let val ((f,f'),(g,g'),(h,h')) = F ((p1,p1'),(p2,p2'),(p3,p3')) in h x end
  and p3' x = let val ((f,f'),(g,g'),(h,h')) = F ((p1,p1'),(p2,p2'),(p3,p3')) in h' x end
in
  ((p1,p1'),(p2,p2'),(p3,p3'))
end

fun pfix (F:('b->'a PU) -> ('b->'a PU)) = fn z:'b =>
let
  fun p x = let val (p',_) = F (fn z => (p,u)) z in p' x end
  and u x = let val (_,u') = F (fn z => (p,u)) z in u' x end
in
  (p,u) : 'a PU
end

fun pfix' (F:('b * ('b ->'a PU)) -> 'a PU) = fn z:'b =>
let
  fun p z x = let val (p',_) = F (z, fn z => (p z,u z)) in p' x end
  and u z x = let val (_,u') = F (z, fn z => (p z,u z)) in u' x end
in
  (p z,u z) : 'a PU
end

fun alt [p] = p
  | alt ps =
let
  val n = Word32.fromInt (length ps) - 0w1
  val vps = Vector.fromList ps
  val enc = #1 (ord n)
  val dec = #2 (ord n)
in
(
  fn (x,s) =>
  let
    fun try ((p,_)::ps, i, e) = (p (x, enc (i,s)) handle Bind  => try (ps, i+0w1, e)
                                                       | Match => try (ps, i+0w1, e)
						       | Option => try (ps, i+0w1, e))
      | try ([], i, e) = raise Pickle ("alt_" ^ Word32.toString i ^ ":" ^ exnMessage e ^ "; " ^ hd (SMLofNJ.exnHistory e))
  in
    try (ps, 0w0, Pickle "alt")
  end,

  fn s =>
  let
    val (i,s') = dec s
    val (_,u) = (* List.nth(ps, Word32.toInt i) *)
	       Vector.sub(vps, Word32.toInt i) 
      handle Subscript => raise Unpickle "alt"
  in
    u s'
  end
)
end

fun alttag t [p] = p
  | alttag t ps =
let
  val n = Word32.fromInt (length ps) - 0w1
  val vps = Vector.fromList ps
  val enc = #1 (ord n)
  val dec = #2 (ord n)
in
(
  fn (x,s) => let val i = t x val (p,_) = Vector.sub(vps, i) in p (x, enc(Word32.fromInt i, s)) end,

  fn s =>
  let
    val (i,s') = dec s
    val (_,u) = (* List.nth(ps, Word32.toInt i) *)
	       Vector.sub(vps, Word32.toInt i) 
      handle Subscript => raise Unpickle "alt"
  in
    u s'
  end
)
end

fun alt' [(_,p)] = p
  | alt' ps =
let
  val freqs = map #1 ps
  val enc = #1 (ord' freqs)
  val dec = #2 (ord' freqs)
in
(
  fn (x,s) =>
  let
    fun try ((_,(p,_))::ps, i, e) = 
        (p (x, enc (Word32.fromInt i,s)) handle (e as Match) => try (ps, i+1, e))
      | try ([], i, e) = raise Pickle ("alt_" ^ Int.toString i ^ ":" ^ exnMessage e ^ "; " ^ hd (SMLofNJ.exnHistory e))
  in
    try (ps, 0, Pickle "alt'")
  end,

  fn s =>
  let
    val (i,s') = dec s
    val (_,(_,u)) = List.nth(ps, Word32.toInt i)
      handle Subscript => raise Unpickle "alt'"
  in
    u s'
  end
)
end

fun share { empty : 'm, find : 'm * 'a -> int option, 
	    insert : 'm * 'a * int -> 'm } 
          =
let
  exception Dict of int * 'm and Inv of (int * 'a IntMap.map)

  fun findDict ([], x) = NONE
    | findDict (Dict(supply, m) :: ds, x) = find(m, x)
    | findDict (d :: ds, x) = findDict (ds, x)

  fun findInv ([], i) = raise Unpickle "share"
    | findInv (Inv(supply,m) :: ds, i) = IntMap.find(m,i)
    | findInv (d :: ds, i) = findInv (ds, i)

  fun updateDict ([], x) = [Dict (1, insert(empty, x, 1))]
    | updateDict (Dict (supply, m) :: ds, x) = 
      Dict (supply+1, insert(m, x, supply+1)) :: ds
    | updateDict (d :: ds, x) = d :: updateDict (ds, x)

  fun updateInv ([], x) = [Inv (1,IntMap.insert(IntMap.empty,1,x))]
    | updateInv (Inv (supply,m) :: ds, x) = Inv (supply+1,IntMap.insert(m,supply+1,x)) :: ds
    | updateInv (d :: ds, x) = d :: updateInv (ds, x)
in
  fn (p,u) : 'a PU =>
  (
    fn (x,s as (dicts,v)) => 
    case findDict (dicts, x) of
      SOME i => 
      #1 int (i, s)

    | NONE =>
      let val s = #1 int (0,s)
	  val (dicts',v') = p(x, s)
      in 
        (updateDict(dicts', x), v')
      end,
    fn s =>
    let val (i,s') = #2 int s
    in
      if i=0
      then let 
        val (x,(invs,v)) = u s'
        val invs' = updateInv(invs, x)
      in
        (x, (invs',v))
      end
      else let val (invs,v) = s'
      in 
        (valOf(findInv (invs, i)), s')
      end
    end
  )
end

fun eqshare eq =
let
  fun find ([], x) = NONE
    | find ((y,i)::m, x) = 
      if eq(x,y) then SOME i else find (m, x)
  fun insert (m, x, i) = (x,i)::m
in
  share { empty = [], find = find, insert = insert }
end

fun shareAcyclicRef (p : 'a PU) = 
let val unsharedref = wrap (ref, !) p
in
  eqshare op= unsharedref
end

fun shareCyclicRef null =
let
  val eq = op=
  val empty = []
  fun find ([], x) = NONE
        | find ((y,i)::m, x) = 
	  if eq(x,y) then SOME i else find (m, x)
  fun insert (m, x, i) = (x,i)::m

  exception Dict of int * ('a ref * int) list and Inv of 'a ref list

  fun findDict ([], x) = NONE
    | findDict (Dict(supply, m) :: ds, x) = find(m, x)
    | findDict (d :: ds, x) = findDict (ds, x)

  fun findInv ([], i) = raise Unpickle "share"
    | findInv (Inv xs :: ds, i) = List.nth(xs, length xs-i)
    | findInv (d :: ds, i) = findInv (ds, i)

  fun updateDict ([], x) = [Dict (1, insert(empty, x, 1))]
    | updateDict (Dict (supply, m) :: ds, x) = 
      Dict (supply+1, insert(m, x, supply+1)) :: ds
    | updateDict (d :: ds, x) = d :: updateDict (ds, x)

  fun updateInv ([], x) = [Inv [x]]
    | updateInv (Inv xs :: ds, x) = Inv (x::xs) :: ds
    | updateInv (d :: ds, x) = d :: updateInv (ds, x)
in
  fn (p,u) : 'a PU =>
  (
    fn (x,s as (dicts,v)) => 
    case findDict (dicts, x) of
      SOME i => 
      #1 int (i, s)
    | NONE =>
      let val contents = !x
	  val dicts = updateDict(dicts,x)
	  val s = #1 int (0,(dicts,v))
	  val s = p (contents,s)
      in 
          s
      end,
    fn s =>
    let val (i,s') = #2 int s
    in
      if i=0 
      then 
	let 
	val x = ref null
        val (invs,v) = s'
	val invs = updateInv (invs,x)
	val (contents,(invs,v)) = u (invs,v)
	val  _ = x := contents
      in
        (x, (invs,v))
      end
      else let val (invs,v) = s'
      in 
        (findInv (invs, i), s')
      end
    end
  )
end

end