(*======================================================================*)
(* Pretty printing combinators						*)
(* Based on "A prettier printer", Phil Wadler, "The Fun of Programming" *)
(*======================================================================*)
structure NewPretty :> NEWPRETTY =
struct

(* Need to repeat these declarations at point of use *)
infixr 6 ++
infixr 6 +/
infixr 5 //

val fast = true

(* Underlying datatype of documents *)
datatype DOC = 
  EMPTY 		(* The empty document *)
| TEXT of string	(* A single line *)
| LINE			(* A newline *)
| NEST of int * DOC	(* Nested doc, indented *)
| JOIN of DOC * DOC	(* Concatenation *)
| UNION of DOC * DOC	(* Choice *)
| HTML of string        (* HTML in angle brackets, counts as zero size *)

(* Functions corresponding to the constructors *)
val empty = EMPTY

fun EMPTY ++ y = y
  | x ++ EMPTY = x
  | x ++ y = JOIN(x,y)

fun nest i EMPTY = EMPTY
  | nest i x = NEST(i,x)

fun text s = TEXT(s)

val line = LINE

fun flatten EMPTY = EMPTY
  | flatten (JOIN(x,y)) = JOIN(flatten x, flatten y)
  | flatten (NEST(i,x)) = NEST(i, flatten x)
  | flatten (TEXT s) = TEXT s
  | flatten LINE = TEXT " "
  | flatten (UNION(x,y)) = flatten x
  | flatten (HTML s) = HTML s

fun group x = if fast then x else UNION(flatten x, x)

val rule = HTML "HR" ++ LINE

(* The concatentation of n copies of s *)
fun copy 0 s = ""
  | copy n s = s ^ copy (n-1) s
  
local
  (* A linear representation for documents *)
  datatype Doc = 		
    Empty
  | Text of string * Doc
  | Line of int * Doc
  | Html of string * Doc

  fun layout (pr,html) Empty = ()
    | layout (pr,html) (Text(s,x)) = (pr s; layout (pr,html) x)
    | layout (pr,html) (Line(i,x)) = (pr ("\n" ^ copy i " "); layout (pr,html) x)
    | layout (pr,html) (Html(s,x)) = if html then (pr ("<" ^ s ^ ">"); layout (pr,html) x) else layout (pr,html) x

  fun fits (w, z) = 
    w >= 0 andalso
    case z of
      []		=> true
    | (_,EMPTY)::z	=> fits (w,z)
    | (_,TEXT s)::z	=> fits (w-size s, z)
    | (i,JOIN(x,y))::z	=> fits (w, (i,x)::(i,y)::z)
    | (_,LINE)::z	=> true
    | (_,HTML s)::z	=> fits (w,z)
    | (i,NEST(j,x))::z	=> fits (w,(i,x)::z)
    | (i,UNION(x,y))::z	=> fits (w,(i,x)::z) orelse fits (w,(i,y)::z)

  fun be w k [] = Empty
    | be w k ((i,EMPTY)::z) = be w k z
    | be w k ((i,JOIN(x,y))::z) = be w k ((i,x)::(i,y)::z)
    | be w k ((i,NEST(j,x))::z) = be w k ((i+j,x)::z)
    | be w k ((i,TEXT s)::z) = Text(s, be w (k+size s) z)
    | be w k ((i,HTML s)::z) = Html(s, be w k z)
    | be w k ((i,LINE)::z) = Line(i, be w i z)
    | be w k ((i,UNION(x,y))::z) = if fits (w-k, (i,x)::z) then be w k ((i,x)::z) else be w k ((i,y)::z)

  fun best w k x = be w k [(0,x)]
in
  fun pretty w pr x = layout (pr,false) (best w 0 x)
  fun prettyHtml w pr x = layout (pr,true) (best w 0 x)
end

fun x // y = x ++ line ++ y
fun x +/ y = if fast then x ++ line ++ y else x ++ (UNION(text " ", line)) ++ y

fun bold x = HTML "b" ++ x ++ HTML "/b"
fun italic x = HTML "i" ++ x ++ HTML "/i"
fun coloured c x = HTML ("FONT COLOR=" ^ c) ++ x ++ HTML "/FONT"
fun xdef s x = HTML ("a name=" ^ s) ++ x ++ HTML "/a"
fun xref s x = HTML ("a href=#" ^ s) ++ x ++ HTML "/a"

fun fold f [] = empty
  | fold f [x] = x
  | fold f (x::xs) = f (x,fold f xs)

val spread = fold (fn (x,y) => x ++ text " " ++ y)
val stack = fold op//

fun bracket l x r  = group (text l ++ nest 2 (line ++ x) ++ line ++ text r)

fun fill [] = empty
  | fill [x] = x
  | fill (x::y::zs) = UNION(flatten x ++ text " " ++ fill (flatten y :: zs),
                             x // fill (y::zs))

fun fillWith sep [] = empty
  | fillWith sep [x] = x
  | fillWith sep (x::y::zs) = x ++ text sep ++ fillWith sep (y :: zs)


fun size EMPTY = 1
  | size (JOIN(x,y)) = 1 + size x + size y
  | size (NEST(i,x)) = 1 + size x
  | size (TEXT s) = 1
  | size LINE = 1
  | size (HTML s) = 1
  | size (UNION(x,y)) = size x + size y


end
