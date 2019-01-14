(*======================================================================*)
(* Pretty printing auxiliary functions					*)
(*======================================================================*)
structure Pretty :> PRETTY =
struct

val indentUnit = ref 2

(*----------------------------------------------------------------------*)
(* Turn a binding depth (or any other index) into an alphabetic id.     *)
(*----------------------------------------------------------------------*)
fun indexToString m =
if m < 0 then indexToString (~m) ^ "'"
else
if m < 26 then Char.toString (Char.chr (Char.ord #"a" + m))
else indexToString (m div 26 - 1) ^ indexToString (m mod 26)

(*----------------------------------------------------------------------*)
(* Turn a non-negative integer into an alphabetic id.                   *)
(*----------------------------------------------------------------------*)
fun indexToAlpha m =
if m < 26 then Char.toString (Char.chr (Char.ord #"a" + m))
else if m < 52 then Char.toString (Char.chr (Char.ord #"A" + m-26))
else indexToAlpha (m div 52 - 1) ^ indexToAlphaNum (m mod 52)

and indexToAlphaNum m =
if m < 26 then Char.toString (Char.chr (Char.ord #"a" + m))
else if m < 52 then Char.toString (Char.chr (Char.ord #"A" + m-26))
else if m < 62 then Char.toString (Char.chr (Char.ord #"0" + m-52))
else indexToAlphaNum (m div 62 - 1) ^ indexToAlphaNum (m mod 62)

(*----------------------------------------------------------------------*)
(* Turn a non-negative integer into a lowercase alphabetic id.          *)
(*----------------------------------------------------------------------*)
fun indexToLower m =
if m < 26 then Char.toString (Char.chr (Char.ord #"a" + m))
else indexToLower (m div 26 - 1) ^ indexToLowerNum (m mod 26)

and indexToLowerNum m =
if m < 26 then Char.toString (Char.chr (Char.ord #"a" + m))
else if m < 36 then Char.toString (Char.chr (Char.ord #"0" + m-26))
else indexToLowerNum (m div 36 - 1) ^ indexToLowerNum (m mod 36)

(*----------------------------------------------------------------------*)
(* Turn a list of items into a string.					*)
(*----------------------------------------------------------------------*)
fun vec (empty,left1,right1,left2,right2,sep) f [] = 
    empty

  | vec (empty,left1,right1,left2,right2,sep) f [x] = 
    left1 ^ f x ^ right1

  | vec (empty,left1,right1,left2,right2,sep) f (x::xs) = 
    let fun separate [] = ""
          | separate (x::xs) = sep ^ f x ^ separate xs
    in
      left2 ^ f x ^ separate xs ^ right2
    end

fun simpleVec sep = vec ("","","","","",sep)

(*----------------------------------------------------------------------*)
(* Make a newline string consisting of a newline character and 		*)
(* indentation to a depth of n.                                         *)
(*----------------------------------------------------------------------*)
fun newline n = 
  let val m = n * !indentUnit
  in
    CharVector.tabulate(m+1, fn 0 => #"\n" | _ => #" ")
  end

(*----------------------------------------------------------------------*)
(* Pretty print a list/set/map with a separate line for each element	*)
(*----------------------------------------------------------------------*)
fun bigVec depth =   
  vec (
    "{}", 
    newline depth ^ "{" ^ newline (depth+1), 
    newline depth ^ "}", 
    newline depth ^ "{" ^ newline (depth+1), 
    newline depth ^ "}", 
    "," ^ newline (depth+1))

(*----------------------------------------------------------------------*)
(* Enclose with parentheses if b is true.        			*)
(*----------------------------------------------------------------------*)
fun parens b s = if b then "(" ^ s ^ ")" else s


end
