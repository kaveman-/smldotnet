(*======================================================================*)
(* Naive detection of duplicates in a list				*)
(*======================================================================*)
structure Dups =
struct

(*----------------------------------------------------------------------*)
(* Given a list and an equality function, return a list of elements	*)
(* that are duplicated.                                                 *)
(*----------------------------------------------------------------------*)
fun duplicates eq xs =
let
  fun gather ([], dups) = dups
    | gather (x::xs, dups) = 
      if List.exists (fn x' => eq(x,x')) dups 
      then gather (xs, dups)
      else if List.exists (fn x' => eq(x,x')) xs
      then gather (xs, x::dups)
      else gather (xs, dups)
in
  gather (xs, [])
end

(*----------------------------------------------------------------------*)
(* Given a list of strings xs return a list of the duplicate elements.  *)
(*----------------------------------------------------------------------*)
fun duplicateStrings xs =
let
  fun check [] (set, dups) = dups
    | check (x::xs) (set, dups) =
      if StringSet.member(set, x)
      then check xs (set, StringSet.add(dups, x))
      else check xs (StringSet.add(set, x), dups)
in
  StringSet.listItems (check xs (StringSet.empty, StringSet.empty))
end

(*----------------------------------------------------------------------*)
(* Remove duplicates using given equality function			*)
(*----------------------------------------------------------------------*)
fun removeDups eq [] = []
  | removeDups eq (x::xs) =
    if List.exists (fn x' => eq(x,x')) xs then removeDups eq xs
    else x :: removeDups eq xs


end
