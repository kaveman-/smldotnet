(*======================================================================*)
(* Integer quicksort                					*)
(*======================================================================*)
structure Sort :> 
sig
  val main : unit -> unit
end
=
struct

(*----------------------------------------------------------------------*)
(* Print a list of numbers 						*)
(*----------------------------------------------------------------------*)
fun printList [] = ()
  | printList ((x:int)::xs) = 
    (print (Int.toString x); print " "; printList xs)


(*----------------------------------------------------------------------*)
(* Quicksort an integer list.                                           *)
(* This is the solution to Exercise 3.38 posed in Paulson, ML for the   *)
(* Working Programmer, second edition.                                  *)
(*----------------------------------------------------------------------*)
fun quick (xs:int list) =
let
  (* Sort xs and append ys to it *)
  fun quicker (xs, ys) =
    case xs of
      [] => ys
    | [x] => x::ys
    | a::bs =>
      let 
        fun partition (left,right,[]) = quicker (left, a::quicker (right, ys))
          | partition (left,right,x::xs) =
            if (x:int) < a then partition (x::left, right, xs)
                   else partition (left, x::right, xs)
      in  
        partition([],[],bs)  
      end
in
  quicker (xs, [])
end

fun make 0 = []
  | make n = Int.rem(n*23, 100) :: make (n-1)

(* The main function *)
fun main () = 
let
  val args = CommandLine.arguments ()
  val nitems = if null args then 50 else valOf (Int.fromString (hd args))
  val xs = make nitems
  val ys = quick xs
in
  print "\nBefore sorting: "; printList xs;
  print "\nAfter sorting: "; printList ys;
  print "\n"

end

end 