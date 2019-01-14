(*======================================================================*)
(* Generic quicksort (from Paulson)					*)
(*======================================================================*)
structure QuickSort =
struct

(*----------------------------------------------------------------------*)
(* cmp is <=								*)
(*----------------------------------------------------------------------*)
fun sort cmp xs =
case xs of
  [] => []
| [x] => xs
| a::bs =>
  let fun partition (left,right,[]) = sort cmp left @ (a :: sort cmp right)
        | partition (left,right, x::xs) =
          if cmp (x,a) then partition (x::left, right, xs)
                    else partition (left, x::right, xs)
  in  
    partition([],[],bs)  
  end

end
