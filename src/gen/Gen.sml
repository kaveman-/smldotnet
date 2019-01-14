(*----------------------------------------------------------------------*)
(* Miscellaneous stuff							*)
(*----------------------------------------------------------------------*)
structure Gen =
struct

(*----------------------------------------------------------------------*)
(* The identity function.						*)
(*----------------------------------------------------------------------*)
fun identity x = x

(*----------------------------------------------------------------------*)
(* Constant functions.							*)
(*----------------------------------------------------------------------*)
fun const c = fn x => c

(*----------------------------------------------------------------------*)
(* Combine two hash values						*)
(*----------------------------------------------------------------------*)
fun combine (x,y) = x + y * 0w509

end
