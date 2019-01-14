(* Sample code illustrating the modelling of null values using option types *)
structure option_demo =
struct

fun main () =
let
  (* has type string option array option *)
  val stroptarropt = SOME(Array.fromList [SOME "One", SOME "Two", SOME "Three"])

  (* has type string option *)
  val stropt = System.String.Join(SOME ",", stroptarropt)
in
  print (valOf (stropt))
end

end