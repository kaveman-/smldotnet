(* Demonstration of arrays in C# and SML.NET *)
structure arrays_demo = 
struct

fun test_rev () =
let
  (* An int array *)
  val intarr = Array.fromList [1,2,3,4,5]
in
  (* Now reverse it in place using C# *)
  Arrays.Utils.Reverse(intarr);
  Array.app (fn i => print (Int.toString i ^ ";")) intarr;
  print "\n"
end

fun test_slice (start, n) =
(let
  (* A string option array *)
  val stroptarr = Array.fromList [SOME "One",SOME "Two",SOME "Three",SOME "Four"]

  (* Invoke slice method *)
  val stroptarr2 = valOf (Arrays.Utils.Slice(stroptarr, start, n))
in
  Array.app (fn stropt => print (valOf (stropt) ^ ";")) stroptarr2;
  print "\n"
end) handle Subscript => print "Subscript was raised\n"

fun main () =
(
  (* Test Arrays.Utils.Reverse on an integer array *)
  test_rev ();

  (* Test Arrays.Utils.Slice on a string array *)
  test_slice (1,2);

  (* Test Arrays.Utils.Slice on a string array, out of bounds *)
  test_slice (3,4)
)

end