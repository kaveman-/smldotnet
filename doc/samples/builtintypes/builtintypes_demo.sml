(* No marshalling required! Demonstrates use of .NET types
   that have direct SML equivalents *)
structure builtintypes_demo =
struct

fun main () =
(
  (* real = System.Double *)
  print ("cosh(1.2) = " ^ Real.toString(System.Math.Cosh(1.2)) ^ "\n");

  (* int = System.Int32 *)
  print ("System.Int32.MinValue = " ^ Int.toString(System.Int32.MinValue) ^ "\n");

  (* char = System.Char *)
  print (valOf(System.Char.GetUnicodeCategory(#"Z").#ToString()))
)

end