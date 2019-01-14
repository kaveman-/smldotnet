structure fields_demo =
struct

fun main () =
let
  val c = Fields.C("Universe", 500)
in
  print ("Value of C.static_const_field is " ^ Int.toString(Fields.C.static_const_field) ^ "\n");
  print ("Value of C.static_readonly_field is " ^ valOf(Fields.C.static_readonly_field) ^ "\n");
  print ("Value of C.static_field is " ^ Int.toString(!Fields.C.static_field) ^ "\n");
  Fields.C.static_field := 25;
  print ("Value of C.static_field is " ^ Int.toString(!Fields.C.static_field) ^ "\n");
  print ("Value of c.inst_readonly_field is " ^ valOf(c.#inst_readonly_field) ^ "\n");
  print ("Value of c.inst_field is " ^ Int.toString(!(c.#inst_field)) ^ "\n");
  c.#inst_field := 501;
  print ("Value of c.inst_field is " ^ Int.toString(!(c.#inst_field)) ^ "\n")

end


end