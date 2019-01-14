(* Types:>TYPES structure defines the possible Java Types. *)
signature TYPES=
sig
   datatype base_type=BOOLEAN|BYTE|CHAR|SHORT|INT|LONG|FLOAT|DOUBLE|
		      CLASS of ClassHandle.Handle
   datatype java_type=F of int*base_type
   (* F(0,t) is a t.  F(n,t) for n>=1 is an n-dimensional array of t's. *)
   type java_void_type=java_type option
   (* NONE means VOID, otherwise the same as java_type. *)

   (* comparison on types *)
   structure BaseKey:ORD_KEY where type ord_key=base_type
   structure Key:ORD_KEY where type ord_key=java_type
  

   (* 2 comparison functions *)
   val base_type_equal:base_type*base_type->bool
   val java_type_equal:java_type*java_type->bool
   (* java_type_size returns the number of words required to store an item
      of this type in locals or on the stack. java_void_type_size is similar 
      but returns 0 for NONE *)
   val java_type_size:java_type->int
   val java_void_type_size:java_type option->int
   
   (* Here are some classification functions *)
   val isnum:java_type->bool (* true for int/long/float/double *)
   val isint:java_type->bool (* true for int *)
   val islong:java_type->bool (* true for int *)
   val issmall:java_type->bool (* true for boolean/byte/char/short *)
   val iscmp:java_type->bool (* true for long/float/double *)
   val isref:java_type->bool (* true for array types and classes *)
   val islogical:java_type->bool (* true for ints and longs *)
   val isloadable:java_type->bool (* true for int/long/float/double/classes/arrays *)
   val isreal:java_type->bool (* true for floats/doubles *)
   val is2:java_type->bool (* true for doubles/longs (IE 2-word quantities) *)

   val bt_issmall:base_type->bool (* like issmall but for base types *)
   val bt_isint:base_type->bool (* ditto, base types *)   

   val widen:java_type->java_type (* if not isshort returns argument otherwise returns int *)
   val drop_dim:java_type->java_type
   (* drop_dim takes an array type and returns the corresponding type of
      the element stored in the array. *)

(*
   val s2base_type:string->base_type option
   (* s2base_type attempts to parse a string as a base_type, where the
      string is in the form "[typename]" or "[classname]", eg
      "double" or "java/lang/Object".  The double quotes are NOT included. *)
*)

   val base_type2s:base_type->string
   (* base_type2s is an APPROXIMATE inverse of s2base_type, for use in
      constructing error messages only! *)

   (* the XXX_toString functions are intended for debugging purposes only and
      should not be used in production code or in any other way relied on. *)
   val base_type_toString:base_type->string
   val java_type_toString:java_type->string
   val java_void_type_toString:java_type option->string
end

  


