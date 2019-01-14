(* using enumeration types in SML.NET *)
structure Demo = struct 
local
        open Classes

	fun pr s = (print "\n";print s)

	(* enums as types *)

	type enum = MyEnum
	    
	(* the proper, underlying enum constructor *)
	fun fromInt i = MyEnum i
	fun toInt (MyEnum i) = i

	(* using derived enumeration constants *)

	(* enum patterns *)
	fun toString MyEnum.A = "A"
	  | toString MyEnum.B = "B"
	  | toString MyEnum.C = "C" (* this case is redundant ! *)
	  | toString (MyEnum i) = Int.toString i 

	(* enum expressions *)
	fun fromString "A" = MyEnum.A
	  | fromString "B" = MyEnum.B
	  | fromString "C" = MyEnum.C
	  | fromString s = MyEnum (valOf (Int.fromString s))

	(* enums as objects *)

        (* invokation *)
	val "A" = valOf(MyEnum.A.#ToString());

	(* boxing and unboxing casts *)
	val Obj = MyEnum.A :> System.Object
	val Val = MyEnum.A :> System.ValueType
	val a = Obj :> MyEnum
	val a = Val :> MyEnum

        (* some unboxing casts should fail *)
	val nonMyEnum = System.Object()
	val _ = (nonMyEnum:>MyEnum;
		 pr "wrong") 
	         handle (e:>System.InvalidCastException) => pr("ok")
	    
	(* identity casts work too! *)
	val _ = pr (toString (MyEnum.A:>MyEnum))

in
fun main ()= 
    (List.app (pr o toString) [MyEnum.A, MyEnum.B, MyEnum.C, MyEnum ~1];
     pr "done!")
end
end










