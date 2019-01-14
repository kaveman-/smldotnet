(* using storage types in SML.NET *)
structure Demo = struct 
fun main ()= 
    let
	open Classes


  (* [decNonAddress] decrements any reference, *except* those of byref kinds *)
        fun decNonAddress(x:(int,'kind) reference) = (x:=(!x)-1;!x)

  (* [decAddress] decrements any integer address*)
        fun decAddress(x: int &) = (x:=(!x)-1;!x);
       (* NB: int & =(int,address) reference *)  
   
   (* ML references, of kind [heap], behave as expected *)

	type intref =  int ref 
	     (* ie. (int,heap) reference *)  
	val r : intref = ref 0
	val () = r := (!r) + 1
	val ref 1 = r
        (* first-classing an ML ref *)
	val 0 = decNonAddress(r)
	(* taking the address of an ML reference *)
	val ~1 = decAddress(& r)

	(* passing ML refs to C# call-by-reference methods *)
	val (ra,rb) = (ref 2,ref 3)
        val _ = Class.swap(&ra,&rb)     	    
	val (ref 3,ref 2) = (ra,rb)
        
    (* every mutable .NET field [C.id] is imported with
       an associated type constructor [C.id] that abbreviates its
       precise storage type *)

	type t = Class.staticField
	     (* ie.(int,(Class,staticField)static) reference *)

	type u = Class.objectField
	     (* ie. (int,(Class,staticField)field) reference *)

	type v = ValueType.valueField
	     (* ie. (int,(ValueType,staticField)field) reference *)

    

    (* using storage cells of kind [static] *)

	(* reading and writing a static field *)
	val _ = Class.staticField := (!Class.staticField) + 1
	(* using a static field as a first class value *)
	val 0 = decNonAddress(Class.staticField)
        (* taking the address of a static *)
	val ~1 = decAddress(&Class.staticField)

    (* using storage cells of kind [field] *)

	(* reading and writing an instance field of an *object* *)
	val obj = Class()
	val objectField : Class.objectField = obj.#objectField
	val _ = objectField := !objectField + 1
	(* using an instance field as a first class value *)
	val 0 = decNonAddress(objectField)
	(* taking the address of an instance field *)
	val ~1 = decAddress(&objectField)
	(* all the while mutating the original object *)
	val ~1 = !(obj.#objectField)


       (* reading and writing an instance field of a *value* *)
	val value = ValueType.null
	val valueField : ValueType.valueField = value.#valueField
       (* reading a value's field works *)
	val 0 = !(valueField)
       (* writing a value's field, is possible, but has no effect
	  on the original value*)
        val _ = valueField := 1
	val 1 = !(valueField)
	(* taking the address of a value's field *)
	val 0 = decAddress(&valueField)
	val ~1 = decAddress(&valueField)
	(* all the while leaving the original value unchanged *)
	val 0 = !(value.#valueField)

       (* writing the field of some reference to a value can be observed *)
	val valueRef = ref ValueType.null 
	val valueRefField : ValueType.valueField = valueRef.#valueField
	val _ = valueRefField := (!valueRefField) + 1
	val 1 = !valueRefField
	val 1 = !(valueRef.#valueField)

	(* taking the address of a value's field *)
	val 0 = decAddress(&valueRefField)
	val 0 = !(valueRef.#valueField)
	(*NB: instantiating a polymorpic function at a byref type is 
	   potentially unsound, rejected as illegal:
             val wrong = decNonAddress(valueRef.#valueField)
	   rejected as a type error
	 *) 

    in print "done!" 
    end 

end










