(* using custom attributes in SML.NET *)
structure Demo = struct 
local   open Classes
in

_classtype {(* a simple nullary attribute instance *)
	    CSharpAttribute(),

	    (* setting a named field of an attribute instance *)
	    CSharpAttribute() where Field = SOME "field-value" end,

	    (* setting a named property of an attribute instance *)
	    CSharpAttribute() 
	    where Property(SOME "property-value") end,
  	    (*NB: in attribute arguments, we use the name of the property, 
	          not the name of the set method *)

	    (* setting both a field and a property ... *)
	    CSharpAttribute() where 
	      Field = SOME "field-value",
	      Property(SOME "property-value") 
            end,

            (* an attribute constructor taking one argument of each
	       permissible attribute argument type *)

	    CSharpAttribute("string",
			    #"A",
			    true,
			    0wxFF:Word8.word,
			    127: Int8.int , 
			    32767:Int16.int,
			    65535:int,
			    200:Int64.int,
			    0.0:Real32.real,
			    0.1:Real.real,
			    Enum.A)
} 
            C() 
with 
end

(* declaring an ML attribute class *)

_classtype MLAttribute(s:string option):System.Attribute() 
with
    ToString () = s
end;

(* attributes may be placed on ... *)

(* a) classes, class constructors and methods, *)
_classtype {MLAttribute("this is a class attribute"),
	    MLAttribute("this is another class attribute")} 
           [] {MLAttribute("this is a constructor attribute")}
	   MLClass()
with
    {MLAttribute("this is a method attribute")} method () = ()							     
end

(* b) interfaces and interface methods *)

_interfacetype {MLAttribute("this is an interface attribute")} 
	   MLInterface
with
   {MLAttribute("this is a method attribute")} methodSig :unit -> unit
end

(* c) delegate classes and constructors *)
_classtype {MLAttribute("this is a delegate class attribute")} []
	   {MLAttribute("this is a delegate constructor attribute")}
	   MLDelegate of unit -> unit

fun main ()= 
    ((* retrieving attributes, eg. from a class *)
     let val SOME (ty:System.Type) = MLClass().#GetType()
	 val inherit = false
	 val SOME (atts:object option array) = (ty:>System.Reflection.MemberInfo).#GetCustomAttributes(inherit) 
     in
	 Array.app (fn (SOME att) => print ("\n"^(valOf(att.#ToString())))) atts
     end;
     print "\ndone!")
end
end










