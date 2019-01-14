(* extremely basic interface examples *)
structure Demo = struct 
local

   (* declaring an interface *)
   _interfacetype ISetter
   with
       set:int -> unit
   end

   (* sugar *)
   _interfacetype IGetter 
   with
       get:unit -> int
   end

   (* inheritance *)
   _interfacetype IProperty : ISetter, IGetter
   with
       say:unit -> unit
   end

   _classtype C(x:int) : IProperty 
   with local
        val r = ref x
   in
       (* implements ISetter.set() *)
       set(i:int) = (print ("\nset " ^ Int.toString i);
		     r := i)
       (* implements IGetter.get() *)
   and get() = (print ("\nget");
		!r)
       (* implements IProperty.say() *)       
   and say () = print (Int.toString (!r))
   end

in
fun main(a:string option array option) = 
     let 
	 (* try it out! *)
	 val c = C(1);
	     
	 (* casting to an interface type *)
	 val i = c :> IProperty
	     
	 (* invoking on an interface value*)
	 val 1 = i.#get()
	     
	 (* casting from an interface value to some class *)
	 val d = i :> C
	     
	 (* invoking directly through the class *)
	 val 1 = d.#get()
	     
	 val _ = c.#set(2)
	     
	 val 2 = d.#get()
	     
	 val true = d.#Equals(c) 
	     
	 (* implicit inheritance of object members at interface types *)
	 val true = i.#Equals(i) 
	     
     in print "\ndone" 
     end
end
end

