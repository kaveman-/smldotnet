signature Render =
   sig
	 
      val apply : (Program.v * Program.v list -> Program.v list) ref
      val inline_closure : (Program.v -> Program.v) ref

      val f : (*amb:*)(Real.real * Real.real * Real.real) * 
	      (*lights:*) Program.v array *
	      (*obj:*)Program.obj * (*depth:*)int * (*fov:*)Real.real *
  	      (*wid:*)int * (*ht:*)int *
	      (*file:*)string -> 
	      unit
   end
