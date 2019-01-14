(* Illustrates use of structures to model .NET
   namespaces, classes and nested classes *)
structure structures_demo =
struct

  (* First, let's open the System namespace *)
  local 
    open System 

    (* Now let's rebind a class as a structure *)
    structure M = Math

    (* We can also rebind a nested namespace as a structure *)
    structure D = Drawing

  in
    fun main () =
    (
      (* The static field System.Math.PI is now in M *)
      print ("PI = " ^ Real.toString M.PI ^ "\n");

      (* The static method System.Drawing.Color.get_Red is now in D.Color *)
      print ("Red = " ^ valOf(D.Color.get_Red().#ToString()) ^ "\n")
    )
  end
end
  
     
    
    