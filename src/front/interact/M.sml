structure M =
struct
    (* force registration of transformations and some commands by mentioning their structures *)
    local open Arity TailRec Case Deunit Equality Flatten 
	       Inline Monomorphise FloatHoist FunScope 
	       TypeCommand
    in
    end

    fun m () = TopLevel.entry("",[])
    fun main (_ : string option array option) = ignore (m ())
    end
val m = M.m;
fun root () = OS.FileSys.chDir  (OS.Path.toString{isAbs=true,vol="C:",arcs=["ml"]});
