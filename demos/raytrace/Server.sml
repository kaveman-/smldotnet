structure Server = 
struct
   fun raytrace (factory:Classes.FactoryClass option,filename:string option) =
       let val _ = Image.current_factory := factory
	   val fs = TextIO.openIn (valOf(filename))
	   val p = Program.read fs
	   val _ = TextIO.closeIn fs
       in
	   Eval.f p
       end	
end

