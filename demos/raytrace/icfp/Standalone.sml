structure Standalone =
struct
fun main (a : string option array option) =
     let val _ = Config.ppmDir := NONE
         val _ = UTC.start_clock()
         val _ = UTC.pause_clock()
	 val p = Program.read TextIO.stdIn
         val _ = UTC.resume_clock()
     in
        Eval.f p;
        UTC.stop_clock() 
     end	
end