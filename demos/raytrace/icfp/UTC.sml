(* Timer framework for benchmarking *)
structure UTC =
struct
   val t1 = ref Time.zeroTime
   val et = ref Time.zeroTime

   fun start_clock()= (t1 := Time.now ())

   fun pause_clock() = 
       let
	   val t2 = Time.now()
           val dt = Time.-(t2,!t1)
       in 
	  et := Time.+(!et,dt)
       end

   fun resume_clock() = t1 := Time.now()

   fun stop_clock()=
     (pause_clock();
      print ("\nTime: " ^ Time.toString (!et) ^ "\n"))	
end

