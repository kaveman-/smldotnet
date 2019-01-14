structure Timer :> TIMER =
struct
     type cpu_timer = Time.time
     type real_timer = Time.time
     fun startCPUTimer () = Time.now ()
     fun startRealTimer () = Time.now ()
     fun totalCPUTimer () = Time.now ()
     fun totalRealTimer () = Time.now ()

     fun checkRealTimer t = Time.-(Time.now (), t)
     fun checkCPUTimer t =
     { usr = Time.-(Time.now (), t), sys = Time.zeroTime, gc = Time.zeroTime }
end
