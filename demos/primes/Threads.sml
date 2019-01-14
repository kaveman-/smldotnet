structure Threads :> 
sig

type 'a Chan
val send : 'a Chan -> 'a -> unit
val receive : 'a Chan -> 'a
val newChan : unit -> 'a Chan
val fork : (unit->unit)->unit
end
 =
struct

type 'a Chan = ('a Queue.Queue) ref * System.Object

fun newChan () = (ref Queue.emptyqueue,System.Object())

fun send (r,m) v = let val _ = System.Threading.Monitor.Enter(m)
                       val q = !r
                       val q' = Queue.add(v,q)
                       val _ = r := q'
                       val _ = System.Threading.Monitor.Pulse(m)
                   in System.Threading.Monitor.Exit(m)
                   end

fun read (r,m) = let val q = !r
                 in if Queue.isempty q then 
                       (System.Threading.Monitor.Wait(m);
                        read (r,m))
                    else let val (v,q') = Queue.remove(q)
                         in (r := q' ; v)
                         end
                  end

fun receive (r,m) = let val _ = System.Threading.Monitor.Enter(m)
                        val v = read (r,m)
                    in (System.Threading.Monitor.Exit(m);
                        v)
                    end

fun fork f = let val ts = System.Threading.ThreadStart(f)
                 val t = System.Threading.Thread(ts)
             in t.#Start()
             end

end
