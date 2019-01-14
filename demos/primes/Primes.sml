(* Pipe of Erastothenes
   After:
   Ian Stark. Thimble: Threads for MLj. Proceedings of the First Scottish 
   Functional Programming Workshop. 1999. 
   http://www.dcs.ed.ac.uk/~stark/publications/thitfm.html

   Calculate primes using a big chain of filtering processes. Each
corresponds to a prime, receiving numbers on its input and passing on
to its ouput just those numbers which are not divisible by that
prime. When a prime pops out of the end of the chain, it is printed
and a corresponding new thread spawned and spliced into the end of the
chain.

*)

structure Primes :> 
sig
 val main : string option array option -> unit
end
= 
struct
open Threads

fun raw v limit out = (send out (SOME v); 
                       if v>=limit then (send out NONE; print "All sent ")
                       else raw (v+1) limit out)

fun mid v inn out = let val x = receive inn
                    in case x of SOME w => (if (w mod v) = 0 then ()
                                            else (send out x);
                                            mid v inn out)
                               | NONE => send out x
                    end
fun processed inn = let val x = receive inn
                        val c = newChan()
                    in case x of SOME v => (print ((Int.toString v)^" ");
                                            fork (fn () => mid v inn c);
                                            processed c )
                               | NONE => ()
                    end

fun main args = 
 let val c = newChan()
 in fork (fn () => raw 2 200 c);
    processed c
 end
 
end