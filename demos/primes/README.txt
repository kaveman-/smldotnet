This contains some *very* basic definitions for asynchronous
channel-based concurrency in SML.NET, and a demonstration of their use
in computing primes (in a rather silly way - see Primes.sml). 

To build and run the demo, do
 make Primes
 run
from within the SML.NET environment, or
 build
 Primes
from the command line.

See Threads.sml for the concurrency operations. Send is
non-blocking. Spawn generates a new OS thread.

TODO: replace this with a proper set of CML-style primitives, a la Thimble.
