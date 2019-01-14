structure Lib =
struct

exception Not_found
exception Failure of string

fun failwith s = raise(Failure s)

fun for(a: int, b, f) =
   let
      fun loop a =
	 if a > b
	    then ()
	 else (f a; loop(a + 1))
   in loop a
   end

fun forDown(b: int, a, f) =
   let
      fun loop b =
	 if b < a
	    then ()
	 else (f b; loop(b - 1))
   in loop b
   end

(* A hack for hash tables with string domain where performance doesn't matter. *)
structure Hashtbl:
   sig
      type ('a, 'b) t

      val add: ('a, 'b) t -> string -> 'b -> unit
      val create: int -> ('a, 'b) t
      val find: ('a, 'b) t -> string -> 'b
   end =
   struct
      datatype ('a, 'b) t = T of (string * 'b) list ref
	    
      fun create _ = T (ref [])

      fun add (T t) k d = t := (k, d) :: !t


      fun find (T (ref t)) k =
	 case List.find (fn (k', _) => k = k') t of
	    NONE => raise Not_found
	  | SOME(_, d) => d
   end
end





