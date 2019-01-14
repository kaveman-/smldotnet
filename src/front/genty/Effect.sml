(*======================================================================*)
(* Effect annotations for computation types.				*)
(* See signature for details.						*)
(*======================================================================*)
structure Effect :> EFFECT =
struct

(*----------------------------------------------------------------------*)
(* An effect is a subset of {reads,writes,allocs,io,loops} union exns   *)
(* where exns is the set of all possible exceptions.			*)
(* However, imported exceptions form a class hierarchy so the set is    *)
(* implicitly "down-closed" wrt the subtype ordering.			*)
(*----------------------------------------------------------------------*)
type Effect = word * Exn.Set

val partialMask = 0wx1
val readsMask   = 0wx2
val writesMask  = 0wx4
val ioMask      = 0wx8
val allocsMask  = 0wx10

(* Some singleton effects *)
val partial     = (partialMask, Exn.empty)
val reads       = (readsMask, Exn.empty)
val writes      = (writesMask, Exn.empty)
val io          = (ioMask, Exn.empty)
val allocs      = (allocsMask, Exn.empty)

(* The effect of throwing a particular exception *)
fun throws e    = (0wx0, Exn.singleton e)

(* The effect of throwing any exception (as all exns subclass Exception) *)
val throwsAny   = throws (Exn.exn [])

(* The empty effect *)
val none        = (0wx0, Exn.empty)

(* An unknown effect *)
val any         = (0wx1f, Exn.singleton (Exn.exn []))

(* Empty predicate *)
fun isNone (w,e) = w=0w0 andalso Exn.isEmpty e

fun exns (w,e) = e

(* Union and intersection *)
fun union ((w1,e1), (w2,e2)) = 
  (Word.orb (w1,w2), Exn.union(e1,e2))
fun intersection ((w1,e1), (w2,e2)) = 
  (Word.andb (w1,w2), Exn.intersection(e1,e2))

fun sub ((w1,e1),(w2,e2)) = 
  Word.andb(w1, Word.notb w2) = 0w0 andalso Exn.isSubset(e1,e2)

(* Equality  *)
fun eq ((w1,e1), (w2,e2)) = w1=w2 
  andalso Exn.isSubset (e1,e2) andalso Exn.isSubset (e2,e1)

fun hash (w,e) = Gen.combine (w, Exn.hashSet e)
  
fun toString (w,e) =
  (if Word.andb(w, partialMask) <> 0w0 then "!" else "") ^
  (if Exn.isEmpty e then "" else Exn.setToString e) ^
  (if Word.andb(w, readsMask) <> 0w0 then "r" else "") ^
  (if Word.andb(w, writesMask) <> 0w0 then "w" else "") ^
  (if Word.andb(w, ioMask) <> 0w0 then "i" else "") ^
  (if Word.andb(w, allocsMask) <> 0w0 then "a" else "")

(*----------------------------------------------------------------------*)
(* Pickling								*)
(*----------------------------------------------------------------------*)
val pickler = 
  Pickle.pair (Pickle.wrap (Word.fromInt, Word.toInt) Pickle.int, 
                Exn.setPickler)

end
