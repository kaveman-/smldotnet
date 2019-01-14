(*======================================================================*)
(* Variable names and environments for MIL types and terms		*)
(* See signature for more details 					*)
(*======================================================================*)
structure Var :> VAR =
struct

type Var = int
type Supply = int

structure Set = IntBinarySet
structure Map = IntBinaryMap

val dummy = 0
fun isDummy v = v=0
val initial = 1
fun fresh supply = (supply+1, supply)

fun eq (v1:Var,v2:Var) = v1=v2

fun extend (env, vs) = 
  foldr (fn ((v,x),env) => Map.insert(env, v, x)) env vs

fun toString v = if v=0 then "*" else Pretty.indexToString (v-1)

fun index v = v
fun supplyIndex v = v
fun fromInt i = i
fun supplyFromInt i = i

fun lookup (env, v) = 
  case Map.find(env, v) of
    NONE => raise Fail ("Var.lookup: variable " ^ toString v ^ " not found")
  | SOME x => x

fun hash i = Word.fromInt i

val pickler = Pickle.int
val supplyPickler = Pickle.int

end
