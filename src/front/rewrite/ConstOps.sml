(*======================================================================*)
(* Constant folding							*)
(*======================================================================*)
structure ConstOps :> CONSTOPS =
struct

local 
  open Constants 
in

fun BOOL true = Constants.BOOLEAN (RTInt.fromInt 1)
  | BOOL false = Constants.BOOLEAN (RTInt.fromInt 0)

val binaryfuns =
foldl (fn ((id, funs),m) => Symbol.Map.insert(m, Id.fromString id, funs))
  Symbol.Map.empty
[
  ("add", 
  (SOME o INT o RTInt.numops.add, SOME o LONG o RTLong.numops.add)),

  ("sub",	
  (SOME o INT o RTInt.numops.sub, SOME o LONG o RTLong.numops.sub)),

  ("mul",	
  (SOME o INT o RTInt.numops.mul, SOME o LONG o RTLong.numops.mul)),

  ("xor",	
  (SOME o INT o RTInt.numops.xorb, SOME o LONG o RTLong.numops.xorb)),

  ("or",	
  (SOME o INT o RTInt.numops.orb, SOME o LONG o RTLong.numops.orb)),

  ("And",	
  (SOME o INT o RTInt.numops.andb, SOME o LONG o RTLong.numops.andb)),

  ("div",	
  (Option.map INT o RTInt.numops.div,Option.map LONG o RTLong.numops.div)),

  ("rem",	
  (Option.map INT o RTInt.numops.rem,Option.map LONG o RTLong.numops.rem)),
  
  ("lt",
  (SOME o BOOL o RTInt.numops.lt, SOME o BOOL o RTLong.numops.lt)),

  ("gt",
  (SOME o BOOL o not o RTInt.numops.le, 
   SOME o BOOL o not o RTLong.numops.le)),

  ("le",
  (SOME o BOOL o RTInt.numops.le, SOME o BOOL o RTLong.numops.le)),

  ("ge",
  (SOME o BOOL o not o RTInt.numops.lt, 
   SOME o BOOL o not o RTLong.numops.lt))
]

(*----------------------------------------------------------------------*)
(* Return the compile-time function for the integer and long versions	*)
(* of the operation specified.                                          *)
(*----------------------------------------------------------------------*)
fun whichBinaryFun jop = Symbol.Map.find(binaryfuns, jop)

fun intOf (BOOLEAN x) = SOME (x, BOOLEAN)
  | intOf (BYTE x) = SOME (x, BYTE)
  | intOf (CHAR x) = SOME (x, CHAR)
  | intOf (SHORT x) = SOME (x, SHORT)
  | intOf (INT x) = SOME (x, INT)
  | intOf _ = NONE

(*----------------------------------------------------------------------*)
(* Apply a primtive operation to a list of constant arguments, if	*)
(* possible, or return NONE if the answer isn't known.        		*)
(*----------------------------------------------------------------------*)
fun applyPrim (prim, args) =
case args of
 [LONG x, LONG y] =>
  (case whichBinaryFun prim of
    NONE => NONE
  | SOME (intop, longop) => longop (x,y))

| [x, y] =>
  (case (intOf x, intOf y) of 
    (SOME (x, c), SOME (y, _)) =>
    (case whichBinaryFun prim of
      NONE => NONE
    | SOME (intop, longop) => intop (x,y))
  | _ => NONE)

| _ => NONE

end (* of local open *)

end (* of struct *)

