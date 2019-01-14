(*======================================================================*)
(* Short identifiers. Used for						*)
(* - ML identifiers (e.g. labels, type variables, ... 			*)
(* - external identifiers (class names, member names, assembly names)   *)
(*======================================================================*)
structure Id =
struct

open Symbol

type id = Symbol.symbol

(* Convert an identifier into an ML string *)
fun toString id = UString.toMLString (Symbol.toUString id)

val fromString = Symbol.symbol o UString.fromString

val 

  [falseSym, trueSym,
   nilSym, consSym,
   thisSym,
   refSym, assignSym, derefSym, addressSym,
   eqSym, neSym,
   dummySym,
   publicSym, privateSym, protectedSym, abstractSym,
   finalSym, staticSym, interfaceSym, transientSym,initonlySym,
   volatileSym, synchronizedSym, nativeSym,
   exnSym,
   null (* null value type values *),
   sealedSym
] 

= map fromString

  ["false", "true",
   "nil", "::",
   "this", 
   "ref", ":=", "!", "&",
   "=", "<>",
   "_$",
   "public", "private", "protected", "abstract",
   "final", "static", "interface", "transient","initonly",
   "volatile", "synchronized", "native",
   "exn",
   "null",
   "sealed"]

(*----------------------------------------------------------------------*)
(* Given a list of atoms xs return a list of the duplicate elements.    *)
(*----------------------------------------------------------------------*)
fun duplicateAtoms xs =
let
  fun check [] (set, dups) = dups
    | check (x::xs) (set, dups) =
      if Symbol.Set.member(set, x)
      then check xs (set, Symbol.Set.add(dups, x))
      else check xs (Symbol.Set.add(set, x), dups)
in
  Symbol.Set.listItems (check xs (Symbol.Set.empty, Symbol.Set.empty))
end

(*----------------------------------------------------------------------*)
(* Given a list of (atom,value) pairs return the duplicates as a list   *)
(* of (atom,value_list) pairs.                                          *)
(*----------------------------------------------------------------------*)
fun duplicateAtoms' xs =
let
  fun addValue (dups, a, v) =
  case Symbol.Map.find(dups, a) of
    NONE => Symbol.Map.insert(dups, a, [v])
  | SOME vs => Symbol.Map.insert(dups, a, v::vs)
    
  fun check [] (set, dups) = dups
    | check ((a,v)::xs) (set, dups) =
      if Symbol.Set.member(set, a)
      then check xs (set, addValue(dups, a, v))
      else check xs (Symbol.Set.add(set, a), dups)
in
  Symbol.Map.listItemsi 
  (check xs (Symbol.Set.empty, Symbol.Map.empty))
end

(*----------------------------------------------------------------------*)
(* Given a symbol map, produce a list with session-independent ordering	*)
(* (in fact, alphabetic). 						*)
(*----------------------------------------------------------------------*)
fun fixMap m =
  QuickSort.sort 
  (fn ((x,_),(y,_)) => 
    UString.compare(Symbol.toUString x, Symbol.toUString y)
    <> GREATER) (Symbol.Map.listItemsi m)


end
