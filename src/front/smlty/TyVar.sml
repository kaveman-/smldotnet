(*======================================================================*)
(* Sorted/overloaded type variables					*)
(*======================================================================*)
structure TyVar :> TYVAR = 
struct

val showTyVarSort = Controls.add false "showTyVarSort"

datatype Sort =
  Normal of TySort.Sort
| Overloaded of TyName.Set.set

(*----------------------------------------------------------------------*)
(* Type variables are tagged with their sorts.			        *)
(* (Section 4.1, p16 Defn).						*)
(*----------------------------------------------------------------------*)
datatype TyVarName = Explicit of Syntax.symbol | Implicit of int
type TyVar = TyVarName * Sort

local open Pickle in

val sortpickler = 
alttag (fn Normal _ => 0 | Overloaded _ => 1)
[
  wrap (Normal, fn Normal x => x) TySort.pickler,
  wrap (fn tns => Overloaded (TyName.Set.addList(TyName.Set.empty, tns)), 
        fn Overloaded s => TyName.Set.listItems s)
    (list TyName.pickler)
]

val tyvarnamepickler = 
alttag (fn Explicit _ => 0 | Implicit _ => 1)
[
  wrap (Explicit, fn Explicit x => x) IdPickle.id,
  wrap (Implicit, fn Implicit x => x) int
]

val pickler = pair (tyvarnamepickler, sortpickler)

end

structure TyVarOrd = 
  struct
    type ord_key = TyVar

    fun compare ((Explicit s1,_),(Explicit s2,_)) = Symbol.Key.compare(s1,s2)
      | compare ((Implicit s1,_),(Implicit s2,_)) = Int.compare(s1,s2)
      | compare ((Implicit _,_),(Explicit _,_)) = LESS
      | compare ((Explicit _,_),(Implicit _,_)) = GREATER
  end

structure Set = SetFn(TyVarOrd)
structure Map = MapFn(TyVarOrd)

(*----------------------------------------------------------------------*)
(* Given the syntax for an explicit type variable, construct an 	*)
(* representation with the appropriate sort (Eq or Any)			*)
(*----------------------------------------------------------------------*)
fun explicit v = 
  (Explicit v, 
  Normal 
  (if String.sub(UString.toMLString (Symbol.toUString v), 0) = #"'"
   then TySort.eq 
(*   (*@HACK*)
   else
   if String.sub(UString.toMLString (Symbol.toUString v), 0) = #"_"
   then TySort.mono
   (*@end hack*)
*)
   else TySort.any))

(*----------------------------------------------------------------------*)
(* Turn a type variable into a string, adding ' or '' for sorts Any and *)
(* Eq, and putting the sort in parentheses otherwise			*)
(*----------------------------------------------------------------------*)
fun toString (Explicit v,sort) = 
    "'" ^ Id.toString v

  | toString (Implicit x,sort) = 
    (case sort of
      Normal s => (if TySort.<= (s, TySort.eq) then "''" else "'") ^ 
                  (if TySort.<= (s, TySort.mono) then "_" else "")
    | Overloaded tynameset => "'"
      ^ (if Controls.get showTyVarSort 
        then "{" ^ Pretty.simpleVec "," TyName.toString (TyName.Set.listItems tynameset)
          ^ "}" else "")) ^ Pretty.indexToString (x+26)

fun isExplicit (Explicit _,_) = true
  | isExplicit _ = false

fun toInt (Explicit _,_) = NONE
  | toInt (Implicit x,_) = SOME x

(*----------------------------------------------------------------------*)
(* Return the sort of a type variable					*)
(*----------------------------------------------------------------------*)
fun sort (v,s) = s

(*----------------------------------------------------------------------*)
(* Equality on type variables						*)
(*----------------------------------------------------------------------*)
fun eq (tyvar1, tyvar2) = TyVarOrd.compare(tyvar1, tyvar2) = EQUAL

type Supply = int

(*----------------------------------------------------------------------*)
(* Generate a new type variable with the specified sort			*)
(*----------------------------------------------------------------------*)
fun fresh sort supply = 
  ((Implicit supply, sort), supply+1)

val initial = 0


end
