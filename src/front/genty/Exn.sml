(*======================================================================*)
(* Exception names							*)
(*======================================================================*)
structure Exn :> EXN =
struct

(*----------------------------------------------------------------------*)
(* Roughly speaking, exceptions are just type names, either imported    *)
(* (so classes) or ML-style (so generated at the point of definition)   *)
(* But to make it easier to represent effects containing sets of	*)
(* exceptions partly closed under subtyping, we record the class 	*)
(* hierarchy in the exception, starting at a subclass of Exception and  *)
(* ending at the exception itself.					*)
(* 									*)
(* Examples: 								*)
(*    exn = System.Exception						*)
(*      is []								*)
(*    exception E           						*)
(*      is [E]								*)
(*    System.IndexOutOfRangeException is				*)
(*      is [SystemException, CoreException, IndexOutOfRangeException]	*)
(*									*)
(* A set of exceptions is then represented by an implicit "root"	*)
(* and a datatype Set (see below). The base case is root=exn.		*)
(*									*)
(* Formally, the set of exceptions denoted by S:Set in the context of   *)
(* the root E:Exn is given by meaning(E, S) defined as:			*)
(*    meaning(E, Only N) = meaning(E, N)				*)
(*    meaning(E, Except N) = down_closure(E) \ meaning(E, N)		*)
(*    meaning(E, {(e_1,S_1),...,(e_n,S_n)} =				*)
(*      Union_i { meaning(E.e_i, S_i) }					*)
(*									*)
(* Examples (where * denotes down-closure):				*)
(*     {}	is Only {}						*)
(*     exn*	is Except {}						*)
(*     E*	is Only {(E, Except {})}				*)
(*									*)
(* BEWARE: run-time generative exceptions are not distinguished.        *)
(*----------------------------------------------------------------------*)
structure M = TyName.Map

type Exn = TyName.TyName list
datatype Set = Only of Nodes | Except of Nodes
withtype Nodes = Set M.map

(* Make an exception from its inheritance chain *)
fun exn (e : TyName.TyName list) = e

(* Extract the type name representing the exception itself *)
fun name e = if List.null e then TyNames.exnTyName else List.last e

(* Equality on exceptions: only need check the final name *)
fun eq (e1,e2) = TyName.eq(name e1, name e2)

fun compare (e1, e2) = TyName.Map.Key.compare(name e1, name e2)

fun toString e = TyName.toString (name e)

(* Subclass test is just prefix test: is e2 a prefix of e1 *)
fun sub (e1, e2) = 
let
  fun match ([], _) = true
    | match (t1::ts1, t2::ts2) =
      TyName.eq(t1,t2) andalso match (ts1,ts2)
    | match _ = false
in
  match (e2, e1)
end


(* Hashing of exceptions *)
fun hash [] = 0w1
  | hash (t::ts) = Gen.combine(TyName.hash t, hash ts)

(* Pickling of exceptions *)
val pickler = Pickle.list TyName.pickler

(*----------------------------------------------------------------------*)
(* Set operations							*)
(*----------------------------------------------------------------------*)
val empty = Only M.empty

fun isEmpty (Only N) = M.isEmpty (M.filter (not o isEmpty) N)
  | isEmpty (Except N) = false

(* singleton(E) returns the set containing E and its subclasses *)
fun singleton [] = Except M.empty
  | singleton (e::E) = Only (M.singleton (e, singleton E))

(* add(S, E) adds E and its descendants to the set S *)
(* Both S and E must be represented relative to the same root *)
fun add (_, []) = 
    (* Add everything under the root, leaving everything *)
    Except M.empty

  | add (Only N, e::E) =
    Only (M.insert(N, e,
      case M.find(N, e) of
        NONE => singleton E
      | SOME S => add (S, E)))

  | add (Except N, e::E) =
    Except (
      case M.find(N, e) of
        NONE => N
      | SOME S => M.insert(N, e, remove (S, E)))

(* remove(S, E) removes E and its descendants from the set S *)
(* Both S and E must be represented relative to the same root *)
and remove (_, []) = 
    (* Remove everything under the root, leaving nothing *)
    empty

  | remove (Only N, e::E) =
    Only (
      case M.find(N, e) of
        NONE => N
      | SOME S => M.insert(N, e, remove (S, E)))

  | remove (Except N, e::E) =
    Except (M.insert(N, e,
      case M.find(N, e) of
        NONE => singleton E
      | SOME S => add (S, E)))

(* Set membership *)
fun member (Only _, []) = false
  | member (Except _, []) = true
  | member (Only N, e::E) =
    (case M.find(N, e) of
      NONE => false
    | SOME S => member (S, E))

  | member (Except N, e::E) =
    (case M.find(N, e) of
      NONE => true
    | SOME S => not (member (S, E)))

fun unionNodes (N1, N2) = M.unionWith union (N1, N2)
and diffNodes (N1, N2) = 
  M.mapi (fn (e,S) => 
    case M.find(N2, e) of
      NONE => S
    | SOME S' => difference(S,S')) N1

and intersectNodes (N1, N2) = M.intersectWith intersection (N1, N2)
    
(* Set union *)
and union (Only N1, Only N2) = 
    Only (unionNodes (N1, N2))

(* SL: or *)
(*
  | union ((Only N1, Except N2) | (Except N2, Only N1)) =
    Except (diffNodes (N2, N1))
*)

  | union (Only N1, Except N2) =
    Except (diffNodes (N2, N1))

  | union (Except N2, Only N1) =
    Except (diffNodes (N2, N1))

  | union (Except N1, Except N2) =
    Except (intersectNodes (N1, N2))
   
(* Set difference *)
and difference (Only N1, Only N2) =
    Only (diffNodes (N1, N2))

  | difference (Except N1, Except N2) =
    Only (diffNodes (N2, N1))

  | difference (Only N1, Except N2) =
    Only (intersectNodes (N1, N2))

  | difference (Except N1, Only N2) =
    Except (unionNodes (N1, N2))
    
and intersection (Only N1, Only N2) = 
    Only (intersectNodes (N1, N2))

(* SL: or *)
(*
  | intersection ((Only N1, Except N2) | (Except N2, Only N1)) =
    Only (diffNodes (N1, N2))
*)
  | intersection (Only N1, Except N2) =
    Only (diffNodes (N1, N2))

  | intersection (Except N2, Only N1) =
    Only (diffNodes (N1, N2))

  | intersection (Except N1, Except N2) = 
    Except (unionNodes (N1, N2))

(* @AJKTODO: optimize this *)
fun isSubset (S1, S2) = isEmpty (difference (S1, S2))

(* Diagnostic pretty printing *)
fun setToString S =
let
  fun str (root, Only N) = 
      Pretty.simpleVec "," str (M.listItemsi N)
    | str (root, Except N) =
      if M.isEmpty N then TyName.toString root
      else TyName.toString root ^ "\\" ^ 
      "{" ^ Pretty.simpleVec "," str (M.listItemsi N) ^ "}"
in
  "{" ^ str (TyNames.exnTyName, S) ^ "}"
end

(* Hashing of sets of exceptions *)
fun hashSet (Only P) = Gen.combine(0w1, hashNodes P)
  | hashSet (Except P) = Gen.combine(0w2, hashNodes P)

and hashNodes P = M.foldri (fn (e,S,w) => Gen.combine(TyName.hash e,
  Gen.combine(hashSet S, w))) 0w1 P

(* Pickling of sets of exceptions *)
local open Pickle in
val setPickler = fix (fn set =>
alttag (fn Only _ => 0 | Except _ => 1)
[
  wrap (Only, fn Only p => p) (TyName.mapPickler set),
  wrap (Except, fn Except p => p) (TyName.mapPickler set)
])
end

end
