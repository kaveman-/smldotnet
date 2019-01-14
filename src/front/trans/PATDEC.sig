(*======================================================================*)
(* Pattern matching decision tree type and operations			*)
(*======================================================================*)
signature PATDEC =
sig

type env = PatPath.Path Symbol.Map.map

(* SL: withtype (decision) *)
datatype dec = 
  (* Match failed *)
  Failure

  (* Test some part of the object against a constructor *)
| IfEq of PatPath.Path * PatCon.con * 
({funvar:MILTerm.BoundVar, refs:int ref, term:MILTerm.Cmp option ref, tree:dec} ref) *
({funvar:MILTerm.BoundVar, refs:int ref, term:MILTerm.Cmp option ref, tree:dec} ref)

  (* Success (e,paths): match succeeded with equation number e and with 
     variables bound to the paths given. Equations are numbered from zero. *)
| Success of int * env

type decision =
{funvar:MILTerm.BoundVar, refs:int ref, term:MILTerm.Cmp option ref, tree:dec} ref

structure Hash : MONO_HASH_TABLE where type Key.hash_key = dec

val incrnode : decision -> unit
val mkDecision : dec -> decision
val shared : decision -> bool
val unique : decision Hash.hash_table -> dec -> decision
val used : decision -> bool

(* Pretty printer for decision DAGs *)
val toString : decision -> string

(* Return set of possible matches and existence of failure nodes *)
val tally : decision -> IntSet.set * bool

end