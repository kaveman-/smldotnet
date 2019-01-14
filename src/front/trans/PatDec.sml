(*======================================================================*)
(* Pattern match compilation; decision trees.				*)
(*======================================================================*)
structure PatDec :> PATDEC =
struct

type env = PatPath.Path Symbol.Map.map

datatype dec =
  Failure			
| Success of int * env
| IfEq of PatPath.Path * PatCon.con * decision * decision 

withtype decision = 
    {tree : dec, refs : int ref, term : MILTerm.Cmp option ref,
     funvar : MILTerm.BoundVar} ref

fun shared (ref {refs as ref count, ...}   : decision) = count > 1
fun used   (ref {refs as ref count, ...}   : decision) = count > 0
fun incrnode (ref {refs as ref count, ...} : decision) = refs := 1 + count
fun mkDecision t = ref {tree = t, refs = ref 0, term = ref NONE, 
  funvar = #1 (TransOps.freshAnonVar ()) }

fun decToString level dec =
case dec of
  Failure => "fail"
| Success (i,env) => "success " ^ Int.toString i ^
  "(" ^ Pretty.simpleVec "," (fn (x,path) => Id.toString x ^ "="
  ^ PatPath.toString path) (Symbol.Map.listItemsi env) ^ ")"
| IfEq(path,c,d1,d2) =>
  Pretty.newline level ^  
  "if " ^ PatPath.toString path ^ " = " ^ PatCon.toString c ^
  Pretty.newline level ^ 
  " then " ^ decisionToString (level+1) d1 ^
  Pretty.newline level ^ 
  " else " ^ decisionToString (level+1) d2

and decisionToString level (ref { tree, ... }) = decToString level tree

val toString = decisionToString 0

fun combine (x,y) = x + y * 0w509

fun hash dec =
case dec of
  Failure => 0w1
| Success (x,_) => Word.fromInt x
| IfEq (access, con, d1, d2) =>
  combine (PatCon.hash con, combine (hashDecision d1, hashDecision d2))
  
and hashDecision (ref {funvar, ...} : decision) = Var.hash(#1 funvar)

fun equal (Failure, Failure) = true
  | equal (Success (x1,e1), Success (x2,e2)) = 
    x1=x2 andalso 
    Eq.list (fn ((x1,p1),(x2,p2)) => Symbol.equal(x1,x2)
    andalso PatPath.equal(p1,p2)) 
      (Symbol.Map.listItemsi e1, Symbol.Map.listItemsi e2)
  | equal (IfEq(acc1, con1, d1a, d1b), IfEq(acc2, con2, d2a, d2b)) =
    d1a=d2a andalso d1b=d2b andalso PatCon.equal(con1, con2) 
    andalso PatPath.equal(acc1,acc2)
  | equal _ = false

structure Hash = 
  HashTableFn(
  struct type hash_key = dec val sameKey = equal val hashVal = hash end);

(* Hash-consing, to get a decision dag rather than a decision tree *)
fun unique table (node as IfEq(_, _, t1, t2)) = 
    if t1 = t2 then t1
    else 
      (case Hash.find table node of
        NONE =>
        let val rnode = mkDecision node
        in 
	  incrnode t1; incrnode t2; 
	  Hash.insert table (node, rnode);
	  rnode
        end

      | SOME rnode => rnode)
  | unique _ _ = Debug.fail "PatDec.unique"

fun tally (ref {tree, ... } : decision) = tally' tree
and tally' Failure = (IntSet.empty, true)
  | tally' (Success(i,_)) = (IntSet.singleton i, false)
  | tally' (IfEq(_,_,d1,d2)) = 
    let 
      val (s1, f1) = tally d1
      val (s2, f2) = tally d2
    in
      (IntSet.union(s1, s2), f1 orelse f2)
    end

end