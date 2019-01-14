(*======================================================================*)
(* Determine which top-level value bindings should be put into globals  *)
(* Outline of algorithm:						*)
(* * Traverse term, keeping track of whether we're in a top-level 	*)
(*   context or not (top-level = not inside a non-local function)       *)
(*									*)
(* * Variables bound at top-level are initially marked as non-global    *)
(*   (Bound = let-bound, letfun-bound or bound as arguments to 		*)
(*    top-level local functions)				 	*)
(*  									*)
(* * When such a variable occurs in a non top-level context, mark it    *)
(*   it as global							*)
(*									*)
(* Also determine which top-level-allocated refs can be stored in 	*)
(* globals: just those that only get used in immediate assignment and   *)
(* dereferencing.							*)
(*======================================================================*)
structure GlobalInfo :> GLOBALINFO =
struct

local open MILTerm MILTermOps
in

val doGlobals = Controls.add true "clos.globals"
val doGlobalRefs = Controls.add false "clos.globalrefs"

(* Given a term e, gather global variable info and return as a set *)
fun gather e = 
let

val varIsGlobal = ref (Var.Map.empty : bool Var.Map.map)
val varIsGlobalRef = ref (Var.Map.empty : bool Var.Map.map)

(* Add top-level bindings to the state *)
fun addBindings xs = 
app (fn (x,_) => varIsGlobal := Var.Map.insert(!varIsGlobal, x, false)) xs

fun addRefBinding (x:BoundVar) = 
  varIsGlobalRef := Var.Map.insert(!varIsGlobalRef, #1 x, true)

(* If a variable has been bound at top-level and isn't yet marked as global,
   then mark it *)
fun addOcc x =
  case Var.Map.find(!varIsGlobal, x) of
    SOME false => varIsGlobal := Var.Map.insert(!varIsGlobal, x, true)
  | _ => ()

(* If a variable has been bound to a new ref cell at top-level and isn't yet marked 
   as first-class, then mark it *)
fun addFirstClassOcc x =
  case Var.Map.find(!varIsGlobalRef, x) of
    SOME true => varIsGlobalRef := Var.Map.insert(!varIsGlobalRef, x, false)
  | _ => ()


(* Traverse a value term used in a dereferencing or assignment *)
fun tvRef v =
case v of
  Var x => addOcc x
| SCon _ => () (*NB: a mutable, static field is represented by the integer 0 *)
(* SL: or *)
(*
| (Unfold v | TApp(v,_) | As(v,_)) => tvRef v
*)
| Unfold v => tvRef v
| TApp(v,_) => tvRef v
| As(v,_) => tvRef v

| _ => MILPretty.failVal v "illegal term in ! or :="

(* Traverse an arbitrary value term *)
and tv v =
case v of
  SCon _ => ()
| Var x => (addOcc x; addFirstClassOcc x)
| Fold(v, _) => tv v
| Unfold v => tv v
| Proj(_,_,v) => tv v
| TAbs(_,v) => tv v
| TApp(v,_) => tv v
| As(v,_) => tv v
| Inj(_,_,vs,_) => tvs vs
| ExCon(_,vs) => tvs vs
| Tuple vs => tvs vs

and tvs vs = app tv vs

(* Traverse a top-level computation term *)
fun tcTop e =
let
  fun tcTopCases (_,cases,default,_) =
  (app (fn (_,(xs,e)) => (addBindings xs; tcTop e)) cases; 
  case default of
    NONE => ()
  | SOME e => tcTop e)
in
  case e of
(*
  Let(Alloc [_],([(x,_)],e)) =>
  (addBindings [x]; addRefBinding x; tcTop e)

|
 *)
  Let(_,(xs,e)) =>
  (addBindings (map #1 xs); tcTop e)

| TryLet(_,handlers,(xs,e)) =>
  (app (fn (xs,e) => (addBindings(map #1 xs); tcTop e)) handlers; addBindings (map #1 xs); tcTop e)

| LetVal(x, _, e) =>
  (addBindings [x]; tcTop e)

| Case a => tcTopCases a
| CaseSCon a => tcTopCases a
| TypeCase a => tcTopCases a

| LetClass(_,_,_,methods,e) =>
  (tcTop e; 
    app (fn (_,_,_,_,_,body) =>
    case body of NONE => () | SOME (f,(xs,body)) => tcNonTop body) methods)
  
| LetFun(_,kind,Fun(f,(xs,body)),e) =>

  let val kind = FunFlow.normalizeKind kind in
  (case kind of
    LocalFun =>
    (addBindings (map #1 xs); tcTop body; tcTop e)

  | KnownFun =>
    (tcTop e; tcNonTop body)

  | AnyFun =>
    (addBindings [f]; tcTop e; tcNonTop body)
  )
  end

| LetFun(_,kind,RecFun defs,e) =>
  let val kind = FunFlow.normalizeKind kind in
  (case kind of
    LocalFun =>
    (app (fn (_,_,(xs,body),_) => (addBindings (map #1 xs); tcTop body)) defs; tcTop e)

  | KnownFun =>
    (tcTop e; app (fn (_,_,(xs,body),_) => tcNonTop body) defs)

  | AnyFun =>
    (app (fn (f,g,_,_) => addBindings [f]) defs; 
     tcTop e; 
     app (fn (f,g,(xs,body),_) => tcNonTop body) defs)
  )
  end

| Encap e =>
  tcNonTop e

| _ => ()
end

and tcNonTop e = 
let
  fun tcNonTopCases (v,cases,default,cty) =
  (tv v;
   app (fn (_,(_,e)) => tcNonTop e) cases; 
   case default of
     NONE => ()
   | SOME e => tcNonTop e)

in
case e of
  App(v,vs) => (tv v; tvs vs)
| Special(_,vs,_) => tvs vs
| Triv vs => tvs vs
(*
| Deref(v,_) => tvRef v
*)
| Throw(v,_,_) => tv v
(*
| Assign(v,_,v') => (tvRef v; tv v')
*)
| Let(body,(_,e)) => (tcNonTop body; tcNonTop e)
| TryLet(body,handlers,(_,e)) => (tcNonTop body; app (fn (_,e) => tcNonTop e) handlers; tcNonTop e)
| LetVal(_,v,e) => (tv v; tcNonTop e)
| Case a => tcNonTopCases a
| CaseSCon a => tcNonTopCases a
| TypeCase a => tcNonTopCases a
| LetClass(_,_,_,methods,e) =>
  (app (fn (_,_,_,_,_,body) =>
    case body of NONE => () | SOME (f,(xs,body)) => tcNonTop body) methods;
  tcNonTop e)
| LetFun(_,_,Fun(_,(_,body)),e) => (tcNonTop body; tcNonTop e)
| LetFun(_,_,RecFun defs,e) =>
  (app (fn (_,_,(_,body),_) => tcNonTop body) defs; tcNonTop e)
| Encap e => tcNonTop e

end

in
  if Controls.get doGlobals then tcTop e else ();
  {
    globalvals = Var.Map.foldri (fn (x,b,s) => if b then Var.Set.add(s,x) else s) Var.Set.empty (!varIsGlobal),
    globalrefs = if Controls.get doGlobalRefs then Var.Map.foldri (fn (x,b,s) => if b then Var.Set.add(s,x) else s) Var.Set.empty (!varIsGlobalRef)
                 else Var.Set.empty
  }
end

end (* of local open *)
end (* of struct *)
