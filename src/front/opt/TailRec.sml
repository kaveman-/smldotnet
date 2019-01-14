(*======================================================================*)
(* For functions that aren't already completely local, create an inner	*)
(* block in order to implement tail calls.                              *)
(*======================================================================*)
structure TailRec :> TRANSFORMER =
struct


local 
  open MILTerm 
in

val tailRec = Controls.add true "tailRec"

fun transform tyenv e =
let

(*----------------------------------------------------------------------*)
(* Tail variable info.							*)
(*----------------------------------------------------------------------*)
val tails = ref (Var.Map.empty : Var.Var Var.Map.map)

(*----------------------------------------------------------------------*)
(* Generate a new tail variable if there isn't already one there.       *)
(*----------------------------------------------------------------------*)
fun addTail g =
  case Var.Map.find(!tails, g) of

    (* If it's already been used in a tail-recursive app then just return *)
    (* its inner name. *)
    SOME g' =>
    (Census.addVar(g,~1); Census.addVar(g', 1); Var g')

    (* Otherwise generate a fresh name, record in the state and return it *)
  | NONE =>
    let
      val g' = Census.freshVar 1
    in
      Census.addVar(g,~1);
      tails := Var.Map.insert(!tails, g, g'); 
      Var g'
    end

(*----------------------------------------------------------------------*)
(* Do the translation on computation terms.				*)
(*----------------------------------------------------------------------*)
fun transCmp tailvar e =
let
  val tt = transCmp tailvar
  fun transCases (v, cases, e, cty) = 
    (v, map (fn (i,(xs,e)) => (i,(xs, tt e))) cases, Option.map tt e, cty)
in
  case e of
  App(Var x, vs) =>
  (case tailvar of
    NONE => e
  | SOME x' => 
    if Var.eq(x, x') andalso Controls.enabled tailRec
    then App(addTail x, vs)
    else e)

| Encap e => 
  Encap (tt e)

| Let(e1, (typedvars, e2)) =>
  Let(transCmp NONE e1, (typedvars, tt e2))

| LetVal(x, v, e) =>
  LetVal(x, v, tt e)

| Case a =>
  Case (transCases a)

| CaseSCon a =>
  CaseSCon (transCases a)
      
| TypeCase a =>
  TypeCase (transCases a)

| TryLet(e0, tabss, (vs, e2)) =>
  TryLet(transCmp NONE e0, map (fn (vs,e) => (vs,transCmp NONE e)) tabss,
    (vs, tt e2))

| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun transMethod (method as (name, atts, mods, tys, tyopt, optabs)) =
        case optabs of 
          NONE => method

        | SOME (f, (vs, e)) =>
          (name, atts, mods, tys, tyopt, SOME (f, (vs, transCmp NONE e)))
  in
    LetClass(classname, classinfo, fields, map transMethod methods, tt e)
  end
  
| LetFun(tyvars, kind, def, e) =>
  let
    fun transDef (Fun (f, (typedvars, e))) =
        Fun (f, (typedvars, 
          if MILTermOps.isLocal kind then tt e else transCmp NONE e))

      | transDef (RecFun recbinds) =
        RecFun (map (fn (f, g, (typedvars, e), cty) =>
        let
          (* Don't do tail recursion transformation if it's a local block
             or the appropriate flags aren't set *)
          val e = if MILTermOps.isLocal kind 
            	  then tt e 
            	  else transCmp (SOME (#1 g)) e

          val tabs = 
            case Var.Map.find(!tails, #1 g) of
              SOME g' =>                 
              let
                val (f',fv') = Census.freshBoundVar 1 f
                val (xs,xvs) = Census.freshTypedVars 1 typedvars
              in
                (xs, LetFun([], LocalFun, RecFun [(f',(g',[]),(typedvars,e),cty)], App(fv', xvs)))
              end
            | NONE => (typedvars, e)

      in
        (f, g, tabs, cty)
      end) recbinds)
  in
    LetFun(tyvars, kind, transDef def, tt e)
  end

| _ => e

end

val _ = Controls.reset ()
val e = transCmp NONE e
in
  Controls.printCounts PrintManager.print; e
end

end (* of local open *)

val _ = Opts.add "tailrec" (transform, "Introducing tail recursion")
val _ = Opts.add "tailrec2" (transform, "Introducing tail recursion")

end (* of struct *)

