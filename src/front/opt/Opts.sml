(*======================================================================*)
(* Table of optimisations applied to MIL terms.				*)
(*======================================================================*)
structure Opts :> OPTS =
struct

(*----------------------------------------------------------------------*)
(* An entry in the optimisation table					*)
(*----------------------------------------------------------------------*)
type Entry =
{
  description : string,		(* Description that's displayed *)
  transform : Opt.Transformer,	(* The function itself *)
  census : Controls.Flag,
  typecheck : Controls.Flag,
  dump : Controls.Flag,
  diff : Controls.Flag,
  enabled : Controls.Flag,
  precheck : MILTy.Type Var.Map.map -> MILTerm.Cmp -> unit,
  postcheck : MILTy.Type Var.Map.map -> MILTerm.Cmp -> unit
}

fun tc tyenv e =
  (TypeCheck.checkAll
    { tyenv = tyenv, kindenv = Var.Map.empty, 
      funenv = Var.Map.empty } e; ())

fun tcPoly tyenv e = 
  (TypeCheck.check {closedBlocks=false,checkAtoms=true,checkDistinctTyVars=true,checkDistinctVars=true,checkCC=false,checkPoly=true} 
                  { tyenv = tyenv, kindenv = Var.Map.empty, funenv = Var.Map.empty } e; ())

fun tcAtom tyenv e = 
  (TypeCheck.check {closedBlocks=false,checkAtoms=true,checkDistinctTyVars=true,checkDistinctVars=true,checkCC=false,checkPoly=false} 
                  { tyenv = tyenv, kindenv = Var.Map.empty, funenv = Var.Map.empty } e; ())

val opts = ref (StringMap.empty : Entry StringMap.map)

fun add name (f,desc) =
  if isSome (StringMap.find(!opts, name))
  then Debug.fail ("Opts.add: transform " ^ name ^ " already present")
  else  
  let
    val d = Controls.add false (name ^ ".dump")
    val df = Controls.add false (name ^ ".diff")
    val c = Controls.add false (name ^ ".census")
    val t = Controls.add false (name ^ ".check")
    val e = Controls.add true name
  in
    opts := StringMap.insert(!opts, name, 
    { description = desc ^ " [" ^ name ^ "]", transform = f, 
      dump = d, diff = df, census = c, typecheck = t, enabled = e, precheck = tc, postcheck = tc })
  end

fun add' name {description,transform,precheck,postcheck} =
  if isSome (StringMap.find(!opts, name))
  then Debug.fail ("Opts.add: transform " ^ name ^ " already present")
  else  
  let
    val d = Controls.add false (name ^ ".dump")
    val df = Controls.add false (name ^ ".diff")
    val c = Controls.add false (name ^ ".census")
    val t = Controls.add false (name ^ ".check")
    val e = Controls.add true name
  in
    opts := StringMap.insert(!opts, name, 
    { description = description ^ " [" ^ name ^ "]", transform = transform, 
      dump = d, diff = df, census = c, typecheck = t, enabled = e, precheck = precheck, postcheck = postcheck })
  end

val memory = ref ([] : MILTerm.Cmp list)

fun isBenign opt = List.exists (fn opt' => opt=opt') 
  ["dump", "diff", "dumpBasis", "typecheck", "census"]

fun applyOpt opt tyenv e =
case StringMap.find(!opts, opt) of
  NONE =>
  (PrintManager.print ("\nNo such transformation: " ^ opt); e)

| SOME { description, transform, census, typecheck, dump, diff, enabled, precheck, postcheck } =>
  if not (Controls.get enabled) then e
  else
  let
    val restoreAnonVarName = MILTermOps.setAnonVarName ([Id.fromString ("!"^description)])
    val _ =
    (
      if Controls.get dump then MILPretty.dumpCmp e else ();
      if Controls.get diff then MILPretty.dumpCmpTo (opt ^ ".before.html") e else ();
      if Controls.get typecheck then ignore (precheck tyenv e) else ();
      PrintManager.dump census (Census.checkCmps ([], [e]))
    )
    val e = PrintManager.process (description, false) 
      (fn () => transform tyenv e)
    val _ = 
    (
      if Controls.get dump then MILPretty.dumpCmp e else ();
      if Controls.get diff then MILPretty.dumpCmpTo (opt ^ ".after.html") e else ();
      if Controls.get typecheck then ignore (postcheck tyenv e) else ();
      PrintManager.dump census (Census.checkCmps ([], [e]))
    )
    val _ = restoreAnonVarName()
  in
    e
  end

fun apply opts tyenv (e,supply) =
let
  val _ = Census.initCensus (e,supply)
  fun ap ("on"::flag::opts) e =
    (case Controls.lookup flag of
      [(_,b)] => (Controls.set (b,true); Controls.reset (); ap opts e)
    | _ => ap opts e)

    | ap ("off"::flag::opts) e =
    (case Controls.lookup flag of
      [(_,b)] => (Controls.set (b,false); Controls.reset (); ap opts e)
    | _ => ap opts e)

    | ap ("memo"::opts) e =
    (memory := e :: !memory; ap opts e)

    | ap (opt::opts) e =
      ap opts (applyOpt opt tyenv e)

    | ap [] e = 
      e

  val e = ap opts e
in
  (e,Census.maxVar ())
end

val _ = List.app (fn name => add name
  (Simplify.simplify { removeEncaps = false, doComplex = true }, "Simplifying")
  ) ["simp1", "simp2", "simp3", "simp4", "simp5", "simp6", "simp7", "simp8", "simp9"]


val _ = add' "presimp"  
  { description = "Pre-simplifying", 
    transform = Simplify.simplify { removeEncaps = false, doComplex = false }, 
    precheck = tc,
    postcheck = tcAtom
  }

val _ = add' "linkpresimp"  
  { description = "Pre-simplifying", 
    transform = Simplify.simplify { removeEncaps = false, doComplex = false }, 
    precheck = tc,
    postcheck = tcPoly
  }

val _ = add "lastsimp"  
  (Simplify.simplify { removeEncaps = true, doComplex = true }, "Simplifying")

end (* of struct *)

