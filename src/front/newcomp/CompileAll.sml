(*======================================================================*)
(* Compile everything that came back from closure conversion.		*)
(*======================================================================*)
structure CompileAll :> COMPILEALL =
struct

local open Env
in

val debug = RTInstrs.debug
val stats = Controls.add false "codegen.stats"

fun compile (projname,classnames)
  { fundefs, globvars, classdefs, closdefs, clinit, appmeths,
    methtys, bindeffects } =
let
  fun makeEnv () = 
  let
    val tyenv = Var.Map.foldl (fn ((x,ty), m) => 
      let val str = RTSymbols.defineSym x
          val rep = TyRep.tyToRep CompileEnv.empty ty
      in
        Var.Map.insert(m,#1 x, (RTInstrs.Single (RTInstrs.ldsfld { name = Id.fromString str, classty = TyRep.globs (), fldty = rep }), ty))
      end) Var.Map.empty globvars
    val _ = Var.Map.app (fn (_,f,_,_) => ignore(RTSymbols.defineSym f)) fundefs
  in
    CompileEnv.initialEnv { tyenv = tyenv, known = Var.Map.map (fn (tyvars, _, (xs,_), cty) => MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))) fundefs, apps = appmeths }  
  end
in
(*  TargetManager.setDefaultName projname; *)
  (Stats.clear();
  CompileOnePlus.init ();
  RTSymbols.init();
  Liveness.initEffectInfo bindeffects;
  RTAssem.emit (projname, fn save =>
  (  
    TyRep.start (classnames,save);
    let val env = makeEnv() in
    PrintManager.process("Compiling closure defs",false)
    (fn () => ListOps.appi (save o CompileClosure.compile env) closdefs);
    PrintManager.process("Compiling class defs",false)
    (fn () => app save (CompileClassType.compile env classdefs)); 
    (*@review akenn: perhaps we could use the same class for globals and as the top fun class? *)
    PrintManager.process("Compiling top fun class",false)
    (fn () => save (CompileClosure.makeTopFun methtys));
    PrintManager.process("Compiling globals",false)
    (fn () => 
     app save (CompileGlobals.compile env (globvars,fundefs,clinit)));
    TyRep.finish true
    end
  )
  ) handle e => (TyRep.finish false; raise e))
  before (if Controls.get stats then Stats.dump() else ())
end

end (* of local open *)

end (* of struct *)



