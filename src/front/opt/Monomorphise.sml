(*======================================================================*)
(* Monomorphise a MIL term (@REQUIRED).        				*)
(* Two passes:								*)
(*   (1) Gather instances, copy polymorphic defns, rename polymorphic   *)
(*       uses.								*)
(*   (2) Specialise defns used at _exactly_ one type.			*)
(* Finally, rename all bound variables.					*)
(*======================================================================*)
structure Monomorphise :> TRANSFORMER =
struct

local 
  open MILTerm 
in

val fullySpecialise = Controls.add false "mono.full"
val showInsts = Controls.add false "mono.showInsts"
val showStats = Controls.add false "mono.stats"

datatype Spec = Singleton of MILTy.Type
              | Representative of MILTy.Type

(*----------------------------------------------------------------------*)
(* A specialisation is a map from variables to information about the    *)
(* types at which the variable is used.                                 *)
(*----------------------------------------------------------------------*)
type ValSpecInfo =
{
  (* Set if var appears uninstantiated *)
  uninstantiated : bool,      
 
  (* The original bound variables in the letrec *)
  funs : MILTerm.BoundVar list,

  (* Compatible Instantiations: an element ([tyset_1,...,tyset_m],[f_1,...,f_n]) 
     of this list represents the replacement of some term f_j [ty_1,...,ty_m] 
     (where ty_i is in tyset_i for each i) by f'_j [ty_1,...,ty_m]
     When tyset_i is a singleton the type parameter is removed.
  *)
  instances : (Spec list * MILTerm.BoundVar list) list
}
 
(*----------------------------------------------------------------------*)
(* Environment passed around monomorphisation functions			*)
(*----------------------------------------------------------------------*)
type Env =
{
  kindenv : MILTy.Kind Var.Map.map,	(* Map from tyvars to kinds: representation classes of types *)
  tysubst : MILTy.Type Var.Map.map	(* Type substitution, used for singletons *)
}

(*----------------------------------------------------------------------*)
(* Monomorphise the (necessarily closed) computation term e.        	*)
(* The type environment tyenv is ignored -- it should be empty.         *)
(*----------------------------------------------------------------------*)
fun transform tyenv e = 
let

val share = not (Controls.get fullySpecialise)

(* Instantiations, indexed on the first function variable of a letrec *)
val spec = ref (Var.Map.empty : ValSpecInfo Var.Map.map)

(* Map from letrec-bound function variables to the first in the group *)
(* and its position in the group *)
val funvars = ref (Var.Map.empty : (Var.Var*int) Var.Map.map)

val tyargmap = ref (Var.Map.empty : bool list Var.Map.map)

(*----------------------------------------------------------------------*)
(* Given a value term x {tys} replace by its specialised variable x' {},*)
(* adding to !spec if necessary. Just leave an empty list of types to   *)
(* be filled in later when we know about singleton instantiations.      *)
(*----------------------------------------------------------------------*)
fun monoTApp (env : Env) (x,argtys) =
  case Var.Map.find(!funvars, x) of
    NONE => 
    Debug.fail ("Monomorphise.monoTApp: cannot find variable " ^ Var.toString x)

  | SOME (firstf, i) =>
    case Var.Map.find(!spec, firstf) of
      NONE =>
      Debug.fail ("Monomorphise.monoTApp: cannot find variable " ^ Var.toString firstf)
 
    | SOME { uninstantiated, instances, funs } =>
      let
       fun find ([],_) = 

	  (* We haven't seen a compatible instantiation already so create
             a freshly-specialised set of functions *)
          let val (fvars,fterms) = Census.freshBoundVars 1 funs
          in
            spec := Var.Map.insert(!spec, firstf, 
              { uninstantiated = uninstantiated, 
                funs = funs,
                instances = 
                  (map (fn argty => 
                    if Var.Set.exists (fn tyvar => Var.Map.inDomain(#kindenv env, tyvar)) (MILTy.tyvars argty)
                    then Representative (MILTy.forceBounds (#kindenv env) argty) else Singleton (MILTy.forceBounds (#kindenv env) argty)) argtys, fvars) :: instances });
            TApp(List.nth(fterms, i), argtys)
          end

        | find ((inst as (specs, xs'))::instances, prefix) =
          let
            fun find' ([],[],result) = 
                (spec := Var.Map.insert(!spec, firstf, 
                  { uninstantiated = uninstantiated, funs = funs,
                    instances = (rev result, xs')::instances @ prefix });
                 TApp(Var (#1 (List.nth(xs',i))), argtys))

              | find' (Singleton ty::specs, argty::argtys, result) =
                if MILTy.eq(ty, argty)
                then find' (specs, argtys, Singleton ty::result)
                else if share andalso MILTyRep.sameRep (#kindenv env) (argty,ty)
                then find' (specs, argtys, Representative ty::result)
                else find (instances, inst::prefix)

              | find' (Representative ty::specs, argty::argtys, result) =
                if share andalso MILTyRep.sameRep (#kindenv env) (argty,ty)
                then find' (specs, argtys, Representative ty::result)
                else find (instances, inst::prefix)
          in
            find' (specs,argtys,[])
          end

      val v = find (instances, [])
    in
      v
    end

(*----------------------------------------------------------------------*)
(* Remove singleton instantiations and add correct type parameter list  *)
(* to tyargmap								*)
(*----------------------------------------------------------------------*)
fun gatherArgs (f,_) =
  case Var.Map.find(!funvars, f) of
    NONE => 
    Debug.fail 
    ("Monomorphise.gatherArgs: cannot find variable " ^ Var.toString f)
  | SOME (firstf, i) =>

    case Var.Map.find(!spec, firstf) of
      NONE => 
      Debug.fail 
      ("Monomorphise.gatherArgs: cannot find variable " ^ Var.toString firstf)
 
    | SOME { uninstantiated, instances, funs } =>
      let
        fun gather (specs, fs) =
        let
         val bools = map (fn Singleton _ => false | _ => true) specs
        in
          app (fn (f,_) => tyargmap := Var.Map.insert(!tyargmap, f, bools)) fs
        end
      in
        app gather instances
      end 

(*----------------------------------------------------------------------*)
(* Add polymorphic variables xs to !spec, initially with no instances.	*)
(*----------------------------------------------------------------------*)
fun initInstances (xs as ((first,_)::_) : MILTerm.BoundVar list) =
(
  funvars := ListOps.foldri (fn (i,(x,_),map) => Var.Map.insert(map, x, (first,i)))
    (!funvars) xs;
  spec := Var.Map.insert(!spec, first,
    { uninstantiated = false, instances = [], funs = xs })
)

(*----------------------------------------------------------------------*)
(* Add an uninstantiated polymorphic variable x to !spec.		*)
(*----------------------------------------------------------------------*)
fun addUninst x =
  case Var.Map.find(!funvars, x) of
    NONE => ()
  | SOME (first, i) =>
    case Var.Map.find(!spec, first) of
      SOME { uninstantiated = false, instances, funs } => 
      spec := Var.Map.insert(!spec, first, 
        { uninstantiated = true, instances = instances, funs = funs })

    | _ => ()

(*----------------------------------------------------------------------*)
(* Get the type instances at which a variable x is used.		*)
(*----------------------------------------------------------------------*)
fun getInstances (x : MILTerm.BoundVar) =
  case Var.Map.find(!funvars, #1 x) of
    NONE =>
    Debug.fail ("Monomorphise.getInstances: cannot find variable " ^ 
      Var.toString (#1 x))
  | SOME (firstf, i) =>
    case Var.Map.find(!spec, firstf) of
      NONE => 
      Debug.fail ("Monomorphise.getInstances: cannot find variable " ^ 
        Var.toString firstf)
    | SOME p => p

(*----------------------------------------------------------------------*)
(* Extend kind environment with bound kinds or an exact substitution	*)
(*----------------------------------------------------------------------*)
fun envPlusInsts env (tyvars, specs) =
let val mt = MILTy.subst (#tysubst env) in
ListPair.foldr 
  (fn ((x,_), spec, ({ kindenv, tysubst }, tyvars)) =>
     case spec of
       Singleton ty => ({ kindenv = kindenv, tysubst = Var.Map.insert(tysubst, x, mt ty) }, tyvars)
     | Representative ty => ({ kindenv = Var.Map.insert(kindenv, x, MILTy.Bound (mt ty)), tysubst = tysubst }, (x,MILTy.Bound (mt ty))::tyvars)
  )
  (env,[]) (tyvars, specs)
end

val emptyEnv = 
{ kindenv = Var.Map.empty, tysubst = Var.Map.empty  }

(*----------------------------------------------------------------------*)
(* Monomorphise a value term and rename variables where appropriate	*)
(*----------------------------------------------------------------------*)
fun monoVal (env : Env) v =
let 
  val mc = monoCmp env
  val mv = monoVal env
  val mt = MILTy.subst (#tysubst env)
in
case v of
  Var x => 
  (addUninst x; v)

| SCon _ => 
  v

| Inj(ty, i, vs, si) => 
  Inj(mt ty, i, map mv vs, si)

| As(v, ty) =>
  As(mv v, mt ty)

| ExCon(ty, vs) => 
  ExCon(mt ty, map mv vs)

| Tuple vs => 
  Tuple (map mv vs)

| Proj(i, n, v) => 
  Proj(i, n, mv v)

| TApp(Var x, tys) =>
  monoTApp env (x, map mt tys)

| Fold(v, ty) => 
  Fold(mv v, mt ty)

| Unfold v => 
  Unfold (mv v)

| _ => MILPretty.failVal v "Monomorphise.monoVal: illegal value term"

end

(*----------------------------------------------------------------------*)
(* Monomorphise a computation term and rename variables 		*)
(*----------------------------------------------------------------------*)
and monoCmp (env : Env) e = 
let 
  val mc = monoCmp env
  val mv = monoVal env
  val mt = MILTy.subst (#tysubst env)
  fun mct cty = 
    let val (eff,tys) = MILTy.fromCmp cty in MILTy.cmp(eff,map mt tys) end

  fun monoAbstr (xs,e) = (xs, monoCmp env e)
  fun monoTAbstr (xs:MILTerm.TypedVar list,e) = 
    (map (fn (x,ty) => (x,mt ty)) xs, monoCmp env e)

  fun monoCase fi (i, abs) = (fi i, monoAbstr abs)    
  fun monoCases fi (v, cases, def, cty) = (mv v, map (monoCase fi) cases, Option.map mc def, mct cty)
in
case e of
  App(v, vs) => 
  App(mv v, map mv vs)

| Special(jop, vs, cty) =>
  Special(jop, map mv vs, mct cty)

| Let(e, abs) =>
  Let(mc e, monoTAbstr abs)

| Triv vs => 
  Triv (map mv vs)

| Encap e =>
  Encap (mc e)

| Case cases => 
  Case (monoCases (fn i => i) cases)

| CaseSCon cases => 
  CaseSCon (monoCases (fn c => c) cases)

| TypeCase cases => 
  TypeCase (monoCases mt cases)

| Throw(v, cty, loc) =>
  Throw(mv v, mct cty, loc)

| TryLet(e, handlers, body) =>
  TryLet(mc e, map monoTAbstr handlers, monoTAbstr body)

(*......................................................................*)
(* The types of fields and methods should be monomorphic already.	*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let 
    val methods = map (fn (s, atts, m, tys, tyopt, absopt) => 
    (s, atts, m, tys, tyopt, 
      case absopt of
        NONE => NONE

      | SOME (f,abs) => SOME (f, monoAbstr abs))) methods

  in
    LetClass(classname, classinfo, fields, methods, mc e)
  end

(*......................................................................*)
(* Specialise a letrec.							*)
(* First recurse on the body, gathering uses of the function vars.      *)
(* If tyvars is empty, do the obvious thing on the definitions.         *)
(* Otherwise, determine what distinct instances _any_ of the functions  *)
(* is used at in the body, and put these in a list.                     *)
(*......................................................................*)
| LetFun(tyvars, kind, def, e) => 
  let
    val fs = 
      case def of RecFun defs => map #1 defs | Fun(f,_) => [f]

    val _ = initInstances fs
    val e = mc e
    val _ = gatherArgs (hd fs)
  in
  case def of
    RecFun defs =>
    if null tyvars 
    then 
      let 
        val defs = 
          map (fn (f, g, tabs, cty) => (f, g, monoTAbstr tabs, mct cty)) defs
      in
        LetFun(tyvars, kind, RecFun defs, e)
      end
    else 
    let
      fun specialise (typarsets, fs') =
      let
        val (env',tyvars') = envPlusInsts env (tyvars, typarsets)
        val mt = MILTy.subst (#tysubst env')

        fun applyToDef ((f, g, (xs,e), cty), f') =
          (f', g, (map (fn (x,ty) => (x,mt ty)) xs, monoCmp env' e), 
          let val (eff,tys) = MILTy.fromCmp cty in MILTy.cmp(eff,map mt tys) end)

      in
        (tyvars', ListPair.map applyToDef (defs,fs'))
      end
      val { uninstantiated, instances, funs } = getInstances (#1 (hd (defs)))
      val pairs = map specialise instances
    in
      if uninstantiated
      then 
        let 
          val defs = 
            map (fn (f, g, tabs, cty) => (f, g, monoTAbstr tabs, cty)) defs
        in
          (foldr (fn ((tyvars,defs),e) => LetFun(tyvars, kind, RecFun defs, e))
          (LetFun(tyvars, kind, RecFun defs, e)) pairs)
        end
      else
        (foldr (fn ((tyvars,defs),e) => LetFun(tyvars, kind, RecFun defs, e)) 
          e pairs)
    end

  | Fun (f, tabs as (xs,e')) =>
    if null tyvars 
    then 
      LetFun(tyvars, kind, Fun (f, monoTAbstr tabs), e)
    else
    let
      fun specialise (typarsets, [f']) =
      let
        val (env',tyvars') = envPlusInsts env (tyvars, typarsets)
        val mt = MILTy.subst (#tysubst env')
      in
        (tyvars', (f', (map (fn (x,ty) => (x,mt ty)) xs, monoCmp env' e')))
      end
      val { uninstantiated, instances, funs } = getInstances f
      val defs = map specialise instances
    in
      if uninstantiated
      then 
        foldr (fn ((tyvars, def), e) => LetFun(tyvars, kind, Fun def, e))
          (LetFun(tyvars, kind, Fun (f,tabs), e)) defs
      else
        foldr (fn ((tyvars, def), e) => LetFun(tyvars, kind, Fun def, e)) 
          e defs
    end
  end

(*......................................................................*)
(* Specialise let x = Fn tyvars.v in e.					*)
(* First recurse on the body, gathering uses of x.                      *)
(* If tyvars is empty, do the obvious thing on the definition.          *)
(* Otherwise, determine at what distinct instances x is used in the     *)
(* body, and monomorphise the defn.                                     *)
(*......................................................................*)
| LetVal(x, TAbs(tyvars, v), body) =>
  let
    val _ = initInstances [x]
    val body = mc body
    val _ = gatherArgs x
    val { uninstantiated, instances, funs } = getInstances x
    fun specialise (typarsets, [x]) =
      let
        val (env',tyvars') = envPlusInsts env (tyvars, typarsets)
      in
        (x, MILTermOps.tabs(tyvars', monoVal env' v))
      end
    val r = map specialise instances
  in    
    (foldl (fn ((x,v),body) => LetVal(x, v, body))
      (if uninstantiated then LetVal(x, TAbs(tyvars, v), body) else body) r)
  end


| LetVal(x, v, e) =>
  LetVal(x, mv v, mc e)
    
end 


(* Clear the census in preparation for renaming *)
val _ = Census.clearCensus ()

(* Determine the size of the original term *)
val oldsize = if Controls.get showStats then MILTermOps.absSize e else 0

(* Do the monomorphisation *)
val e = PrintManager.process
  ("Monomorphising wrt target representations", false)
  (fn () => monoCmp emptyEnv e)

(*----------------------------------------------------------------------*)
(* Clean up a value term by removing some type applications.		*)
(*----------------------------------------------------------------------*)
fun mv v =
case v of
  Var _ => v
| SCon _ => v
| Inj(ty, i, vs, si) => Inj(ty, i, map mv vs, si)
| As(v, ty) => As(mv v, ty)
| ExCon(excon, vs) => ExCon(excon, map mv vs)
| Tuple vs => Tuple (map mv vs)
| Proj(i, n, v) => Proj(i, n, mv v)
| Fold(v, ty) => Fold(mv v, ty)
| Unfold v => Unfold (mv v)
| TApp(Var x, tys) =>
  (case Var.Map.find(!tyargmap, x) of    
    NONE => MILPretty.failVal v "Monomorphise.mv: type application not in map"
  | SOME bools => 
    let
      fun gather ([],[]) = []
        | gather (ty::tys,true::bools) = ty::gather(tys,bools)
        | gather (ty::tys,false::bools) = gather(tys,bools)
      val tys = gather (tys,bools)
    in
      if List.null tys then Var x
      else TApp(Var x, tys)
    end
  )

| _ => MILPretty.failVal v "Monomorphise.mv: illegal value term" 

(*----------------------------------------------------------------------*)
(* Clean up a computation term 						*)
(*----------------------------------------------------------------------*)
and mc e = 
let 
  fun mabs (xs,e) = (xs, mc e)
  fun mcase (i, a) = (i, mabs a)
  fun mcases (v, cases, def, cty) = (mv v, map mcase cases, Option.map mc def, cty)
in
case e of
  App(v, vs) => App(mv v, map mv vs)
| Special(jop, vs, cty) => Special(jop, map mv vs, cty)
| Let(e, a) => Let(mc e, mabs a)
| Triv vs => Triv (map mv vs)
| Encap e => Encap (mc e)
| Case cases => Case (mcases cases)
| CaseSCon cases => CaseSCon (mcases cases)
| TypeCase cases => TypeCase (mcases cases)
| Throw(v, cty, loc) => Throw(mv v, cty, loc)
| TryLet(e, handlers, cont) => TryLet(mc e, map mabs handlers, mabs cont)
| LetClass(classname, classinfo, fields, methods, e) =>
  let 
    val methods = map (fn (s, atts, m, tys, tyopt, absopt) => 
    (s, atts, m, tys, tyopt, 
      case absopt of
        NONE => NONE
      | SOME (f,a) => SOME (f, mabs a))) methods
  in
    LetClass(classname, classinfo, fields, methods, mc e)
  end

| LetFun(tyvars, kind, def, e) => 
  LetFun(tyvars, kind, 
    (case def of
      RecFun defs => 
      RecFun (map (fn (f, g, a, cty) => (f, g, mabs a, cty)) defs)

    | Fun (f, a) =>
      Fun (f, mabs a)), mc e)

(*@HACK*)
| LetVal(x, TAbs(tyvars, v), body) => LetVal(x, TAbs(tyvars,mv v), mc body)
| LetVal(x, v, e) => LetVal(x, mv v, mc e)
    
end 


(* Remove some type apps *)
val e = PrintManager.process
  ("Removing singleton instantiations", false) (fn () => mc e)

(* Rename bound variables in the term *)
val e = PrintManager.process
  ("Renaming term", false) (fn () => Census.renameCmp e)

(* Calculate the new size *)
val newsize = if Controls.get showStats then MILTermOps.absSize e else 0

fun dumpSpec (Singleton ty) = MILTy.toString ty
  | dumpSpec (Representative ty) = "rep " ^ MILTy.toString ty
fun dump prln =
Var.Map.appi (fn (f, { uninstantiated, instances, funs }) =>
   if null instances then ()
   else 
   (
     prln (Int.toString (length instances) ^ " instantiation-sets for " ^ Pretty.simpleVec "," (Var.toString o #1) funs ^ ":");
     app (fn (typarsets, newfuns) =>
       prln ("  [" ^ Pretty.simpleVec "," dumpSpec typarsets ^ "]")) instances
   ))
   (!spec)

fun gatherStats () =
  Var.Map.foldr (fn ({ instances, ... }, acc) =>
    if null instances then acc
    else IntMap.insert(acc, length instances,
      getOpt(IntMap.find(acc, length instances), 0) + 1))
    IntMap.empty
    (!spec)  
in
  PrintManager.dump showStats (fn prln =>
    (prln ("Term grew from size " ^ Int.toString oldsize ^ " to size " ^ Int.toString newsize);
    prln ("Increase: " ^ Int.toString ((newsize*100) div oldsize - 100) ^ "%  ");
    IntMap.appi (fn (size, count) => prln (Int.toString size ^ " instantiations: " ^ Int.toString count))
    (gatherStats ())));

  PrintManager.dump showInsts dump;
  e
end 

end (* of local open MILTerm *)

val _ = Opts.add "mono" (transform, "Monomorphising")

end (* of struct *)