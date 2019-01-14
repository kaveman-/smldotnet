(*======================================================================*)
(* Scope analysis: determine which bindings (value, computation, and    *)
(* function) should be floated out or hoisted in.			*)
(*======================================================================*)
structure ScopeCalc :> SCOPECALC =
struct

local 
  open MILTerm ScopeTypes ScopeEnv
in

fun isCast (Special((Ext.Cast, _, _), _, _)) = true
  | isCast _ = false

val [floatVal, floatCmp, floatHigherFun, floatCast, hoistVal, hoistCmp] = 
  map (Controls.add true)
  ["floatVal", "floatCmp", "floatHigherFun", "floatCast", 
   "hoistVal", "hoistCmp"]

fun calc tyenv e =
let

(*----------------------------------------------------------------------*)
(* Smallest common scope of variable occurrences, used to determine	*)
(* where to hoist a binding.						*)
(*----------------------------------------------------------------------*)
  val occscopes = ref (Var.Map.empty : MILPath.Path Var.Map.map)

(*----------------------------------------------------------------------*)
(* Scope where definition appears					*)
(*----------------------------------------------------------------------*)
  val defscopes = ref (Var.Map.empty : MILPath.Path Var.Map.map)

(*----------------------------------------------------------------------*)
(* Bindings that should be floated outwards.				*)
(*----------------------------------------------------------------------*)
  val floatedbindings = 
    ref (Var.Map.empty : Binding list Var.Map.map)

  val replacedbindings =
    ref (Var.Map.empty : MILTerm.Val Var.Map.map)

(*----------------------------------------------------------------------*)
(* Bindings that should be hoisted inwards.				*)
(*----------------------------------------------------------------------*)
  val hoistedbindings =
    ref (MILPathOps.Map.empty : Binding MILPathOps.Map.map)

(*----------------------------------------------------------------------*)
(* Variables whose bindings get hoisted and floated			*)
(*----------------------------------------------------------------------*)
  val hoistedvars = ref Var.Set.empty
  val floatedvars = ref Var.Set.empty

(*----------------------------------------------------------------------*)
(* Add an occurrence scope for hoisting purposes			*)
(*----------------------------------------------------------------------*)
fun addOccScope (x,path) =
  case Var.Map.find(!occscopes, x) of
    NONE => 
    occscopes := Var.Map.insert(!occscopes, x, path)

  | SOME path' =>
    occscopes := Var.Map.insert(!occscopes, x, MILPathOps.join (path,path'))

(*----------------------------------------------------------------------*)
(* Add a definition scope.						*)
(*----------------------------------------------------------------------*)
fun addDef (env : Env) (x : MILTerm.BoundVar) =
  defscopes := Var.Map.insert(!defscopes, #1 x, #scope env)

fun addDefPath (x : MILTerm.BoundVar, path) =
  defscopes := Var.Map.insert(!defscopes, #1 x, path)

(*----------------------------------------------------------------------*)
(* Given a set of type variables, return list of kinded type vars	*)
(*----------------------------------------------------------------------*)
fun bindTyVars (env : Env) tvs =
  map (fn x => (x, valOf(Var.Map.find(#kindenv env, x))))
  (Var.Set.listItems tvs)

(*----------------------------------------------------------------------*)
(* What are the variables bound by this binding?			*)
(*----------------------------------------------------------------------*)
fun bound (ValBind(x,_)) = [x]
  | bound (CmpBind(xs,_)) = map #1 xs
  | bound (FunBind(_,_,Fun(f,_))) = [f]
  | bound (FunBind(_,_,RecFun defs)) = map (#1) defs

(*----------------------------------------------------------------------*)
(* Given a binding, add some extra type variables to the binding and	*)
(* rename the bound variables.						*)
(*----------------------------------------------------------------------*)
fun reAbstract (extratyvars, vars) b =
  case b of
    ValBind(x, TAbs(tyvars, v)) => 
    (tyvars, ValBind(hd vars, TAbs(extratyvars @ tyvars, v)))

  | ValBind(x, v) =>
    ([], ValBind(hd vars, TAbs(extratyvars, v)))

  | CmpBind _ =>
    Debug.fail "ScopeCalc.reAbstract: cannot add tyvars to a computation"

  | FunBind(tyvars, kind, Fun(f, a)) =>
    (tyvars, FunBind(extratyvars @ tyvars, kind, Fun(hd vars, a)))

  | FunBind(tyvars, kind, RecFun defs) =>
    (tyvars, FunBind(extratyvars @ tyvars, kind,  
      RecFun (ListPair.map (fn (f',(f,g,a,cty)) => (f',g,a,cty)) (vars,defs))))
  
(*----------------------------------------------------------------------*)
(* Add a floated binding			       			*)
(*----------------------------------------------------------------------*)
local
  fun addBinding f b =
  floatedbindings := 
    (case Var.Map.find(!floatedbindings, f) of
      NONE => Var.Map.insert(!floatedbindings, f, [b])
    | SOME bs => Var.Map.insert(!floatedbindings, f, b::bs))
in
fun addFloatedDefn f (binding,extratyvars) =
  if null extratyvars
  then
  (
    addBinding f binding;
    floatedvars := Var.Set.addList(!floatedvars, map #1 (bound binding))
  )
  else
  let
    val vars = bound binding
    val (vars',_) = Census.freshBoundVars 1 vars
    val (tyvars,newbinding) = reAbstract (extratyvars, vars') binding
  in
    addBinding f newbinding;
    replacedbindings := 
      ListPair.foldl (fn (var,var',m) => 
        Var.Map.insert(m, #1 var, 
          TAbs(tyvars, TApp(Var (#1 var'), map (MILTy.tyvar o #1) 
            (extratyvars @ tyvars)))))
        (!replacedbindings) (vars,vars');
    floatedvars := Var.Set.addList(!floatedvars, map #1 vars)
  end
end

(*----------------------------------------------------------------------*)
(* Remove some variables from a set.					*)
(* Ugly exception handling could be removed by writing our own sets.    *)
(*----------------------------------------------------------------------*)
fun remove (fvs, vars) =
  foldl (fn (v, m) => (Var.Set.delete(m, v)) handle _ => m) fvs vars

(*----------------------------------------------------------------------*)
(* Given an environment appropriate to a binding and the variables free *)
(* in the binding, decide whether or not to float the binding. If yes,  *)
(* return SOME (p,f) where p is the new path and f is the innermost     *)
(* enclosing function/method variable; otherwise return NONE.		*)
(*----------------------------------------------------------------------*)
fun tryToFloat (env : Env, fvs, flag) = 
let
  val path = #scope env
  val thislevel = length path
  val maxlength = 
    Var.Set.foldl 
    (fn (x, l) => 
      if Var.isDummy x then thislevel  (* Must NOT float Null *)
      else
      case Var.Map.find(!defscopes, x) of
        NONE => 
        thislevel

      | SOME defpath =>
        Int.max(length defpath, l))
    0
    fvs

  val optfun = 
    if maxlength >= thislevel then NONE
    else
      List.find (not o MILPathOps.canHoistThrough)
      (rev (List.take(path, thislevel - maxlength)))
in
  case optfun of
    SOME (MILPath.LetFun { var = f, ... }) =>
    if Controls.enabled flag
    then ((*Debug.print ("Floating from " ^ MILPathOps.toString path ^ " to " ^ MILPathOps.toString (List.drop (path, thislevel - maxlength)) ^ "\n");*)
      SOME (List.drop (path, thislevel - maxlength), f))
    else NONE

  | _ => 
    NONE
end 

(*----------------------------------------------------------------------*)
(* Given an environment appropriate to a binding and the bound variable	*)
(* itself, determine whether or not the binding should be hoisted.	*)
(*----------------------------------------------------------------------*)
fun tryToHoist (env : Env, xs, flag) =
let
  val optpath = 
    foldl (fn (x,optpath) =>
      case (Var.Map.find(!occscopes, x),optpath) of
        (NONE,optpath) => optpath
      | (optpath,NONE) => optpath
      | (SOME path1, SOME path2) => SOME (MILPathOps.join (path1,path2)))
      NONE xs
in
  case optpath of
    NONE => 
    NONE

  | SOME path =>
    let
      val len = length path - length (#scope env)
      val diff = List.take (path, len)
    in
      if len = 0 orelse (List.exists (not o MILPathOps.canHoistThrough) diff)
        orelse not (Controls.enabled flag)
      then NONE
      else SOME (strip path)
    end
end

(*----------------------------------------------------------------------*)
(* Given a list of types, return the free type variables		*)
(*----------------------------------------------------------------------*)
fun tyvarsTys tys = 
  foldl 
  (fn (ty,tvs) => Var.Set.union (MILTy.tyvars ty,tvs)) 
  Var.Set.empty 
  tys

fun tyvarsCTy cty = tyvarsTys (#2 (MILTy.fromCmp cty))

(*----------------------------------------------------------------------*)
(* Given a value term, analyse flow and return:				*)
(*   (1) its free variables;						*)
(*   (2) its free type variables;					*)
(*   (3) its type.							*)
(*----------------------------------------------------------------------*)
fun flowVal (env : Env) v =
  case v of

(*......................................................................*)
(* Constant introduction                    				*)
(*......................................................................*)
  SCon(ty,_) => 
  (Var.Set.empty, Var.Set.empty, ty)
  
(*......................................................................*)
(* Variables								*)
(*......................................................................*)
| Var x => 
  (addOccScope (x, #scope env);
  (Var.Set.singleton x, Var.Set.empty, Var.lookup(#tyenv env, x)))

(*......................................................................*)
(* Mu introduction							*)
(*......................................................................*)
| Fold(v, ty) =>
  let
    val (fvs, tvs, _) = flowVal env v
  in
    (fvs, Var.Set.union(MILTy.tyvars ty, tvs), ty)
  end

(*......................................................................*)
(* Mu elimination                     					*)
(*......................................................................*)
| Unfold v =>
  let
    val (fvs, tvs, ty) = flowVal env v
    val SOME a = MILTy.fromMu ty
  in
    (fvs, tvs, MILTy.unfold a)
  end

(*......................................................................*)
(* Product elimination							*)
(*......................................................................*)
| Proj(i, n, v) =>
  let
    val (fvs, tvs, ty) = flowVal env v
    val SOME tys = MILTy.fromProdCon ty
  in
    (fvs, tvs, List.nth(tys, i))
  end

(*......................................................................*)
(* Sum introduction                                			*)
(*......................................................................*)
| Inj(ty, i, vs, _) => 
  let
    val (fvs, tvs, _) = flowVals env vs
  in
    (fvs, Var.Set.union(MILTy.tyvars ty, tvs), ty)
  end

(*......................................................................*)
(* Exception 				                                *)
(*......................................................................*)
| ExCon(_, vs) =>
  let 
    val (fvs, tvs, _) = flowVals env vs
  in
    (fvs, tvs, MILTys.topExn)
  end
  
(*......................................................................*)
(* Product introduction			                                *)
(*......................................................................*)
| Tuple vs =>
  let 
    val (fvs, tvs, tys) = flowVals env vs
  in
    (fvs, tvs, MILTy.prod tys)
  end

(*......................................................................*)
(* Quantifier elimination						*)
(*......................................................................*)
| TApp(v, tys) => 
  let
    val (fvs, tvs, ty) = flowVal env v
    val SOME a = MILTy.fromForall ty
  in
    (fvs, Var.Set.union (tvs, tyvarsTys tys), MILTy.app(MILTy.abs a, tys))
  end

(*......................................................................*)
(* Quantifier introduction						*)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val (fvs, tvs, ty) = flowVal (envPlusTyVars env tyvars) v
  in
    (fvs, Var.Set.difference (tvs, 
      Var.Set.addList(Var.Set.empty, map #1 tyvars)), MILTy.forall(tyvars, ty))
  end

| As(v, ty) =>
  let
    val (fvs, tvs, _) = flowVal env v
  in
    (fvs, Var.Set.union(tvs, MILTy.tyvars ty), ty)
  end

(*----------------------------------------------------------------------*)
(* Analyse flow for a vector of terms, accumulating free variables.	*)
(*----------------------------------------------------------------------*)
and flowVals env [] = 
    (Var.Set.empty, Var.Set.empty, [])

  | flowVals env (v::vs) =
    let
      val (fvs,tvs,ty) = flowVal env v
      val (fvs',tvs',tys) = flowVals env vs
    in
      (Var.Set.union(fvs,fvs'), Var.Set.union(tvs,tvs'), ty::tys)
    end

(*----------------------------------------------------------------------*)
(* Analyse flow for a typed abstraction					*)
(*----------------------------------------------------------------------*)
and flowTAbstr (env : Env) ((xs, e) : MILTerm.TAbstr) =
let
  val _ = app (addDef env o #1) xs
  val (fvs, tvs, cty) = flowCmp (envPlusTypedVars env xs) e
  val tvs' = tyvarsTys (map #2 xs)
in
  (remove (fvs, map (#1 o #1) xs), Var.Set.union(tvs, tvs'), cty)
end

(*----------------------------------------------------------------------*)
(* Analyse flow for an abstraction					*)
(*----------------------------------------------------------------------*)
and flowAbstr (env : Env) tys (xs, e) =
let
  val _ = app (addDef env) xs
  val (fvs, tvs, cty) = flowCmp (envPlusBoundVars env xs tys) e
in
  (remove (fvs, map #1 xs), tvs, cty)
end

(*----------------------------------------------------------------------*)
(* Given a computation term, analyse flow and return			*)
(*   (1) Its free variables;						*)
(*   (2) its free type variables;					*)
(*   (3) its type.							*)
(*----------------------------------------------------------------------*)
and flowCmp (env : Env) e =
let
  fun flowCases (tysfor, itemf) (cases, optdef) =
    let 
      val (fvs, tvs) = 
        case optdef of
          NONE => (Var.Set.empty, Var.Set.empty)
        | SOME e => let val (fvs, tvs, _) = flowCmp (envPlusScope env (itemf NONE)) e in (fvs,tvs) end

      fun flowCase ((i, abs), (fvs', tvs')) =
        let
          val env = envPlusScope env (itemf (SOME i))
          val (fvs, tvs, cty) = flowAbstr env (tysfor i) abs 
        in
          (Var.Set.union(fvs, fvs'), Var.Set.union(tvs, tvs'))
        end
    in
      foldr flowCase (fvs, tvs) cases
    end
in
  case e of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(v, vs) =>
  let
    val (fvs1, tvs1, ty) = flowVal env v
    val (fvs2, tvs2, _) = flowVals env vs
    val SOME (_, cty) = MILTy.fromArrow ty
  in
    (Var.Set.union(fvs1, fvs2), Var.Set.union(tvs1, tvs2), cty)
  end

(*......................................................................*)
(* Special operation (tyvars cannot be free in cty)			*)
(*......................................................................*)
| Special(j, vs, cty) =>
  let
    val (fvs, tvs, _) = flowVals env vs
  in
    (fvs, tvs, cty)
  end

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, tabs as (xs,e2)) =>
  let
    val (fvs1, tvs1, cty1) = flowCmp env e1
    val (eff,_) = MILTy.fromCmp cty1
  in
    case (if Effect.isNone eff andalso Var.Set.isEmpty tvs1 
          then tryToFloat (env, fvs1, 
            if isCast e1 then floatCast else floatCmp)
          else NONE) of
      SOME (path, f) =>
      let 
        val _ = addFloatedDefn f (CmpBind(xs, e1), [])
        val _ = app (fn (x,_) => addDefPath (x, path)) xs
        val (fvs2, tvs2, cty2) = flowCmp (envPlusTypedVars env xs) e2
      in  
        (Var.Set.union(fvs1, remove (fvs2, map (#1 o #1) xs)), 
         Var.Set.union(tvs1,tvs2), MILTy.unionCmpTypes(cty1, cty2))
      end

    | NONE =>
      let
        val (fvs2, tvs2, cty2) = flowTAbstr env tabs
        val _ = 
        case (if Effect.isNone eff then tryToHoist (env, map (#1 o #1) xs, hoistCmp) 
              else NONE) of
          NONE => 
          ()

        | SOME newpath =>
          (hoistedbindings := 
            MILPathOps.Map.insert(!hoistedbindings, newpath, CmpBind (xs,e1));
          hoistedvars := Var.Set.addList(!hoistedvars, map (#1 o #1) xs))
      in
        (Var.Set.union(fvs1, fvs2), Var.Set.union(tvs1, tvs2),
          MILTy.unionCmpTypes(cty1, cty2))
      end
  end
    
(*......................................................................*)
(* Value bindings                                                       *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val (fvs1, tvs1, ty1) = flowVal env v
  in
    case tryToFloat (env, fvs1, floatVal) of
      SOME (path, f) =>
      let
        val _ = addFloatedDefn f (ValBind(x, v), bindTyVars env tvs1)
        val _ = addDefPath (x, path)
        val (fvs2, tvs2, cty) = flowCmp (envPlusTypedVars env [(x,ty1)]) e
      in  
        (Var.Set.union(fvs1, remove (fvs2, [#1 x])), Var.Set.union(tvs1,tvs2),
          cty)
      end

    | NONE =>
      let
        val _ = addDef env x
        val (fvs2, tvs2, cty) = flowCmp (envPlusTypedVars env [(x,ty1)]) e
        val _ = 
        case tryToHoist (env, [#1 x], hoistVal) of
          NONE => 
          ()

        | SOME newpath =>
          (hoistedbindings := 
            MILPathOps.Map.insert(!hoistedbindings, newpath, ValBind (x,v));
          hoistedvars := Var.Set.add(!hoistedvars, #1 x))
      in
        (Var.Set.union(fvs1, remove (fvs2, [#1 x])), Var.Set.union(tvs1, tvs2),
          cty)
      end
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  let
    val (fvs, tvs, tys) = flowVals env vs
  in
    (fvs, tvs, MILTy.noeffect tys)
  end

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  let
    val (fvs, tvs, cty) = flowCmp env e
    val (_, tys) = MILTy.fromCmp cty
  in
    (fvs, tvs, MILTy.cmp(Effect.allocs, tys))
  end

(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case (v,cases,default,cty) =>
  let
    val (fvs, tvs, ty) = flowVal env v
    val SOME tyss = MILTy.fromSum ty
    fun tysfor i = List.nth(tyss, i)
    val (fvs', tvs') = flowCases (tysfor, MILPath.CaseCon) (cases,default)
  in
    (Var.Set.union(fvs, fvs'), Var.Set.union(tvs, tvs'), cty)
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (v, cases, default, cty) =>
  let
    val (fvs, tvs, _) = flowVal env v
    val (fvs', tvs') = 
      flowCases (fn _ => [], MILPath.CaseSCon) (cases, default)
  in
    (Var.Set.union(fvs, fvs'), Var.Set.union(tvs, tvs'), cty)
  end
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| TypeCase (v, cases, default, cty) =>
  let
    val (fvs, tvs, _) = flowVal env v
    fun tysfor ty = [ty]
    val (fvs', tvs') = flowCases (tysfor,MILPath.TypeCase) (cases,default)
  in
    (Var.Set.union(fvs, fvs'), Var.Set.union(tvs, tvs'), cty)
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(v, cty, loc) =>
  let
    val (fvs, tvs, _) = flowVal env v
  in
    (fvs, Var.Set.union(tvs, tyvarsCTy cty), cty)
  end

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(e0, handlers, tabs) =>
  let
    val (fvs0, tvs0, cty0) = flowCmp env e0
    val (fvs2, tvs2, cty2) = flowTAbstr env tabs

    fun flowHandler (abs, (fvs, tvs, cty)) =
    let
      val (fvs', tvs', cty') = flowTAbstr env abs
    in
      (Var.Set.union(fvs, fvs'), Var.Set.union(tvs, tvs'),
        MILTy.unionCmpTypes(cty, cty'))
    end

    val (fvs1, tvs1, cty1) = foldr flowHandler (fvs2,tvs2,cty2) handlers
  in
    (Var.Set.union(fvs0, fvs1), Var.Set.union(tvs0, tvs1),
    MILTy.unionCmpTypes(cty0, cty1))
  end

(*......................................................................*)
(* Internal class definition						*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun flowMethod ((name, atts, mods, tys, tyopt, optabs) : MILTerm.MethodInfo, (fvs', tvs')) =
        case optabs of 
          NONE => 
          (fvs', tvs')

        | SOME (f, abs) =>
          let
            val argtys = 
              if Symbol.Set.member(mods, Id.staticSym)
              then tys
              else classname::tys
            val env = envPlusScope env 
              (MILPath.LetFun { var = #1 f, recursive = false, kind = NONE })
            val (fvs, tvs, _) = flowAbstr env argtys abs 
          in
            (Var.Set.union(fvs, fvs'), Var.Set.union(tvs, tvs'))
          end
    val (fvs, tvs) = foldl flowMethod (Var.Set.empty, Var.Set.empty) methods
    val (fvs', tvs', cty) = flowCmp env e
  in
    (Var.Set.union(fvs, fvs'), Var.Set.union(tvs, tvs'), cty)
  end

(*......................................................................*)
(* Recursive and non-recursive functions.                               *)
(*......................................................................*)
| LetFun(tyvars, kind, fundef, e) =>
  let
    val defnenv = envPlusTyVars env tyvars
    val defnenv = 
      case fundef of 
        RecFun recbinds =>
        envPlusTypedVars defnenv (map (fn (_,g,(xs,_),cty) =>
          (g, MILTy.arrow(map #2 xs, cty))) recbinds)
      | _ => defnenv

    val (fvs, tvs, fs, bodyenv) =
    case fundef of
      RecFun recbinds =>
      let
        val fs = map #1 recbinds
        val gs = map #2 recbinds

        val _ = app (addDef env) fs
        val _ = map (fn (f,g,_,_) => addDefPath (g, MILPath.LetFun { var = #1 f, recursive = true, kind = SOME kind } :: #scope env)) recbinds

        fun flowDef ((f : MILTerm.BoundVar, g, tabs, cty), (fvs, tvs)) =
        let
	  val defnenv = envPlusScope defnenv 
            (MILPath.LetFun { var = #1 f, recursive = true, kind = SOME kind })
          val (fvs', tvs', _) = flowTAbstr defnenv tabs
          val tvs' = remove(tvs', map #1 tyvars)
        in
          (Var.Set.union(fvs, fvs'), Var.Set.union(tvs, tvs'))
        end

        val (fvs, tvs) = foldl flowDef (Var.Set.empty, Var.Set.empty) recbinds 
      in
        (fvs, tvs, fs, envPlusTypedVars env
          (map (fn (f,_,(xs,_),cty) => (f, MILTy.forall(tyvars,
             MILTy.arrow(map #2 xs, cty)))) recbinds))
      end

    | Fun (f, tabs as (xs,_)) =>
      let
        val defnenv = envPlusScope defnenv 
          (MILPath.LetFun { var = #1 f, recursive = false, kind = SOME kind })
        val (fvs, tvs, cty) = flowTAbstr defnenv tabs
        val tvs = remove (tvs, map #1 tyvars) 
      in
        (fvs, tvs, [f], envPlusTypedVars env [(f, MILTy.forall(tyvars,
          MILTy.arrow(map #2 xs, cty)))])
      end
  in
    if kind=AnyFun
    then
      case tryToFloat (env, fvs, floatHigherFun) of
        SOME (path, f) => 
        (addFloatedDefn f (FunBind(tyvars, kind, fundef), bindTyVars env tvs);
         app (fn f => addDefPath (f, path)) fs)

      | _ => ()
    else ();
    let
      val (fvs', tvs', cty) = flowCmp bodyenv e
      val fvs' = remove (fvs', map #1 fs)
    in
      (Var.Set.union(fvs, fvs'), Var.Set.union(tvs, tvs'), cty)
    end
  end

end

  val _ = Controls.reset ()

  val _ = flowCmp (initialEnv tyenv) e

  val _ = Controls.printCounts PrintManager.print

  val info =
  { floatedbindings = !floatedbindings,
    hoistedbindings = !hoistedbindings,
    replacedbindings = !replacedbindings,
    hoistedvars = !hoistedvars,
    floatedvars = !floatedvars,
    funkinds = Var.Map.empty
  }

in
  ScopePretty.dump info;
  info
end

end (* of local open *)
end (* of struct *)