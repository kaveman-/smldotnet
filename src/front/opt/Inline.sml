(*======================================================================*)
(* General function inlining transformation				*)
(*======================================================================*)
structure Inline :> TRANSFORMER =
struct

local 
  open MILTerm MILTermOps
in

val maxInlineSize = ref 8
val maxInlineSizeR = ref 12
val maxUnrollSize = ref 12

val inline = Controls.add true "inline.fun"
val unroll = Controls.add false "inline.unroll"
val show = Controls.add false "inline.show"

fun doInline e = 
  not (MILTermOps.cmpBigger (e, !maxInlineSize)) 
  andalso Controls.enabled inline

(*----------------------------------------------------------------------*)
(* Transform the computation term e.					*)
(*----------------------------------------------------------------------*)
fun transform tyenv e =
let

(*----------------------------------------------------------------------*)
(* Empty environment        						*)
(*----------------------------------------------------------------------*)
val emptyEnv = 
{ 
  (* Functions to be inlined *)
  inlined = Var.Map.empty, 

  (* Types of value variables *)
  tyenv = tyenv,           
  
  (* Kinds of type variables *)
  kindenv = Var.Map.empty
}

(*----------------------------------------------------------------------*)
(* Extend the type environment      					*)
(*----------------------------------------------------------------------*)
fun envPlusTypedVars 
  { inlined, tyenv, kindenv } xs =
  { inlined = inlined, tyenv = addTypedVars(tyenv, xs), kindenv = kindenv }

(*----------------------------------------------------------------------*)
(* Extend the kind environment      					*)
(*----------------------------------------------------------------------*)
fun envPlusTyVars 
  { inlined, tyenv, kindenv} tyvars =
  { inlined = inlined, tyenv = tyenv, kindenv = addTyVars(kindenv, tyvars) }

(*----------------------------------------------------------------------*)
(* Add a function to the inlinable functions 				*)
(*----------------------------------------------------------------------*)
fun envPlusInlined 
  { inlined, tyenv, kindenv} ((x,_),a) =
  { inlined = Var.Map.insert(inlined, x, a), tyenv = tyenv, kindenv = kindenv }

(*----------------------------------------------------------------------*)
(* Transforms a computation term.					*)
(*----------------------------------------------------------------------*)
fun transCmp env ce =
let
  fun transCases tysFor (v, cases, optdefault, cty) =
    let 
      val defresult = Option.map (transCmp env) optdefault
      fun transCase (i, (vs, ce)) =
        let
          val tys = tysFor i 
          val (ce, cty') = transCmp (envPlusTypedVars env (ListPair.zip(vs, tys))) ce
        in
          (i, (vs, ce))
        end
      val cases = map transCase cases
    in
      case defresult of
        NONE => 
        (v, cases, optdefault, cty)

      | SOME (ce,_) => 
        (v, cases, SOME ce, cty)
    end

  fun transHandler ((xs, ce), (result, cty)) =
      let
        val (ce, cty') = transCmp (envPlusTypedVars env xs) ce
      in
        ((xs, ce)::result, MILTy.unionCmpTypes(cty,cty'))
      end

(*......................................................................*)
(* First float some bindings if the scope is about to change		*)
(*......................................................................*)
in
  case ce of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(Var v, velist) =>
  let
    val funty = 
    case Var.Map.find(#tyenv env, v) of
      NONE => Debug.fail
      ("GlobalOpt.transCmp: variable not in environment: " ^ Var.toString v)
    | SOME ty => ty

    val SOME (_, cty) = MILTy.fromArrow funty 
  in
    case Var.Map.find(#inlined env, v) of
      SOME ([], tabs) =>
      let
        val (typedvars, body) = Census.renameTAbstr tabs
      in
        Census.addVar (v, ~1); 
        (ListPair.foldr (fn ((var,ty),arg,e) => LetVal(var,arg,e))
          body (typedvars, velist), cty)
      end

    | _ => 
      (App(Var v, velist), cty)
  end

(*......................................................................*)
(* Arrow elimination: polymorphic variable       			*)
(*......................................................................*)
| App(TApp(Var v, tys), velist) =>
  let
    val polyty = 
    case Var.Map.find(#tyenv env, v) of
      NONE => Debug.fail
      ("GlobalOpt.transCmp: variable not in environment: " ^ Var.toString v)
    | SOME ty => ty

    val SOME a = MILTy.fromForall polyty

    val SOME (_, cty) = MILTy.fromArrow (MILTy.app (MILTy.abs a, tys)) 
  in
    case Var.Map.find(#inlined env, v) of
      SOME (tyvars, tabs) =>
      let
        val (typedvars, body) = Census.renameTAbstr tabs
        val S = ListPair.foldl (fn ((x,_),ty,S) => Var.Map.insert(S,x,ty))
          Var.Map.empty (tyvars,tys)
      in
        Census.addVar (v, ~1); 
        (ListPair.foldr (fn ((var,ty),arg,e) => LetVal(var,arg,e))
          (MILTermOps.substCmp S body) (typedvars, velist), cty)
      end

    | NONE => 
      (App(TApp(Var v, tys), velist), cty)
  end

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
| App(v, vs) =>
  let
    val funty = MILTermOps.typeOfVal (#tyenv env) v
    val SOME (_, cty) = MILTy.fromArrow funty 
  in
    (ce, cty)
  end

(*......................................................................*)
(* Runtime operation							*)
(*......................................................................*)
| Special(j, velist, cty) =>
  (ce, cty)

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, (xs, e2)) =>
  let
    val (e1,cty1) = transCmp env e1
    val (_,tys) = MILTy.fromCmp cty1
    val (e2,cty2) = transCmp (envPlusTypedVars env xs) e2
  in
    (Let(e1, (xs, e2)), MILTy.unionCmpTypes (cty1, cty2))
  end
    
(*......................................................................*)
(* Value bindings                                                       *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val ty = MILTermOps.typeOfVal (#tyenv env) v
    val env = envPlusTypedVars env [(x,ty)]
    val (e, cty) = transCmp env e
  in
    (LetVal(x, v, e), cty)
  end      

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  let
    val (e, cty) = transCmp env e
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap e, MILTy.cmp(Effect.allocs, tys))
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  (ce, MILTy.noeffect (map (MILTermOps.typeOfVal (#tyenv env)) vs))

(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case (v, cases, default, cty) =>
  let 
    val ty = MILTermOps.typeOfVal (#tyenv env) v
    val SOME tyss = MILTy.fromSum ty

    fun tysFor i = List.nth(tyss, i)

    val (result) = transCases tysFor (v, cases, default, cty)
  in
    (Case result, cty)
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (a as (_,_,_,cty)) =>
  let
    val result = transCases (fn c => []) a
  in
    (CaseSCon result, cty)
  end
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| TypeCase (a as (_,_,_,cty)) =>
  let
    fun tysFor ty =  [ty]
    val result = transCases tysFor a
  in
    (TypeCase result, cty)
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(v, cty, loc) =>
  (Throw(v, cty, loc), cty)

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(ce0, tabss, (vs, ce2)) =>
  let
    val (ce0, cty0) = transCmp env ce0
    val (ce2, cty2) = transCmp (envPlusTypedVars env vs) ce2
    val (tabss, cty1) = foldr transHandler ([],cty2) tabss
  in
    (TryLet(ce0, tabss, (vs, ce2)), MILTy.unionCmpTypes (cty0, cty1))
  end

(*......................................................................*)
(* Internal class definition						*)
(*......................................................................*)
| LetClass(classname,classinfo, fields, methods, e) =>
  let
    val methodvars = 
      List.mapPartial (fn (_,_,_,_,_,SOME(x,_)) => SOME x | _ => NONE) methods

    fun transMethod (name, atts, mods, tys, tyopt, optabs) =
        (name, atts, mods, tys, tyopt, 
          case optabs of 
            NONE => NONE
          | SOME (f,(vs, ce)) =>
            let
              val argtys = 
                if Symbol.Set.member(mods, Id.staticSym)
                then tys
                else classname::tys
              val env = envPlusTypedVars env (ListPair.zip(vs,argtys))
            in
              SOME (f,(vs, #1 (transCmp env ce)))
            end)
    val methods = map transMethod methods
    val (e,cty) = transCmp env e
    val (eff, tys) = MILTy.fromCmp cty
  in
    (LetClass(classname,classinfo, fields, methods, e), 
      MILTy.cmp(Effect.union(eff, Effect.io), tys))
  end

(*......................................................................*)
(* Recursive functions.                                                 *)
(*......................................................................*)
| LetFun(tyvars, kind, RecFun recbinds, e) =>
  let
    fun makeFunTypes (funvar1,funvar2,(typedvars,ce):MILTerm.TAbstr,cty) =
      ((funvar1, MILTy.forall(tyvars, MILTy.arrow(map #2 typedvars, cty))),
      (funvar2, MILTy.arrow(map #2 typedvars, cty)))

    val pairs = map makeFunTypes recbinds
    val (bodyfuns,defnfuns) = ListPair.unzip pairs
    val defnenv = envPlusTyVars (envPlusTypedVars env defnfuns) tyvars

    val bodyenv = envPlusTypedVars env bodyfuns

    val defnenv = foldl (fn ((_,g,(vs,e),_),env) =>
        if not (MILTermOps.cmpBigger(e, !maxUnrollSize)) 
        andalso Controls.enabled unroll
        then envPlusInlined env (g, ([], (vs, e)))
        else env) defnenv recbinds

    fun transDef (funvar1, funvar2, (typedvars, e), cty) =
        let
          val (e, cty) = transCmp (envPlusTypedVars defnenv typedvars) e
        in
          (funvar1, funvar2, (typedvars, e), cty)
        end

    val newdef = RecFun (map transDef recbinds)

    val (e,cty) = transCmp bodyenv e
  in
    (LetFun(tyvars, kind, newdef, e), cty)
  end

(*......................................................................*)
(* Non-recursive function defn: these can be inlined.       		*)
(*......................................................................*)
| LetFun(tyvars, kind, Fun (f, (typedvars,e1)), e2) =>
  let
    val defnenv = envPlusTyVars env tyvars

    val (e1, cty) = transCmp (envPlusTypedVars defnenv typedvars) e1

    val bodyenv = 
      if MILTermOps.isLocal kind then env
      else 
      if doInline e1 andalso Controls.enabled inline
        then 
        (
(*          if Controls.get show
          then 
	   Debug.print ("\nInline " ^ MILPretty.boundVarToString f ^ 
	   "size " ^ Int.toString (MILTermOps.absSize e1)^
	   "= (" ^ Pretty.simpleVec "," (fn (x,ty) => MILPretty.boundVarToString x ^ ":" 
            (* ^ MILTy.toString ty *)) typedvars ^ ")" ^ MILPretty.cmpToString e1)
          else ();
*)
          envPlusInlined env (f, (tyvars, (typedvars, e1)))
        )
        else env

    val bodyenv = envPlusTypedVars bodyenv [(f,
      MILTy.forall(tyvars, MILTy.arrow(map #2 typedvars, cty)))]

    val newdef = Fun (f, (typedvars, e1))
    val (e2, cty) = transCmp bodyenv e2
  in
    (LetFun(tyvars, kind, newdef, e2), cty)
  end
end

val _ = Controls.reset ()

(*----------------------------------------------------------------------*)
(* Do it!								*)
(*----------------------------------------------------------------------*)
val (e, cty) = transCmp emptyEnv e

in
  Controls.printCounts PrintManager.print; 
  e
end

end (* of local open *)

val _ = Opts.add "inline" (transform, "Inlining functions")

end (* of struct *)