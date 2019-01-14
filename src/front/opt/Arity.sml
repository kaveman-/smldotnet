(*======================================================================*)
(* Arity-raise and de-curry known functions.                            *)
(*======================================================================*)
structure Arity :> 
sig
  val transform : Opt.Transformer
  val full : Opt.Transformer
end
=
struct

local 
  open MILTerm MILTermOps
in

val [argArity, resultArity, uncurry] = 
    map (Controls.add true)
    ["argArity", "resultArity", "uncurry"]

(*----------------------------------------------------------------------*)
(* The different varieties of iso.					*)
(*----------------------------------------------------------------------*)
type Isos = { argArity : bool, resArity : bool, uncurry : bool }

(*----------------------------------------------------------------------*)
(* Apply a transformation to the term e 				*)
(*----------------------------------------------------------------------*)
fun transform tyenv e =
let

(*----------------------------------------------------------------------*)
(* Given a value term return its type.                            	*)
(*----------------------------------------------------------------------*)
fun transVal (env as (funs,tyenv,kindenv,symenv)) v = MILTermOps.typeOfVal tyenv v
and transCmp (env as (funs,tyenv,kindenv,symenv)) e =
let
  fun transCase tysFor (i, (vs, e)) =
      let 
        val tys = tysFor i
        val (e, cty') = transCmp (funs, addBoundVars(tyenv, vs, tys), kindenv, addBoundSymbols(symenv,vs)) e
      in
        (i, (vs, e))
      end

  fun transCases tysFor (cases, defopt) = (map (transCase tysFor) cases, Option.map (#1 o transCmp env) defopt)
in
  case e of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(v, vs) =>
  let
    val funty = transVal env v
    val tys = map (transVal env) vs
  in
  case MILTy.fromArrow funty of
    NONE => 
    MILPretty.failCmp e "Arity.transCmp: expected arrow type"

  | SOME (_, cty) => 
    let
      val (eff,restys) = MILTy.fromCmp cty
      fun default () = (App(v, vs), cty)

      fun whichIsos (Var x) = Var.Map.find(funs, x)
        | whichIsos (TApp(Var x, _)) = Var.Map.find(funs, x)
        | whichIsos _ = NONE
    in
      case whichIsos v of
        NONE => default ()

      | SOME { argArity, resArity, uncurry } =>
        let
          fun doResult (C,args) = 
            case restys of
              [resty] =>
              (case MILTy.fromProd resty of
                SOME [] => 
                if resArity 
                then (C (Let(App(v, args), ([], Triv [Tuple []]))), cty)
                else (C (App(v, args)), cty)

              | _ => 
                (case (Effect.isNone eff andalso uncurry, 
                  MILTy.fromArrow resty) of
                    (true, SOME (arg2tys, cty2)) =>
                    let
                      val (xs,xvs) = Census.freshTypedAnonVars 1 arg2tys
                      val (f,fv) = Census.freshBoundAnonVar 1
                    in
                      (C (LetFun([], AnyFun, Fun (f, (xs, App(v, args @ xvs))), 
                        Triv [fv])), cty)
                    end

                  | _ => (C (App(v, args)), cty)
                )
              )

            | _ => (C (App(v, args)), cty)
        in
          case (vs, tys, argArity) of
            ([arg], [argty], true) => 
            (case MILTy.fromProd argty of
              SOME prodtys =>
              let
                val (xs,xvs) = Census.freshTypedAnonVars 1 prodtys
                val n = length prodtys
              in
                Census.addVal (arg, length xs - 1);
                doResult 
                  (fn e' => ListOps.foldli 
                  (fn (i, ((x,si),_), e) => 
                      let val proji = Proj(i,n,arg) 
                          val si = symbolOfVal symenv proji
                      in
                         LetVal((x,si),proji, e) 
                      end
                   ) e' xs,
                  xvs)
              end

            | NONE => 
              doResult (Gen.identity, [arg])
            )

          | _ =>
            doResult (Gen.identity, vs)
        end
    end  
  end

(*......................................................................*)
(* Runtime operation							*)
(*......................................................................*)
| Special(_,_,cty) =>
  (e, cty)

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, (xs, e2)) =>
  let
    val (e1, cty1) = transCmp env e1
    val (effect, tys) = MILTy.fromCmp cty1
    val (e2, cty2) = transCmp (funs, addTypedVars(tyenv, xs), kindenv, addTypedSymbols(symenv,xs)) e2
  in
    (Let(e1, (xs, e2)), MILTy.unionCmpTypes(cty1,cty2))
  end
    
(*......................................................................*)
(* Value bindings.                                                      *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val ty = transVal env v
    val tyenv = addBoundVar(tyenv, x, ty)
    val symenv = addBoundSymbol(symenv, x)
    val (e, cty) = transCmp (funs, tyenv, kindenv,symenv) e
  in
    (LetVal(x, v, e), cty)
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  let
    val tys = map (transVal env) vs
  in
    (Triv vs, MILTy.noeffect tys)
  end

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  let
    val (e, cty) = transCmp env e
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap e, MILTy.cmp (Effect.allocs, tys))
  end

(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case(v, cases, optdefault, cty) =>
  let 
    val ty = transVal env v
    val tyss = 
      case MILTy.fromSum ty of
        SOME tyss => tyss
      | NONE => MILPretty.failVal v "Arity.transCmp: expected sum type"
    fun tysFor i = List.nth(tyss, i)

    val (cases, optdefault) = transCases tysFor (cases, optdefault)
  in
    (Case(v, cases, optdefault, cty), cty)   
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (v, cases, optdefault, cty) =>
  let
    val (cases, optdefault) = transCases (fn _ =>[]) (cases,optdefault)
  in
    (CaseSCon(v, cases, optdefault, cty), cty)
  end
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| TypeCase (v, cases, optdefault, cty) =>
  let
    fun tysFor ty = [ty]

    val (cases, optdefault) = transCases tysFor (cases,optdefault)
  in
    (TypeCase(v, cases, optdefault, cty), cty)
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
    val (ce2, cty2) = transCmp (funs, addTypedVars(tyenv, vs), kindenv, addTypedSymbols(symenv,vs)) ce2
    fun transHandler ((typedvars, ce), (result, cty)) =
        let
          val (ce, cty') = 
            transCmp (funs, addTypedVars(tyenv, typedvars), kindenv, addTypedSymbols(symenv,typedvars)) ce
        in
          ((typedvars, ce)::result, MILTy.unionCmpTypes(cty,cty'))
        end

    val (tabss, cty1) = foldr transHandler ([], cty2) tabss
  in
    (TryLet(ce0, tabss, (vs, ce2)), MILTy.unionCmpTypes(cty0,cty1))
  end

(*......................................................................*)
(* Internal class definition						*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun transMethod (name, atts, mods, tys, tyopt, optabs) =
        (name, atts, mods, tys, tyopt,
          case optabs of 
            NONE => NONE
          | SOME (f,(vs, e)) =>
            let
              val argtys = 
                if Symbol.Set.member(mods, Id.staticSym)
                then tys
                else classname::tys
              val tyenv = addBoundVars(tyenv, vs,argtys)
              val symenv = addBoundSymbols(symenv, vs)
              val (e, cty) = transCmp (funs, tyenv, kindenv,symenv) e
            in
              SOME (f,(vs, e))
            end)

    val (e, cty) = transCmp env e
    val methods = map transMethod methods
    val (eff, tys) = MILTy.fromCmp cty
  in
    (LetClass(classname, classinfo, fields, methods, e), 
      MILTy.cmp(Effect.union(eff, Effect.io), tys))
  end
  
(*......................................................................*)
(* Recursive functions.                                                 *)
(*......................................................................*)
| LetFun(tyvars, funkind, RecFun recbinds, ce) =>
  let
    fun makeFunType (_, _, (vs : MILTerm.TypedVar list,ce),cty) = 
      MILTy.arrow(map #2 vs, cty)
    val funtys = map makeFunType recbinds
    val defbvs = map #2 recbinds
    val defntyenv = addBoundVars(tyenv, defbvs, funtys)
    val defnsymenv = addBoundSymbols(symenv, defbvs)
    val defnkindenv = addTyVars(kindenv, tyvars)

    val bodybvs = map #1 recbinds
    val bodytyenv = addBoundVars(tyenv, bodybvs,
                                 map (fn ty => MILTy.forall(tyvars, ty)) funtys)
    val bodysymenv = addBoundSymbols(symenv,bodybvs)

    fun doIso (((f,_), (g,_), (typedvars, ce), cty), funs)  =
      if funkind <> AnyFun 
      then
        let
          val argArity = 
            (case typedvars of
              [(_,ty)] => isSome (MILTy.fromProd ty)
            | _ => false) 
            andalso Controls.enabled argArity

          val (resArity, uncurry) = 
            case MILTy.fromCmp cty of
              (eff,[ty]) => 
              (case MILTy.fromProd ty of
                 SOME [] => 
                 (Controls.enabled resultArity, false) 

               | _ => 
                if Effect.isNone eff andalso isSome (MILTy.fromArrow ty)
                then (false, Controls.enabled uncurry) 
                else (false, false)
              )
            | _ => (false, false)
          val entry = { resArity = resArity, argArity = argArity, uncurry = uncurry }
        in
          Var.Map.insert(Var.Map.insert(funs, f, entry), g, entry)
        end
      else funs

    val funs = foldl doIso funs recbinds

    fun transDef (f : BoundVar, g, (typedvars, ce), cty) =
    let
      val (ce, _) = 
        transCmp (funs, addTypedVars(defntyenv,typedvars), defnkindenv, addTypedSymbols(defnsymenv,typedvars)) ce

      fun default () = (f, g, (typedvars, ce), cty)

      val { argArity, resArity, uncurry } = 
        getOpt(Var.Map.find(funs,#1 f), 
        {argArity = false, resArity = false, uncurry = false})

      fun doResult (tabs as (typedvars, e)) =
        case MILTy.fromCmp cty of
          (eff,[ty]) =>
          (case MILTy.fromProd ty of
            SOME [] => 
            if resArity
            then (f, g, (typedvars, Let(e, 
              ([(dummyBoundVar, ty)], Triv []))), MILTy.cmp(eff, []))
            else (f, g, tabs, cty)

          | _ => 
            (case (Effect.isNone eff andalso uncurry, 
               MILTy.fromArrow ty) of
               (true, SOME (arg2tys, cty2)) =>
               let
                 val (xs,xvs) = Census.freshTypedAnonVars 1 arg2tys
                 val (f',fv') = Census.freshBoundAnonVar 1
               in
                 (f, g, (typedvars @ xs,
                    Let(e, ([(f',ty)], App(fv', xvs)))), cty2)
               end

             | _ => (f, g, tabs, cty)
             )
          )

        | _ => 
          (f, g, tabs, cty)

    in
      if funkind <> AnyFun then
        case typedvars of
          [(var as (v,si),ty)] => 
          (case (argArity, MILTy.fromProd ty) of
            (true, SOME prodtys) =>
            let
              val (xs,xvs) = Census.freshTypedAnonVars 1 prodtys
                val xs = ListOps.mapi (fn (i,((x,_),ty)) =>  ((x,symbolOfProj(si,i)),ty)) xs
            in
              doResult (xs, LetVal(var, Tuple xvs, ce))
            end

          | _ =>
            doResult (typedvars, ce)
          )

        | _ => 
          doResult (typedvars, ce)
      else default ()
    end

    val recbinds = map transDef recbinds
    val (ce, cty) = transCmp (funs, bodytyenv, kindenv,bodysymenv) ce
  in
    (LetFun(tyvars, funkind, RecFun recbinds, ce), cty)
  end

(*......................................................................*)
(* Non-recursive functions.                                             *)
(*......................................................................*)
| LetFun(tyvars, funkind, Fun (f, (typedvars,e1)), e2) =>
  let
    (* First translate the body of the function *)
    val (e1, cty) = transCmp (funs, addTypedVars(tyenv, typedvars),
      addTyVars(kindenv, tyvars),addTypedSymbols (symenv,typedvars))e1

    (* Decide which iso's to apply *)
    val (funs, { argArity, resArity, uncurry }) = 
      if funkind <> AnyFun 
      then
        let
          val argArity = 
            (case typedvars of
              [(_,ty)] => isSome (MILTy.fromProd ty)
            | _ => false) 
            andalso Controls.enabled argArity

          val (resArity, uncurry) = 
            case MILTy.fromCmp cty of
              (eff,[ty]) => 
              (case MILTy.fromProd ty of
                 SOME [] => 
                 (Controls.enabled resultArity,false) 

               | _ => 
                if Effect.isNone eff andalso isSome (MILTy.fromArrow ty)
                then (false, Controls.enabled uncurry) 
                else (false, false)
              )
            | _ => (false, false)

          val entry = 
            { resArity = resArity, argArity = argArity, uncurry = uncurry }
        in
          (Var.Map.insert(funs, #1 f, entry), entry)
        end
      else (funs, { resArity = false, argArity = false, uncurry = false })

      val tabs' = (typedvars, e1)

      val bodytyenv = 
        Var.Map.insert(tyenv, #1 f, MILTy.forall(tyvars, 
          MILTy.arrow(map #2 typedvars,cty)))

      val bodysymenv = 
          addBoundSymbol(symenv,f)


      fun makeNewDef () =
      let
        fun doResult (tabs as (typedvars, e)) =
          case MILTy.fromCmp cty of
            (eff,[ty]) =>
            (case MILTy.fromProd ty of
              SOME [] => 
              (
                if resArity
                then (typedvars, Let(e, ([(dummyBoundVar,ty)], Triv [])))
                else tabs
              )
              
            | _ => 
              (case (Effect.isNone eff andalso uncurry, 
                 MILTy.fromArrow ty) of
                  (true, SOME (arg2tys, cty2)) =>
                  let
                    val (xs,xvs) = Census.freshTypedAnonVars 1 arg2tys
                    val (f',fv') = Census.freshBoundAnonVar 1
                  in
                    ((typedvars @ xs, Let(e, ([(f', ty)], App(fv', xvs)))))
                  end

                | _ => 
                  tabs
              ))

          | _ => 
            tabs

      in
        if funkind <> AnyFun then
          case typedvars of
            [(var as (v,si),ty)] => 
            (case (argArity, MILTy.fromProd ty) of
              (true, SOME prodtys) =>
              let
                val (xs,xvs) = Census.freshTypedAnonVars 1 prodtys
                val xs = ListOps.mapi (fn (i,((x,_),ty)) =>  ((x,symbolOfProj(si,i)),ty)) xs
              in
                doResult (xs, LetVal(var, Tuple xvs, e1))
              end

            | _ =>
              doResult tabs')

          | _ => 
            doResult tabs'
        else tabs'
      end

    val (e2, cty) = transCmp (funs, bodytyenv, kindenv, bodysymenv) e2
    val tabs = makeNewDef ()
  in
    (LetFun(tyvars, funkind, Fun (f, tabs), e2), cty)
  end

end

val _ = Controls.reset ()
val (e,cty) = 
  PrintManager.process
  ("Arity raising", false)
  (fn () => transCmp (Var.Map.empty, tyenv, Var.Map.empty, Var.Map.empty) e)

in
  Controls.printCounts PrintManager.print; e
end

fun full tyenv e =
    let
      val e = Inline.transform tyenv e
      val e = FunScope.transform tyenv e
(*
      val _ = 
        if Controls.get iterateDump 
        then (PrintManager.print "d..."; MILPretty.dumpCmp e)
        else ()
*)

      val e = transform tyenv e
    in
      if Controls.getTotal () <> 0 then full tyenv e else e
    end

val _ = Opts.add "arity1" (full, "Arity raising functions")
val _ = Opts.add "arity2" (full, "Arity raising functions")

end (* of local open *)
end (* of struct *)

