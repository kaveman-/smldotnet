(*======================================================================*)
(* Compile a value term or non-continuation computation term.           *)
(*======================================================================*)
structure CompileValCmp (* :> COMPILEVALCMP *) =
struct

local 
  open RepNames CompileEnv TyRep MILTerm CompileFixedOps
  open RTInstrs
  structure I = RTInstrs
  infixr 5 ++ $+
  infix 6 +$
in

val [omitCasts, deadVars] = 
  map (Controls.add false) ["codegen.omitCasts","codegen.deadVars"]

fun scopeBoundVars((env:CompileEnv.Env),xs:MILTerm.BoundVar list,instrs) = 
    if Controls.get(debug) then
    let val bindings = 
         List.foldr (fn ((x,longid), bindings) =>
            if List.null longid 
            then bindings 
            else (case Var.Map.find(#tyenv env, x) of
                     SOME (Single (ldloc (n,_)), ty) => (n,TyRep.tyToRep env ty,List.last longid)::bindings
                   | _ => bindings))
            [] xs
    in
        if List.null bindings 
        then instrs
        else beginscope bindings $+ instrs +$ endscope
    end
    else instrs

fun scopeTypedVars((env:CompileEnv.Env),xs:MILTerm.TypedVar list,instrs) = 
    scopeBoundVars(env,map #1 xs, instrs)


val simpleReuseLocals = Controls.add true "codegen.simpleReuseLocals"
val reuseLocals = Controls.add true "codegen.reuseLocals"

fun filterLive isLive tyenv = tyenv (* Var.Map.filteri (fn (x,(ldsfld _,_)) => true | (x,_) => isLive x) tyenv*)

(*----------------------------------------------------------------------*)
(* Extend the type environment with a single variable, assigning it     *)
(* the next available local. Return this local with the new env.        *)
(*----------------------------------------------------------------------*)
fun envPlusNewVar' prefer (env as { known, blocks, stacked, handler, tyenv, kindenv, apps, locals }) (x:MILTerm.BoundVar, ty) =
if Census.isDead (#1 x)
then
(
  Stats.inc "dead arguments";
  (env, $pop)
)
else
case Var.Map.find(tyenv, #1 x) of
  SOME (Single (ldsfld f), ty) =>
  (env, Single (stsfld f))

| _ =>
  let
    val isLive = 
    if Controls.get reuseLocals then 
    (case Liveness.findLive (#1 x) of
      NONE => (fn y => isSome (Var.Map.find(tyenv, y)))
    | SOME live => (fn y => Var.Set.member(live, y)))
    else if Controls.get simpleReuseLocals
         then (fn y => isSome(Var.Map.find(tyenv, y)))
         else (fn y => true)
    val { load, store, ... } = RTLocals.newVar {isLive = isLive, prefer = prefer, var = x, rep = TyRep.tyToRep env ty }
  in  
    ({ known = known, stacked = stacked, blocks = blocks, kindenv = kindenv, apps = apps, handler = handler, 
       locals = case load of Single (ldloc (i,_)) => IntMap.insert(locals, i, #1 x) | _ => locals,
      tyenv = Var.Map.insert(filterLive isLive tyenv, #1 x, (load, ty)) }, store)
  end

fun checkStoresDistinct instrs =
let fun checkStoresDistinct instrs =
    
    case instrs of 
        stloc i::instrs => if List.all (fn (stloc j) => i <> j | _ => true) instrs
                           then checkStoresDistinct instrs
                           else Debug.fail "checkStoresDistinct:1"
      | starg i::instrs => if List.all (fn (starg j) => i <> j | _ => true) instrs
                           then checkStoresDistinct instrs
                           else Debug.fail "checkStoresDistinct:2"
      | _ :: instrs => checkStoresDistinct instrs
      | [] => true
in
    checkStoresDistinct(RTInstrs.toList instrs)
end


val envPlusNewVar = envPlusNewVar' (fn _ => false)

fun envPlusClosureVar (env as { known, stacked, blocks, tyenv, kindenv, apps, handler, locals }) (x:Var.Var, a) =
  { known = known, blocks = blocks, kindenv = kindenv, apps = apps, handler = handler,
    tyenv = Var.Map.insert(tyenv, x, a), stacked = stacked, locals = locals }

fun envPlusTypedVars (env : Env) xs =
let
  fun gather ([], env, instrs) = (checkStoresDistinct instrs;(env, instrs))
    | gather (xty::xs, env, instrs) =
      let val (env,store) = envPlusNewVar env xty
      in
        gather(xs, env, store ++ instrs)
      end
in
  gather (xs, env, Empty)
end

fun envPlusBlockArgs f (env : Env) xs =
case Liveness.getBlockArgsInfo f of
  NONE => envPlusTypedVars env xs
| SOME argsinfo =>
let
  fun gather ([], [], env, instrs) = (checkStoresDistinct instrs;(env, instrs))
    | gather (xty::xs, args::argsinfo, env, instrs) =
      let val (env,store) = envPlusNewVar' (fn x => isSome (List.find(fn y => Var.eq(x,y)) args)) env xty
      in
        gather(xs, argsinfo, env, store ++ instrs)
      end
in
  gather (xs, argsinfo, env, Empty)
end

fun deferEval (Triv [_],[x:BoundVar]) = Liveness.isStackable (#1 x) andalso Controls.get Liveness.stackVals
  | deferEval (e,[x]) = Liveness.isStackable (#1 x) andalso Controls.get Liveness.stackCmps
  | deferEval _ = false

fun getVar (Var f) = f
  | getVar (TApp(v, _)) = getVar v
  | getVar (Unfold v) = getVar v
  | getVar (As(v, _)) = getVar v
  | getVar v =
    MILPretty.failVal v "CompileCmp:compile: expected variable"

fun compCmpBinding (env : Env) (e, xs) =
(* Variables are never used *)
if List.all (Census.isDead o #1 o #1) xs
then 
  let 
    val (cty,reps,instrs,maxstack) = compile Empty env e
    val (eff,tys) = MILTy.fromCmp cty
  in
    (* Don't generate any code for dead bindings of pure computations *)
    if Effect.isNone eff andalso Controls.get deadVars
    then (env, Empty, 0)

    (* Otherwise just discard the result(s) *)
    else (env, instrs ++ fromList (map (fn _ => pop) xs), maxstack)
  end
else
  if deferEval (e, map #1 xs)
  then (envPlusStacked env (#1 (#1 (hd xs)), e), Empty, 0)
  else
  let
    val (cty, reps, instrs, maxstack) = compile Empty env e
    val (eff,tys) = MILTy.fromCmp cty
    val (env,bindinstrs) = envPlusTypedVars env xs
  in
    (env, instrs ++ bindinstrs, maxstack)
  end  

and compValBinding (env : Env) (v, x) =
(* Variables are never used *)
if Census.isDead (#1 x)
then (env, Empty, 0)
else if deferEval (Triv [v], [x])
  then (envPlusStacked env (#1 x, Triv [v]), Empty, 0)
  else
  let
    val (ty, rep, instrs, maxstack) = compileVal env v
    val (env,bindinstrs) = envPlusTypedVars env [(x,ty)]
  in
    (env, instrs ++ bindinstrs, maxstack)
  end  

(*----------------------------------------------------------------------*)
(* Compile a value term to produce:                                     *)
(*   its type                                                           *)
(*   its target representation                                          *)
(*   a list of instructions that puts the value on the top of the stack *)
(*   the number of stack items required to produce the value            *)
(*----------------------------------------------------------------------*)
and compileVal (env : Env) (v : Val)  =
case v of

(*......................................................................*)
(* Constants compile directly to ldc or ldstr                           *)
(*......................................................................*)
  SCon (ty, scon) => 
  let
    val rep = tyToRep env ty
    val instrs = 
        case (scon,rep) of
           (Constants.NULL, VMTy.ValueClass _) => #load (RTLocals.newTmp rep)
         | (Constants.NULL, _) => $ldnull
         | _ => $ (ldc scon)
  in
    (ty, rep, instrs, I.units rep)
  end

(*......................................................................*)
(* A local variable                                                     *)
(*......................................................................*)
| Var x =>
  ( 
    case Var.Map.find(#stacked env, x) of
      SOME e =>
      let val (cty, reps, instrs, maxstack) = compile Empty env e
          val (_, [ty]) = MILTy.fromCmp cty
      in (ty, hd reps, instrs, maxstack) end

    | NONE =>
      case Var.Map.find(#tyenv env, x) of
        SOME (instrs, ty) =>
        let val rep = tyToRep env ty
        in
          (case instrs of Single (ldloc (i,_)) => (case IntMap.find(#locals env, i) of SOME x' => 
                                            if Var.eq(x,x') then () else MILPretty.errorVal v ("CompileValCmp.compileVal: local has been reassigned to variable " ^ Var.toString x')
                                    | _ => ()) | _ => ());
          (ty, rep, instrs, I.units rep)
        end

      | NONE => 
        MILPretty.failVal v "CompileVal.compileVal: variable not in environment" 
  )

(*......................................................................*)
(* Injection into a sum type                                            *)
(*......................................................................*)
| Inj(ty, i, vs, si) =>
  let val (tys, reps, instrs, maxstack) = compileVals env vs
  in
    case MILTy.fromSum ty of

  (*..................................................................*)
  (* Option types                                                     *)
  (*..................................................................*)
      SOME ([[], [ty']]) =>
      (case vs of
        [] =>
        let val (instrs, stack) = CompileOnePlus.none env ty'
        in
          (ty, tyToRep env ty, instrs, stack)
        end

      | [_] =>
        let val (instrs, stack) = CompileOnePlus.some env (instrs,maxstack) ty'
        in
          (ty, tyToRep env ty, instrs, stack)
        end

      | _ =>
        MILPretty.failVal v "CompileVal.compileVal: multiple args to 1+ type"
      )
    | SOME ([[ty'], []]) =>
      (case vs of
        [] =>
        let val (instrs, stack) = CompileOnePlus.none env ty'
        in
          (ty, tyToRep env ty, instrs, stack)
        end

      | [_] =>
        let val (instrs, stack) = CompileOnePlus.some env (instrs,maxstack) ty'
        in
          (ty, tyToRep env ty, instrs, stack)
        end

      | _ =>
        MILPretty.failVal v "CompileVal.compileVal: multiple args to 1+ type"
      )
    | SOME tyss =>
    let val ldtag = SumRep.compileTagFromIndex{con=si,index=i}
    in
  (*..................................................................*)
  (* Enumeration type                                                 *)
  (*..................................................................*)
      if List.all List.null tyss
      then (ty, tyToRep env ty, $ ldtag, 1)
      else
  (*..................................................................*)
  (* General sum type                                                 *)
  (*..................................................................*)
      let
        val rep = tyToRep env (MILTy.con [])
        val conty = MILTy.con tys
        val conrep = tyToRep env conty
        val argsstack = ListOps.sum (map I.units reps)
        val maxstack = Int.max (maxstack, argsstack+1)
        val (instrs, maxstack) =
          StaticNewObj.init (instrs +$ ldtag, maxstack)
          { classty = conrep, argtys = reps @ [SumRep.tagRep()] }
      in
        (ty, rep, instrs, maxstack)
      end
    end
  end

(*......................................................................*)
(* Exception constructors:                                              *)
(*......................................................................*)
| ExCon(ty, vs) =>
  let
    val (argtys, reps, instrs, maxstack) = compileVals env vs
    val exnrep = tyToRep env ty
    val (instrs, maxstack) = 
        case MILTy.fromExn ty of
            SOME (ex,tys) => 
            if TyName.isExternal (Exn.name ex) 
            then init (instrs, maxstack) { classty = exnrep, argtys = reps }
            else StaticNewObj.init (instrs, maxstack) { classty = exnrep, argtys = reps }
        | _ => Debug.fail "CompileValCmp.sml:ExCon"
  in
    (
      MILTys.topExn,
      tyToRep env MILTys.topExn,
      instrs,
      maxstack
    )
  end


(*......................................................................*)
(* The unit value                                                       *)
(*......................................................................*)
| Tuple [] =>
  (
    MILTy.prod [],
    tyToRep env (MILTy.prod []),
    $ ldnull,
    1
  )

(*......................................................................*)
(* Tuples                                                               *)
(*......................................................................*)
| Tuple vs =>
  let
    val (tys, reps, instrs, maxstack) = compileVals env vs
    val prodty = MILTy.prod tys
    val prodrep = tyToRep env prodty
    val reps = 
      if Controls.get TyRep.genGenerics
      then ListOps.mapi (fn (i, _) => VMTy.Var i) reps
      else reps
    val (instrs, maxstack) = StaticNewObj.init (instrs, maxstack) {classty = prodrep, argtys = reps}
  in
    (
      prodty,
      prodrep,
      instrs,
      maxstack
    )
  end
     
(*......................................................................*)
(* Projection from a tuple/constructor/exn constructor                  *)
(* Special case for single string argument exception constructors.      *)
(*......................................................................*)
| Proj (i, n, v') =>
  let
    val (tuplety, tuplerep, instrs, maxstack) = compileVal env v'
    val tys = case MILTy.fromProdCon tuplety of
      SOME tys => tys
    | NONE => 
      MILPretty.failVal v
      "CompileVal.compileVal: expected product/constructor type"

    val fldty = (List.nth (tys, i) handle Subscript =>
      MILPretty.failVal v "CompileVal.compileVal: subscript out of range")

    val isStringExn =
      isSome (MILTy.fromExn tuplety) andalso length tys = 1
      andalso MILTy.eq (hd tys, MILTys.string)
    val fldrep = 
      if Controls.get TyRep.genGenerics andalso (isSome (MILTy.fromProd tuplety))
      then VMTy.Var i
      else tyToRep env fldty
  in 
    if isSome (MILTy.fromSum tuplety)
    then 
        let val (instrs,maxstack) = 
            SumRep.compileIndexFromTag(instrs +$ ldfld{name = RepNames.sumTag,
                                                     classty = tuplerep, 
                                                     fldty = SumRep.tagRep()},
                                       maxstack)
        in
        (fldty,
         fldrep,
         instrs,
         maxstack)
        end
    else         
    (
      fldty,
      fldrep,
      instrs +$
        (if isStringExn
         then callvirt{name = Id.fromString RuntimeNames.exnMessage, 
                       classty = tyToRep env MILTys.topExn,
                       resty = SOME (tyToRep env MILTys.string), 
                       argtys = []}
         else ldfld{name = argLabel i, 
                   classty = tuplerep, 
                   fldty = fldrep}
        ),
      maxstack
    )
  end

(*......................................................................*)
(* Mu-introduction                                                      *)
(*......................................................................*)
| Fold (v, ty') => 
  let
    val (ty, rep, instrs, maxstack) = compileVal env v
    val SOME a = MILTy.fromMu ty'
  in
    if Controls.get TyRep.genGenerics andalso List.exists (fn (_,ty) => IntSet.isEmpty (MILTy.dtyvars ty)) (#2 a)
    then
    let
      val (instrs, maxstack) = CompileFixedOps.init (instrs, maxstack)
          { classty = tyToRep env ty', argtys = [tyToRep env ty] } 
    in
      (ty', rep, instrs, maxstack)
    end
    else (ty', rep, instrs, maxstack)
  end

(*......................................................................*)
(* Mu elimination                                                       *)
(*......................................................................*)
| Unfold v =>
  let
    val (ty, rep, instrs, maxstack) = compileVal env v
    val a = case MILTy.fromMu ty of
      SOME a => a
    | NONE => 
      MILPretty.failVal v ("CompileVal.compileVal: expected recursive type, found" ^ MILTy.toString ty)
    val closed = List.all (fn (_,ty) => IntSet.isEmpty (MILTy.dtyvars ty)) (#2 a)
    val resty = MILTy.unfold a
  in
    if not closed andalso Controls.get TyRep.genGenerics
    then
    (
      resty,
      rep,
      instrs +$ (ldfld{name = argLabel 0, classty = tyToRep env ty, 
        fldty = tyToRep env resty}),
      maxstack
    )
    else
    (
      resty,
      rep,
      instrs,
      maxstack
    )
  end

(*......................................................................*)
(* Type application                                                     *)
(*......................................................................*)
| TApp(v, tys) =>
  let
    val (polyty, rep, instrs, maxstack) = compileVal env v
    val a as (tyvars, ty) = case MILTy.fromForall polyty of 
      SOME a => a
    | NONE => MILPretty.failVal v "CompileVal:compileVal: expected poly type"
  in
    (
      MILTy.app (MILTy.abs a, tys),
      rep, 
      instrs,
      maxstack
    )
  end

(*......................................................................*)
(* Type abstraction                                                     *)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let val (ty, rep, instrs, maxstack) = compileVal (envPlusTyVars env tyvars) v
  in
    (
      MILTy.forall (tyvars, ty),
      rep,
      instrs,
      maxstack
    )
  end


(*......................................................................*)
(* Nop-op coercions.                                                    *)
(* @review: actually not all of these are no-ops now                    *)
(*......................................................................*)
| As(v, ty') => 
  let
    val (ty, rep, instrs, maxstack) = compileVal env v
    val rep' = tyToRep env ty'
    fun default () = (ty', rep', instrs, maxstack)
  in       
      case (rep,rep') of
          (VMTy.ValueClass _ ,VMTy.Class _) =>  (* box - Beta2 *)
              (ty', rep', instrs +$ (box rep), maxstack)

        | (VMTy.Address rep,VMTy.Class _) =>  (* box - Beta2 *)
              (ty', rep', instrs +$ ldobj rep +$ box rep, maxstack)

        | (VMTy.ValueClass _, VMTy.ValueClass _) => 
              (*@HACK: for taking the address of the interior a ref cell, 
               used only for method invocation, field read/update of a value 
               stored in value type ML reference *)
              let (*TODO: review use of forceBounds *)
                  val ty = MILTy.forceBounds (#kindenv env) ty
              in
              (case MILTy.fromRefty ty of 
                   SOME ([cellty],refkind) => 
                       (case MILTys.fromRefKind refkind of
                            SOME MILTys.Heap =>
                                (ty', rep', instrs +$ (ldflda { name = argLabel 0, classty = rep, fldty = rep'}), maxstack)

                          | SOME MILTys.Address => (* its already a byref *)
                                (ty', rep', instrs, maxstack)

                          | SOME (MILTys.Field(classtn,field)) => 
                                (let val classrep = tyToRep env (MILTy.tyname classtn)
                                 in
                                     (ty', rep',
                                      instrs +$ ldflda{name=field, classty = classrep, fldty = rep' }, 
                                      maxstack)
                                 end)
                          | SOME (MILTys.Static(classtn,field)) => 
                                 (let val classrep = tyToRep env (MILTy.tyname classtn)
                                  in
                                     (* naive:
                                      (ty',
                                       instrs@[I.ldsflda { name = field, classty = classrep, fldty = rep'}], 
                                       1+stack) *)
                                      (ty', rep',
                                       $ (ldsflda { name = field, classty = classrep, fldty = rep'}), 
                                       1) 
                                  end)
                          | NONE => Debug.fail "CompileVal:NopCast:1")
                 | NONE => default ())
              end
        | (VMTy.Class _, VMTy.ValueClass _) => 
              (*@HACK: for taking the address of the interior a ref cell, 
               used only for method invocation, field read/update of a value 
               stored in value type ML reference *)
              let (*TODO: review use of forceBounds *)
                  val ty = MILTy.forceBounds (#kindenv env) ty
              in
              (case MILTy.fromRefty ty of 
                   SOME ([cellty],refkind) => 
                       (case MILTys.fromRefKind refkind of
                            SOME MILTys.Heap =>
                                (ty', rep', instrs +$ ldflda { name = argLabel 0, classty = rep, fldty = rep'}, maxstack)

                          | SOME MILTys.Address => (* its already a byref *)
                                (ty', rep', instrs, maxstack)

                          | SOME (MILTys.Field(classtn,field)) => 
                                (let val classrep = tyToRep env (MILTy.tyname classtn)
                                 in
                                     (ty', rep',
                                      instrs +$ ldflda{name=field, classty = classrep, fldty = rep' }, 
                                      maxstack)
                                 end)
                          | SOME (MILTys.Static(classtn,field)) => 
                                 (let val classrep = tyToRep env (MILTy.tyname classtn)
                                  in
                                     (* naive:
                                      (ty',
                                       instrs +$ ldsflda { name = field, classty = classrep, fldty = rep'}, 
                                       1+stack) *)
                                      (ty', rep',
                                       $ (ldsflda { name = field, classty = classrep, fldty = rep'}), 
                                       1) 
                                  end)
                          | NONE => Debug.fail "CompileVal:NopCast:1")
                 | NONE => default ())
              end

        | _  => (ty', rep', instrs, maxstack)
  end

(*----------------------------------------------------------------------*)
(* Compile a list of value terms to produce:                            *)
(*   their types                                                        *)
(*   their target representations                                       *)
(*   a list of instructions that put the values on the stack, with the  *)
(*     last item on the top.                                            *)
(*   the number of stack items that the values actually occupy          *)
(*----------------------------------------------------------------------*)
and compileVals env (vs : Val list) =
case vs of
  [] => 
  (
    [],
    [],
    Empty,
    0
  )

| v::vs =>
  let val (ty, rep, instrs, maxstack) = compileVal env v
      val (tys, reps, instrs', maxstack') = compileVals env vs
  in
  (
    ty::tys,
    rep::reps,
    instrs ++ instrs',
    Int.max(maxstack, I.units rep + maxstack')
  )
  end


(*----------------------------------------------------------------------*)
(* Compile a non-continuation computation term.                         *)
(* Result:                                                              *)
(*   computation type of the term                                       *)
(*   list of instructions to evaluate the term with results on stack    *)
(*   bound on stack used by instructions                                *)
(*----------------------------------------------------------------------*)
and compile (precall:Instrs) (env : Env) (e : MILTerm.Cmp) =
case e of 
(*......................................................................*)
(* Moggi-val                                                            *)
(*......................................................................*)
  Triv vs =>
  let val (tys, reps, instrs, maxstack) = compileVals env vs
  in
  (
    MILTy.noeffect tys,
    reps,
    instrs,
    maxstack
  )
  end

(*......................................................................*)
(* Function application                                                 *)
(*......................................................................*)
| App(Proj(i,_,v), vs) =>
  let
    val f = getVar v
    val a = appMethod i
    val (argtys, argreps, arginstrs, argmaxstack) = compileVals env vs
    val (funinstrs, funmaxstack, cty, resultreps, callinstr) = 
    case v of
      As(v,funty) => 
      let val (closty,closrep,instrs,maxstack) = compileVal env v
          val SOME (_,cty) = MILTy.fromArrow funty
          val (_,resulttys) = MILTy.fromCmp cty
          val resultreps = map (tyToRep env) resulttys
          val resrep = OptionOps.hd resultreps
      in (instrs,maxstack,cty,resultreps,
         if isSome (MILTy.fromClosure closty) 
         then (Stats.inc "direct closure calls";
               callinst { name = if Controls.get CompileOps.useOverrides 
                                then Id.fromString (RTSymbols.getSym f) else a,
                         classty = closrep, argtys = argreps, resty = resrep })
         else callvirt { name = a, classty = closrep, argtys = argreps, resty = resrep })
      end
    | _ => 
      let val (funty,funrep,instrs,maxstack) = compileVal env v
          val SOME (_,cty) = MILTy.fromArrow funty
          val (_,resulttys) = MILTy.fromCmp cty
          val resultreps = map (tyToRep env) resulttys
          val resrep = OptionOps.hd resultreps
      in (instrs,maxstack,cty,resultreps,
          callvirt { name = a, classty = funrep, argtys = argreps, resty = resrep }) 
      end

    val resstack = ListOps.sum (map units resultreps)
  in
  (
    cty, resultreps,
    funinstrs ++ arginstrs ++ precall +$ callinstr,
    Int.max(resstack, Int.max(funmaxstack, argmaxstack + 1))
  )
  end

(*......................................................................*)
(* Throw an exception                                                   *)
(* We only really need this for try x <= (throw e) ...                  *)
(*......................................................................*)
| Throw(v, cty, message) =>
  let
    val (eff,tys) = MILTy.fromCmp cty
    val resultreps = map (tyToRep env) tys
    val (exnty, _, instrs, stack) = compileVal env v
  in
  (
    cty,
    resultreps,
    (if Controls.get CompileException.exnLocs andalso message<>"" 
    then fromList [ldc (Constants.STRING (UString.fromString message)), stsfld { name = RepNames.exnLocMessage, classty = globs (),
                fldty = RTOps.string }] else Empty) ++ instrs +$ throw,
    stack
  )
  end

| App(v, vs) =>
  let
    val (argtys, argreps, arginstrs, argmaxstack) = compileVals env vs
    fun compApp () =
    let
      val f = getVar v

      val (funty, funrep, funinstrs, funmaxstack) = compileVal env v
      val (_,cty) = case MILTy.fromArrow funty of SOME a => a
        | NONE => MILPretty.failCmp e "CompileCmp: expected function in application term"
      val (_, resulttys) = MILTy.fromCmp cty
      val resultreps = map (tyToRep env) resulttys
      val resrep = OptionOps.hd resultreps
      val resstack = ListOps.sum (map units resultreps)
      val a = 
        case Var.Map.find(#apps env, f) of
          SOME i => appMethod i
        | NONE => 
          (Debug.print 
            ("\nWarning: no app method listed for " ^ Var.toString f); Id.fromString "$")
    in
    (
      cty, resultreps,
      funinstrs ++ arginstrs ++ precall +$ callvirt { name = a, classty = funrep, argtys = argreps, 
           resty = resrep },
      Int.max(resstack, Int.max(funmaxstack, argmaxstack + units funrep))
    )
    end

    fun compGlobalApp (f, funty) =
    let
      val (_,cty) = case MILTy.fromArrow funty of SOME a => a
        | NONE => MILPretty.failCmp e "CompileCmp: expected function in application term"
      val (_,resulttys) = MILTy.fromCmp cty
      val resultreps = map (tyToRep env) resulttys
      val resrep = OptionOps.hd resultreps
      val resstack = ListOps.sum (map units resultreps)
    in
      (
        cty, resultreps,
        arginstrs ++ precall +$ call { name = Id.fromString (RTSymbols.getSym f),
          classty = globs (), argtys = argreps, resty = resrep},
        Int.max(argmaxstack, resstack)
      ) 
    end

  in
    case v of
      Var x =>
      (case Var.Map.find(#known env, x) of
        SOME funty =>
        compGlobalApp (x, funty)

      | _ => compApp ())

    | TApp(Var x, tys) =>
      (case Var.Map.find(#known env, x) of
        SOME polyty =>
        compGlobalApp (x, MILTy.app (MILTy.abs 
          (valOf(MILTy.fromForall polyty)), tys))

      | _ => compApp ())

    | _ => compApp ()
  end

(*......................................................................*)
(* Language extensions and primitives                                   *)
(*......................................................................*)
| Special(info as (Ext.NewDelegate,_,_), [Proj(i, _,v)], cty) =>
  let
    val (effect, restys) = MILTy.fromCmp cty
    val resreps = map (externalTyToRep env) restys
    val (ty, rep, instrs, maxstack) = compileVal env v
    val SOME (argtys,cty') = MILTy.fromArrow ty
    val argreps = map (tyToRep env) argtys
    val funrep = tyToRep env ty
    val (_, resulttys) = MILTy.fromCmp cty'
    val resultreps = map (tyToRep env) resulttys
    val meth = { name = appMethod i, classty = funrep, argtys = argreps, 
                 resty = OptionOps.hd resultreps }

    val (instrs, maxstack) =
        CompileFixedOps.init (instrs +$ dup rep +$ ldvirtftn meth, Int.max(maxstack,2))
        { classty = hd resreps, argtys = [RTOps.object, RTOps.nativeInt] }
  in
  ( 
    cty,
    resreps,
    instrs,
    maxstack
  )
  end      

| Special(info as (optype, optty, optname), vs, cty) =>
  let
    val (effect, restys) = MILTy.fromCmp cty
    val (tys, reps, instrs, maxstack) = compileVals env vs
    val resreps = map (externalTyToRep env) restys
    val optrep = Option.map (tyToRep env) optty
  in
    case optype of
      Ext.New =>
      let
        val (instrs, maxstack) = 
            (* we emit a "static constructor call" for closures *)
            case restys of 
                [ty] => if isSome (MILTy.fromClosure ty)
                        then StaticNewObj.init (instrs, maxstack){ classty = hd resreps, argtys = reps }
                        else CompileFixedOps.init (instrs, maxstack){ classty = hd resreps, argtys = reps }
              | _ =>  CompileFixedOps.init (instrs, maxstack){ classty = hd resreps, argtys = reps }
      in
      ( 
        cty, resreps,
        instrs,
        maxstack
      )
      end

    | Ext.Address i =>
      let
          val refty = hd tys (* the reference type *)
          (*TODO: review use of forceBounds *)
          val refty = MILTy.forceBounds (#kindenv env) refty
          val v = hd vs (* the reference value *)
          val SOME (tys,refkind) = MILTy.fromRefty refty
          val rep = if null tys then RTOps.object else tyToRep env (List.nth(tys,i)) 
          val refrep = tyToRep env refty
          val cty =  MILTy.cmp(Effect.none,[MILTy.refty (tys,MILTys.refKind MILTys.Address)])
      in 
        case MILTys.fromRefKind refkind of
                       SOME MILTys.Heap =>
                           (cty, resreps,
                            instrs +$ ldflda { name = argLabel i, classty = refrep, fldty = rep },
                            maxstack)
                     | SOME MILTys.Address => (* it's already an address *)
                           (cty, resreps, instrs, maxstack)
                     | SOME (MILTys.Field(classtn,field)) =>
                           (let val classrep = tyToRep env (MILTy.tyname classtn)
                            in
                                (cty,resreps,instrs +$ ldflda { name = field, classty = classrep, fldty = rep},maxstack)
                            end)
                     | SOME (MILTys.Static(classtn,field)) =>
                           (let val classrep = tyToRep env (MILTy.tyname classtn)
                            in
                                (* naive:(cty,instrs@[pop,ldsflda { name = field, classty = classrep, fldty = rep}],stack) *)
                                (cty,resreps,$(ldsflda { name = field, classty = classrep, fldty = rep}),1)
                            end)
                     | NONE => 
                       (Debug.print ("\nWarning: CompileValCmp:Address encountered unknown reference type "^MILTy.toString refty);
                        (* This must be dead code, so simply pop the reference and load the address of a temp to satisfy the verifier *)
                        let val {loada,...} = RTLocals.newTmp rep
                        in
                            (cty,resreps,instrs ++ ($pop) ++ loada,Int.max(maxstack,units (VMTy.Address rep)))
                        end)

      end

    | _ =>
      let
        val (instrs', maxstack') = 
          CompileSpecial.compile 
          ((optype, optrep, optname), reps, tys, OptionOps.hd resreps, maxstack)
      in
      (  
        cty,
        resreps,
        instrs ++ instrs',
        maxstack'
      )
      end
  end

| _ =>
  MILPretty.failCmp e "CompileCmp.compile: continuation-only term"

end (* of local open *)  
end (* of struct *)
