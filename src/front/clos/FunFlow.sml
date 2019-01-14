(*======================================================================*)
(* Perform a flow analysis for closure functions.			*)
(*======================================================================*)
structure FunFlow :> FUNFLOW =
struct

(*----------------------------------------------------------------------*)
(* A partition of closure function variables by the equivalence `might  *)
(* flow to the same application as'.                                    *)
(*----------------------------------------------------------------------*)
structure Partition = Partition(Var.Map)

local 
  open FreeVarsEnv MILTerm 
in

val showClosFlow = Controls.add false "clos.showFlow"
val allClosures = Controls.add false "clos.allClosures"

type DefInfo = 
{
  recf : Var.Var,
  args : MILTerm.TypedVar list,
  cty  : MILTy.CmpType,
  tyvars : (Var.Var * MILTy.Kind) list
} Var.Map.map

fun normalizeKind KnownFun = if Controls.get allClosures then AnyFun else KnownFun
  | normalizeKind k = k

(*----------------------------------------------------------------------*)
(* Gather information about a term ce.                                  *)
(*----------------------------------------------------------------------*)
fun gather ce =
let

(* Add a function def to the mapping *)
fun addDef (defs : DefInfo) (env : Env, f, g, tyvars, xs, cty) = 
  Var.Map.insert(defs, f, 
  { recf = g, cty = cty, args = xs, tyvars = tyvars @ Var.Map.listItemsi (#kindenv env) })

(*----------------------------------------------------------------------*)
(* Collect definitions: just accumulate a parameter "defs"		*)
(* Also need to determine the type of the result, but we don't care     *)
(* about effects as these don't affect partitioning.			*)
(*----------------------------------------------------------------------*)
fun collect (env : Env) e (defs : DefInfo) =
let
  fun collectCases tysFor (v, cases, optdefault, cty) =
  let
    fun gather [(i, (xs, e))] defs = 
        let
          val env = envPlusTypedVars env (ListPair.zip(xs,tysFor i))          
        in
          collect env e defs
        end
      | gather ((i, (xs, e))::rest) defs =
        let
          val env = envPlusTypedVars env (ListPair.zip(xs,tysFor i))
          val (defs, cty) = collect env e defs
          val (defs, _) = gather rest defs
        in
          (defs, cty)
        end
    val (defs, cty) = gather cases defs
  in
    case optdefault of 
      NONE =>
      (defs, cty)
    | SOME e =>
      collect env e defs
  end

  fun typeVal v = MILTermOps.typeOfVal (Var.Map.map #2 (#tyenv env)) v
in
  case e of

  App(v, vs) =>
  let val SOME (_, cty) = MILTy.fromArrow (typeVal v)
  in
    (defs, cty)
  end

| Special(_, _, cty) =>
  (defs, cty)

| Encap e =>
  collect env e defs

| Let(e1, (xs, e2)) =>
  collect (envPlusTypedVars env xs) e2 defs

| LetVal(x, v, e) =>
  collect (envPlusTypedVars env [(x,typeVal v)]) e defs

| Triv vs =>
  (defs, MILTy.noeffect (map typeVal vs))

| Case (v, cases, default, cty) =>
  let val SOME tyss = MILTy.fromSum (typeVal v)
  in
    collectCases (fn i => List.nth(tyss, i)) (v, cases, default, cty)
  end

| CaseSCon a =>
  collectCases (fn _ => []) a
      
| TypeCase a =>
  collectCases (fn ty => [ty]) a

| Throw(v, cty, loc) =>
  (defs, cty)

| TryLet(e, handlers, (xs, body)) =>
  let
    val defs = foldr (fn ((xs,e),defs) => #1 (collect (envPlusTypedVars env xs) e defs))
      defs handlers
  in
    collect (envPlusTypedVars env xs) body defs
  end

| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun collectMethods [] defs = defs
      | collectMethods ((name, atts, mods, tys, tyopt, optabs)::methods) defs =
        case optabs of 
          NONE => 
          collectMethods methods defs

        | SOME ((f,_), (xs, e)) =>
          let
            val argtys = 
              if Symbol.Set.member(mods, Id.staticSym)
              then tys
              else classname::tys
            val env = envPlusBoundVars env (xs,argtys)
            val (defs,cty) = collect env e defs
          in
            collectMethods methods defs
          end
    val defs = collectMethods methods defs
  in
    collect env e defs
  end

| LetFun(tyvars, kind, def, e) =>
  let
    val defnenv = envPlusTyVars env tyvars
    val kind = normalizeKind kind
    val isany = case kind of AnyFun => true | _ => false

    val (defsvars, bodyvars) = 
      case def of
        Fun (f, _) => ([], [f])
      | RecFun recbinds => (map #2 recbinds, map #1 recbinds)

    fun collectDef (Fun (f, (xs, e))) =
        let
          (* Gather information about the body of the function *)
          val (defs, cty) = collect (envPlusTypedVars defnenv xs) e defs

          (* The type of the function as seen externally *)
          val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))

          val bodyenv = envPlusTypedVars env [(f, ty)]
          val bodyenv = envPlusFun bodyenv (kind, [f])
          val defs = if isany then addDef defs (env, #1 f, Var.dummy, tyvars, xs, cty)
                     else defs
        in
          (defs, bodyenv)
        end

      | collectDef (RecFun recbinds) =
        let
          val defnenv = 
            foldl
            (fn ((_, g, (xs, _), cty), defnenv) =>             
            envPlusTypedVars defnenv [(g, MILTy.arrow(map #2 xs, cty))]) defnenv recbinds
          val defnenv = envPlusFun defnenv (kind, defsvars)

          val defs = foldr (fn ((_, g, (xs, e), cty),defs) => 
            #1 (collect (envPlusTypedVars defnenv xs) e defs)) 
            defs recbinds
      
          val bodyenv = 
            foldl
            (fn ((f, g, (xs, _), cty), env) =>
              let
                val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))
              in
                envPlusTypedVars (envPlusFun env (kind, [f])) [(f, ty)]
              end) env recbinds
          val defs = if isany then foldr (fn ((f, g, (xs, _), cty), defs) =>
              addDef defs (env, #1 f, #1 g, tyvars, xs, cty)) defs recbinds
            else defs
        in
          (defs, bodyenv)
        end

    val (defs, bodyenv) = collectDef def
  in
    collect bodyenv e defs
  end
end 

(* Do the work! *)
val (definfo,_) = 
  PrintManager.process ("Collecting function definitions", false)
      (fn () => collect emptyEnv ce Var.Map.empty)

(* Create a single partition containing all function defs *)
val partition = ref (Partition.new_partition (map #1 (Var.Map.listItemsi definfo)))

(* Function applications where the function is known *)
val knownapps = ref Var.Set.empty

(* Function applications where the function has been passed in *)
(* We store a representative function def from the partition *)
val unknownapps = ref (Var.Map.empty : Var.Var Var.Map.map)

(* Function applications to which no possible function can flow *)
(* Record the result type to help closure conversion *)
val deadapps = ref (Var.Map.empty : MILTy.CmpType Var.Map.map)

(* Add a function application *)
fun addApp (env : FreeVarsEnv.Env,f,ty) =
let
  val parts = Partition.list_parts (!partition)
  val ty = 
    case MILTy.fromForall ty of
      SOME (a as (kinds, _)) => 
      let val ty = MILTy.app (MILTy.abs a, map (fn MILTy.Bound ty => ty) kinds)
      in
        MILTy.forceBounds (#kindenv env) ty
      end

    | NONE => 
      MILTy.forceBounds (#kindenv env) ty

  val ty = 
    case MILTy.fromMu ty of
      NONE => ty
    | SOME a => MILTy.unfold a

  val SOME (argtys,cty) = MILTy.fromArrow ty

  val possparts =
    List.filter (fn part => 
    let 
      val rep::_ = Partition.list_part (!partition, part)
      val SOME { tyvars, args, cty = cty', ... } = Var.Map.find(definfo, rep)
      val argtys' = map #2 args
      val ty' = MILTy.arrow(argtys', cty')
      val S = foldl (fn ((x,MILTy.Bound ty),S) => Var.Map.insert(S,x,ty)
                      | (_,S) => S)
        Var.Map.empty tyvars
      val ty' = MILTy.subst S ty'
      val SOME (argtys', cty') = MILTy.fromArrow ty'
    in
      List.length argtys = List.length argtys' 
      andalso (* avoid useless calls to sameRep *)	 
      Eq.list (MILTyRep.sameRep (#kindenv env)) (argtys,argtys')
      andalso 
      Eq.list (MILTyRep.sameRep (#kindenv env)) 
        (#2 (MILTy.fromCmp cty), #2 (MILTy.fromCmp cty'))
      (*
      MILTy.leq (#kindenv env) (ty,ty') *)
    end) parts   
in
  case possparts of
    [] =>
    deadapps := Var.Map.insert(!deadapps, f, cty)

  | firstpart::_ =>
    (unknownapps := Var.Map.insert(!unknownapps, f, hd (Partition.list_part (!partition, firstpart)));
     partition := Partition.union_list(!partition, possparts))
end

(*----------------------------------------------------------------------*)
(* Collect function application info for a value term			*)
(*----------------------------------------------------------------------*)
datatype ValKind = AppPos | DelPos | OtherPos
fun typeVal env v =
case v of
  Var x => #2 (lookup(env, x))
| As(_, ty) => ty
| SCon (ty, _) => ty
| Fold(_, ty) => ty
| ExCon(excon, vs) => MILTys.topExn
| Inj(ty, _, vs, _) => ty
| Tuple vs => MILTy.prod (map (typeVal env) vs)
| TAbs(tyvars, v) => MILTy.forall (tyvars, typeVal (envPlusTyVars env tyvars) v)
| Unfold v =>
  let val SOME a = MILTy.fromMu (typeVal env v)
  in MILTy.unfold a
  end
| Proj(i, n, v) => 
  let val SOME tys = MILTy.fromProdCon (typeVal env v)
  in
    List.nth(tys,i)
  end

| TApp(v, tys) =>
  let val SOME a = MILTy.fromForall (typeVal env v)
  in
    MILTy.app(MILTy.abs a, tys)
  end

fun getVar v = 
case v of
  Var x => SOME x
| As(v,_) => getVar v
| Unfold v => getVar v
| Fold (v,_) => getVar v
| TAbs(_,v) => getVar v
| TApp(v,_) => getVar v
| Proj(_, _, v) => getVar v
| _ => NONE

fun flowVal' pos env v =
let val ty = typeVal env v
in
  case getVar v of
    NONE => ty
  | SOME f =>
    case pos of
      OtherPos => ty
    | DelPos => (addApp (env, f, ty); ty)
    | AppPos =>
      let val info = lookupFunKind(env, f)
      in
        case info of
          SOME AnyFun => (knownapps := Var.Set.add(!knownapps, f); ty)
        | NONE => (addApp (env, f, ty); ty)
        | _ =>ty
      end
end

and flowVal env v = flowVal' OtherPos env v
and flowVals env vs = map (flowVal env) vs

(*----------------------------------------------------------------------*)
(* Collect function application info for a computation term		*)
(*----------------------------------------------------------------------*)
and flowCmp env ce =
let
  fun flowCases tysFor (v, cases, optdefault, cty) =
  let
    val defresult =  Option.map (flowCmp env) optdefault
    fun gather [] = 
        []

      | gather ((i, (xs, e))::rest) =
        let
          val env = envPlusTypedVars env (ListPair.zip(xs,tysFor i))
          val cty = flowCmp env e
          val ctys = gather rest
        in
          cty::ctys
        end
    val ctys = gather cases
    val cty::ctys = 
      case defresult of NONE => ctys
                      | SOME cty => cty::ctys
  in
    cty
  end

in
  case ce of

  App(v, vs) =>
  let val SOME (_, cty) = MILTy.fromArrow (flowVal' AppPos env v)
  in
    flowVals env vs; cty
  end

  (* Treat delegate creation as an unknown application *)
| Special((Ext.NewDelegate,_,_),[v],cty) =>
  (flowVal' DelPos env v; cty)

| Special(j, vs, cty) => (flowVals env vs; cty)
| Encap e => flowCmp env e
| Triv vs => MILTy.noeffect (flowVals env vs)

| Let(e1, (xs, e2)) =>
  let
    val e1ty = flowCmp env e1
    val (_,tys) = MILTy.fromCmp e1ty    
    val e2ty = flowCmp (envPlusTypedVars env xs) e2
  in  
    MILTy.unionCmpTypes(e1ty,e2ty)
  end

| LetVal(x, v, e) =>
  let val ty = flowVal env v
  in
    flowCmp (envPlusTypedVars env [(x,ty)]) e
  end

| Case (v,cases,default, cty) =>
  let val SOME tyss = MILTy.fromSum (flowVal env v)
  in
    flowCases (fn i => List.nth(tyss,i)) (v, cases, default, cty)
  end

| CaseSCon (v, cases, default, cty) =>
  (flowVal env v; flowCases (fn _ => []) (v, cases, default, cty))
      
| TypeCase (v, cases, default, cty) =>
  (flowVal env v; flowCases (fn ty => [ty]) (v, cases, default, cty))

| Throw(v, cty, loc) =>
  (flowVal env v; cty)

| TryLet(e, handlers, (xs, body)) =>
  (
    flowCmp env e;
    app (fn (xs,e) => ignore (flowCmp (envPlusTypedVars env xs) e)) handlers;
    flowCmp (envPlusTypedVars env xs) body
  )

| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun flowMethods [] = 
        ()

      | flowMethods ((name, atts, mods, tys, tyopt, optabs)::methods) =
        case optabs of 
          NONE => 
          flowMethods methods 

        | SOME ((f,_), (xs, e)) =>
          let
            val argtys = 
              if Symbol.Set.member(mods, Id.staticSym)
              then tys
              else classname::tys
            val env = envPlusBoundVars env (xs,argtys)
            val cty = flowCmp env e
          in
            flowMethods methods 
          end
  in
    flowMethods methods;
    flowCmp env e
  end

| LetFun(tyvars, kind, def, ce) =>
  let

  (*..................................................................*)
  (* The type/kind environment for inside the defns		      *)
  (*..................................................................*)
    val defnenv = envPlusTyVars env tyvars
    val kind = normalizeKind kind

    val (defsvars, bodyvars) = 
      case def of
        Fun (f, _) => ([], [f])
      | RecFun recbinds => (map #2 recbinds, map #1 recbinds)

    fun flowDef (Fun (f, (xs, e))) =
        let
          (* Gather information about the body of the function *)
          val cty = flowCmp (envPlusTypedVars defnenv xs) e          

          (* The type of the function as seen externally *)
          val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))

          val bodyenv = 
            envPlusTypedVars env [(f, ty)]
          val bodyenv = envPlusFun bodyenv (kind, [f])
        in
          bodyenv
        end

      | flowDef (RecFun recbinds) =
        let
          val defnenv = 
            foldl
            (fn ((_, g, (xs, _), cty), defnenv) =>             
            envPlusTypedVars defnenv [(g, MILTy.arrow(map #2 xs, cty))]) defnenv recbinds
          val defnenv = envPlusFun defnenv (kind, defsvars)

          fun processDef (_, g, (xs, e), cty) = flowCmp (envPlusTypedVars defnenv xs) e
          val _ = app (ignore o processDef) recbinds
      
          val bodyenv = 
            foldl
            (fn ((f, g, (xs, _), cty), env) =>
              let
                val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))
              in
                envPlusTypedVars (envPlusFun env (kind, [f])) [(f, ty)]
              end) env recbinds
        in
          bodyenv
        end

    val bodyenv = flowDef def
    val bodycty = flowCmp bodyenv ce
  in
    bodycty
  end
end 

val _ = 
  PrintManager.process ("Collecting function applications", false)
    (fn () => flowCmp emptyEnv ce)

(* Each partition corresponds to an apply method name *)
(* We gather up the partitions and create a map from bound function variables *)
(* to the number of the partition *)
val defmeths = ListOps.foldri (fn (i,part,defmeths) =>
  foldl (fn (f, defmeths) => 
  let val defmeths = Var.Map.insert(defmeths, f, i)
  in
    case Var.Map.find(definfo, f) of
      NONE => defmeths
    | SOME { recf, ...} => if Var.isDummy recf then defmeths else Var.Map.insert(defmeths, recf, i)
  end) defmeths
  (Partition.list_part (!partition, part))) 
  Var.Map.empty
  (Partition.list_parts (!partition))

(* Create a similar map for unknown function applications *)
val refmeths = Var.Map.foldli (fn (unknown, known, refmeths) =>
  let val SOME m = Var.Map.find(defmeths, known)
  in Var.Map.insert(refmeths, unknown, m)
  end) 
  Var.Map.empty (!unknownapps)

(* Method types for each apply method *)
val methtys = map (fn part =>
  let
    val SOME { tyvars, args, cty, ... } =     
      Var.Map.find(definfo, hd (Partition.list_part (!partition, part)))
    val S = foldl (fn ((x,MILTy.Bound ty),S) => Var.Map.insert(S,x,ty))
      Var.Map.empty tyvars
  in
    MILTy.subst S (MILTy.arrow(map #2 args, cty))
  end) (Partition.list_parts (!partition))

local 
  open NewPretty 
  infixr 6 ++		(* Concatenation *)
  infixr 6 +/		(* Concatenation with space *or* newline *)
  infixr 5 //		(* Concatenation with newline *)
in
val _ = PrintManager.dumpDoc showClosFlow (fn () =>

  rule ++ bold (text "FUNCTION DEFINITIONS: ") ++ line ++ 
    spread (map (text o Var.toString o #1) (Var.Map.listItemsi definfo)) ++ line ++

  rule ++ bold (text "PARTITIONS:") ++ line ++
    stack (ListOps.mapi 
      (fn (i, part) => 
        text ("  Partition " ^ Int.toString i ^ "(" ^ Pretty.indexToAlpha i ^ ") = {") ++
        spread (map 
          (fn f => 
            let val SOME { tyvars, args, cty, ... } = Var.Map.find(definfo, f)
            in
              (* (if null tyvars then empty else text ("<" ^ Pretty.simpleVec "," MILTy.boundTyVarToString tyvars ^ "> ")) ++ *)
              text (Var.toString f) ++ text ":"  ++ MILTy.pTy (MILTy.arrow(map #2 args,cty))
            end) 
          (Partition.list_part (!partition,part)))
        ++ text "}"
      )
      (Partition.list_parts (!partition))) ++ line ++

  rule ++ bold (text "KNOWN APPLICATIONS:") ++ line ++
    fillWith "," (map (text o Var.toString) (Var.Set.listItems (!knownapps))) ++ line ++

  rule ++ bold (text "DEAD APPLICATIONS:") ++ line ++
    fillWith "," (map (text o Var.toString o #1) (Var.Map.listItemsi (!deadapps))) ++ line ++

  rule ++ bold (text "UNKNOWN APPLICATIONS:") ++ line ++
    stack (map (fn (x,i) => text ("  " ^ Var.toString x ^ ":" ^ Int.toString i ^ "(" ^ Pretty.indexToAlpha i ^ ")")) (Var.Map.listItemsi  refmeths))
)
end

in
  { defmeths = defmeths, refmeths = refmeths, deadapps = !deadapps, methtys = methtys }
end
  
end (* of local open *)
end (* of struct *)
