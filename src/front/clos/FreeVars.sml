(*======================================================================*)
(* Gather free variable information prior to closure converting.        *)
(*======================================================================*)
structure FreeVars : FREEVARS =
struct

local 
  open FreeVarsEnv MILTerm FreeVarsInfo MILTermOps
in

val showFVInfo = Controls.add false "clos.showFree"

(*----------------------------------------------------------------------*)
(* Function/method info: see signature for details.			*)
(*----------------------------------------------------------------------*)
type FunInfo = 
{
  kind : MILTerm.FunKind option,
  args : MILTerm.TypedVar list,
  fvs  : VarsInfo,
  cty  : MILTy.CmpType,
  tyvars : (Var.Var*MILTy.Kind) list,
  envtyvars : (Var.Var*MILTy.Kind) list,
  nonrecvar : Var.Var
} Var.Map.map


fun kindToString (SOME LocalFun) = "Block"
  | kindToString (SOME AnyFun) = "Closure"
  | kindToString (SOME KnownFun) = "Global function"
  | kindToString NONE = "Method"

(*----------------------------------------------------------------------*)
(* Gather information about a term ce.                                  *)
(*----------------------------------------------------------------------*)
fun gather {globalvals,globalrefs} e = 
let

(*----------------------------------------------------------------------*)
(* Free variable info for functions and methods   			*)
(* Methods have kind=NONE.                                              *)
(*----------------------------------------------------------------------*)
val funinfo = 
  ref (Var.Map.empty : FunInfo)

val resulttys =
  ref (Var.Map.empty : MILTy.CmpType Var.Map.map)

val globaltys = 
  ref (Var.Map.empty : MILTerm.TypedVar Var.Map.map)

val globalreftys =
  ref (Var.Map.empty : MILTerm.TypedVar Var.Map.map)

val bindeffects = 
  ref (Var.Map.empty : Effect.Effect Var.Map.map)

fun dump prln =
  (prln (Int.toString (Var.Map.numItems (!globaltys)) ^ " globals:");
   app (fn ((x,_),ty) => prln ("  " ^ Var.toString x ^ ":" ^ MILTy.toString ty)) (Var.Map.listItems (!globaltys));
   prln (Int.toString (Var.Map.numItems (!globalreftys)) ^ " global refs:");
   app (fn ((x,_),ty) => prln ("  " ^ Var.toString x ^ ":" ^ MILTy.toString ty)) (Var.Map.listItems (!globalreftys));
   prln (Int.toString (Var.Map.numItems (!funinfo)) ^ " functions and methods:");

   app (fn (x, { kind, args, fvs, cty, tyvars, envtyvars, ... }) => 
    (prln ("  " ^ kindToString kind ^ " " ^ Var.toString x ^ 
      (if null tyvars then "" else "<" ^ Pretty.simpleVec "," MILTy.boundTyVarToString tyvars ^ "> ") ^
      "<" ^ Pretty.simpleVec "," (fn ((x,_),ty) => Var.toString x ^ ":" ^ MILTy.toString ty) args ^ "> : " ^ 
      MILTy.cmpToString cty);
     prln ("    envtyvars = [" ^ Pretty.simpleVec "," MILTy.boundTyVarToString envtyvars ^ "]");
     prln ("    fvs = " ^ fvsToString fvs))) (Var.Map.listItemsi (!funinfo))
   )


val changed = ref true

fun lookupFun f =
  case Var.Map.find(!funinfo, f) of SOME { fvs, ... } => fvs | NONE => empty

(*----------------------------------------------------------------------*)
(* Store a function's info;				                *)
(*----------------------------------------------------------------------*)
fun addFun (env : Env, f, g, kind, tyvars, vs, cty, fvs : FreeVarsInfo.VarsInfo) =
  let
    val temp = Var.Map.insert(!funinfo, f, 
    { kind = kind, cty = cty, args = vs, fvs = fvs,
      tyvars = tyvars, envtyvars = Var.Map.listItemsi (#kindenv env), nonrecvar = f })
  in
    (if !changed then ()
    else
      if fvsEqCardinality (fvs, lookupFun f) then () else changed := true);
    
    funinfo := 
    (if Var.isDummy g then temp
     else
       (if !changed then ()
       else (if fvsEqCardinality (fvs, lookupFun g) then () else changed := true);
       Var.Map.insert(temp, g, { kind = kind, args = vs, cty = cty, 
         fvs = fvs, nonrecvar = f, tyvars = [], envtyvars = tyvars @ Var.Map.listItemsi (#kindenv env) })))
  end

(*----------------------------------------------------------------------*)
(* Collect initial free variable information about a value term.	*)
(* Determine its type at the same time.                                 *)
(*----------------------------------------------------------------------*)
fun fvVal env acc v =
  case v of

  Var x => 
  let val (typedvar as (_,ty)) = lookup(env, x)
  in
    (* If it's been classified as a global ref then don't count it *)
    if Var.Set.member(globalrefs, x)
    then (globalreftys := Var.Map.insert(!globalreftys, x, (#1 typedvar, hd (#1 (valOf (MILTy.fromRefty ty))))); (ty, acc))
    else

    (* If it's been classified as a global then don't count it *)
    if Var.Set.member(globalvals, x)
    then (globaltys := Var.Map.insert(!globaltys, x, typedvar); (ty, acc))
    else

    case lookupFunKind(env, x) of 
      (* It's a closure value or not a function so it counts as a free variable *)
      NONE =>
      (ty, add acc (x,ty))

    | SOME AnyFun =>
      (ty, add acc (x,ty))

      (* It's an application of a local function so add in the function's live vars
         as these must be counted as live. The function itself is not a free var. *)
    | SOME LocalFun => (ty, union(lookupFun x, acc))

      (* It's an application of a known function so add in the function's free vars
         as these must be passed as extra args. The function itself is not a free var. *)
    | SOME KnownFun => (ty, union(lookupFun x, acc))
  end

  (* For all other value terms just decompose *)
| SCon(ty, _) => (ty, acc)

| Fold(v, ty) => 
  let val (_, acc) = fvVal env acc v
  in
    (ty, acc)
  end

| As(v, ty) => 
  let val (_, acc) = fvVal env acc v
  in
    (ty, acc)
  end

| Unfold v => 
  let
    val (ty', acc) = fvVal env acc v
    val SOME a = MILTy.fromMu ty'
  in
    (MILTy.unfold a, acc)
  end

| Inj(ty, _, vs, _) => 
  let val (tys, acc) = fvVals env acc vs 

  in
    (ty, acc)
  end

| ExCon(excon, vs) => 
  let val (tys, acc) = fvVals env acc vs
  in
    (MILTys.topExn, acc)
  end

| Tuple vs =>
  let val (tys, acc) = fvVals env acc vs
  in
    (MILTy.prod tys, acc)
  end

| Proj(i, n, v) => 
  let 
    val (prodty, acc) = fvVal env acc v
    val SOME tys = MILTy.fromProdCon prodty
  in
    (List.nth(tys,i), acc)
  end

| TAbs(tyvars, v) =>
  let val (ty, acc) = fvVal (envPlusTyVars env tyvars) acc v
  in
    (MILTy.forall (tyvars, ty), acc)
  end

| TApp(v, tys) =>
  let
    val (polyty, acc) = fvVal env acc v
    val SOME a = MILTy.fromForall polyty
  in
    (MILTy.app(MILTy.abs a, tys), acc)
  end


(*----------------------------------------------------------------------*)
(* Collect initial free variable information about value terms.   	*)
(* Determine their types at the same time.                              *)
(*----------------------------------------------------------------------*)
and fvVals env acc [] = 
    ([], acc)

  | fvVals env acc (v::vs) =
    let
      val (ty, acc) = fvVal env acc v
      val (tys, acc) = fvVals env acc vs
    in
      (ty::tys, acc)
    end

(*----------------------------------------------------------------------*)
(* Collect initial free variable information about a computation term. 	*)
(* Determine its type at the same time.                                 *)
(*----------------------------------------------------------------------*)
and fvCmp env acc e =
let
  fun fvCases tysFor acc (v, cases, optdefault, cty) =
  let
    fun gather acc [] = 
        ([], acc)

      | gather acc ((i, (xs, e))::rest) =
        let
          val env = envPlusTypedVars env (ListPair.zip(xs,tysFor i))
          val (cty, acc) = fvCmp env acc e
          val (ctys, acc) = gather (removeBound (acc, xs)) rest
        in
          (cty::ctys, acc)
        end

    val (ctys, acc) = gather acc cases
    
    val (cty::ctys, acc) = 
      case optdefault of
        NONE => (ctys, acc)
      | SOME e =>
        let val (cty, acc) = fvCmp env acc e
        in (cty::ctys, acc) end
  in
    (cty,acc)
  end
in
  case e of

  App(v, vs) =>
  let
    val (funty, acc) = fvVal env acc v    
    val SOME (_, cty) = MILTy.fromArrow funty
    val (tys, acc) = fvVals env acc vs 
  in
    (cty, acc)
  end

| Special(_, vs, cty) =>
  let val (_, acc) = fvVals env acc vs
  in
    (cty, acc)
  end

| Encap e =>
  let
    val (cty, acc) = fvCmp env acc e
    val (eff, tys) = MILTy.fromCmp cty
  in
    (MILTy.cmp(Effect.allocs, tys), acc)
  end

| Let(e1, (xs, e2)) =>
  let
    val (e1ty, acc) = fvCmp env acc e1
    val (e2ty, acc) = fvCmp (envPlusTypedVars env xs) acc e2
    val (eff,_) = MILTy.fromCmp e1ty
  in  
    app (fn (x,_) => bindeffects := Var.Map.insert(!bindeffects, #1 x, eff)) xs;
    (MILTy.unionCmpTypes(e1ty,e2ty), removeTyped (acc, xs))
  end

| LetVal(x, v, e) =>
  let
    val (ty, acc) = fvVal env acc v
    val (cty, acc) = fvCmp (envPlusTypedVars env [(x,ty)]) acc e
  in
    (cty, removeBound (acc, [x]))
  end

| Triv vs =>
  let val (tys, acc) = fvVals env acc vs
  in
    (MILTy.noeffect tys, acc)
  end

| Case (v,cases,default,cty) =>
  let 
    val (ty, acc) = fvVal env acc v
    val SOME tyss = MILTy.fromSum ty 
    fun tysFor i = List.nth(tyss, i)
  in
    fvCases tysFor acc (v, cases, default,cty)
  end

| CaseSCon (cases as (v, _, _, _)) =>
  let 
    val (_, acc) = fvVal env acc v
  in
    fvCases (fn _ => []) acc cases
  end
      
| TypeCase (v, cases, default, cty) =>
  let 
    val (_, acc) = fvVal env acc v
    fun tysFor ty = [ty]
  in
    fvCases tysFor acc (v, cases, default, cty)
  end

| Throw(v, cty, loc) =>
  let
    val (_, acc) = fvVal env acc v
  in
    (cty, acc)
  end

| TryLet(e, handlers, (xs, body)) =>
  let
    val (cty0, acc) = fvCmp env acc e

    val acc = 
    foldr (fn ((xs, e), acc) => 
    let
      val (cty, acc) = fvCmp (envPlusTypedVars env xs) acc e
    in
      removeTyped (acc, xs)   
    end) acc handlers

    val (cty2, acc) = fvCmp (envPlusTypedVars env xs) acc body
  in
    (cty2, removeTyped (acc, xs))
  end

(*......................................................................*)
(* If there are any free variables in the methods then we put them in   *)
(* globals first and extract them in individual methods.                *)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun fvMethods [] acc = 
        acc

      | fvMethods ((name, atts, mods, tys, tyopt, optabs)::methods) acc =
        case optabs of 
          NONE => 
          fvMethods methods acc

        | SOME (f:BoundVar, (xs, e):Abstr) =>
          let
            val argtys = 
              if Symbol.Set.member(mods, Id.staticSym)
              then tys
              else classname::tys
            val env = envPlusBoundVars env (xs,argtys)
            val (cty, methodfvs) = fvCmp env empty e
            val methodfvs = removeBound(methodfvs, xs)
          in
            addFun (env, #1 f, Var.dummy, NONE, [],
              ListPair.zip(xs,argtys), cty, methodfvs);
            fvMethods methods (union(acc, methodfvs))
          end

    val acc = fvMethods methods acc
  in
    fvCmp env acc e
  end

| LetFun(tyvars, kind, def, e) =>
  let

    val kind = FunFlow.normalizeKind kind
    val defnenv = envPlusTyVars env tyvars

    val (defsvars, bodyvars) = 
      case def of
        Fun (f, _) => ([], [f])
      | RecFun recbinds => (map #2 recbinds, map #1 recbinds)

    fun fvDef fvs (Fun (f, (xs, e))) =
        let
          (* Gather information about the body of the function *)
          val (cty, funfvs) = fvCmp (envPlusTypedVars defnenv xs) empty e

          (* Remove argument variables *)
          val funfvs = removeTyped (funfvs, xs)

          (* The type of the function as seen externally *)
          val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))

          val bodyenv = envPlusTypedVars env [(f, ty)]
          val bodyenv = envPlusFun bodyenv (kind, [f])

          (* Free variables that count towards enclosing scopes *)
          val fvs = case kind of
            KnownFun => fvs
          | _ => union (funfvs, fvs)
        in
          resulttys := Var.Map.insert(!resulttys, #1 f, cty);

          addFun (env, #1 f, Var.dummy, SOME kind, tyvars, xs, cty, funfvs);

          (bodyenv, fvs)
        end

      | fvDef fvs (RecFun recbinds) =
        let
          val defnenv = 
            foldr (fn ((_, g, (xs, _), cty), defnenv) =>             
            envPlusTypedVars defnenv [(g, MILTy.arrow(map #2 xs, cty))]) defnenv recbinds
          val defnenv = envPlusFun defnenv (kind, defsvars)

          fun processDef ((f:MILTerm.BoundVar, g:MILTerm.BoundVar, (xs, e), cty), fvs) =
            let
              val (cty, funfvs) = fvCmp (envPlusTypedVars defnenv xs) empty e
               
              (* Remove argument variables *)
              val funfvs = removeTyped (funfvs, xs)

              (* Remove recursive occurrences *)
              val funfvs = removeBound(funfvs, [g])              

              (* Free variables that count towards enclosing scopes *)
              val fvs = case kind of
                KnownFun => fvs
              | _ => union (funfvs, fvs)
            in
              addFun (env, #1 f, #1 g, SOME kind, tyvars, xs, cty, funfvs);
              fvs
            end
      
          val fvs = foldr processDef fvs recbinds

          (* Remove recursive occurrences as these don't count *) 
          val fvs = removeBound(fvs, defsvars)

          val bodyenv = 
            foldr
            (fn ((f, _, (xs, _), cty), env) =>
              let
                val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))
              in
                envPlusTypedVars env [(f, ty)]
              end) env recbinds

          val bodyenv = envPlusFun bodyenv (kind, bodyvars)
        in
          (bodyenv, fvs)
        end

    val (bodyenv, acc) = fvDef acc def
    val (bodycty, acc) = fvCmp bodyenv acc e
  in
    (bodycty, removeBound (acc, bodyvars))
  end
end (* of let fun fvCases *)

(* Iterate the free variable calculation until a fixed point is reached *)
fun iterate (iter, e) =
let
  val (cty, fvs) = fvCmp emptyEnv empty e
in
  if !changed 
  then 
  (
    changed := false; 
    iterate(iter+1, e)
  )
  else fvs
end

val fvs = iterate (1, e)

in
  PrintManager.dump showFVInfo dump;
  { globalfuns = !funinfo, resulttys = !resulttys, globalvals = !globaltys, globalrefs = !globalreftys,
    bindeffects = !bindeffects }
end
  
end (* of local open *)
end (* of struct *)
