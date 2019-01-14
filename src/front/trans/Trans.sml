(*======================================================================*)
(* Translation to MIL.                                                  *)
(* The target of the translation is the `flat value' subset of MIL in   *)
(* which values appear only in val; elsewhere, they must be atoms.      *)
(*======================================================================*)
structure Trans :> TRANS =
struct

local 
  open MILTerm TransType TransOps Gen
in

structure Map = Symbol.Map
structure Set = Symbol.Set

(* annotate bound vars with source info? *)
(* val debug = Controls.add true "debug" *)

(*======================================================================*)
(* Main entry point!                                                    *)
(*======================================================================*)
fun trans 
{ sourcemap, SE : TransOps.StrEnv,
  entity, strexp, tynameTys = TNE, supply } = 
let

val {freshAnonVar,freshBoundVar,letLine,withSource} = mkFreshVarFns (#2 entity,sourcemap)

(*----------------------------------------------------------------------*)
(* Evaluation of several expressions: optimise the Triv case.           *)
(*----------------------------------------------------------------------*)

fun transEval tr es f =
  let
    fun t typedvs [] = f (rev typedvs)
      | t typedvs ((e as (loc,_)) ::es) = 
        let
          val (ce, cty) = tr e
          val (eff, [ty]) = MILTy.fromCmp cty
          fun default () =
          let
            val (x,xv) = freshAnonVar (SOME loc)
            val (ce2, cty2) = t ((xv, ty) :: typedvs) es
          in
            case ce of 
              Triv [ve] => (LetVal(x, ve, ce2), cty2)
            | _ => (Let(ce, ([(x,ty)], ce2)), MILTy.unionCmpTypes(cty,cty2))
          end
        in
          case ce of
            Triv [ve as Var v] =>
            t ((ve, ty)::typedvs) es

          | Triv [ve as SCon c] => 
            t ((ve, ty)::typedvs) es

          | _ => default ()
        end
  in
    t [] es
  end

(*@TODO: merge with transEval *)
fun transEvalSansLoc tr es f =
  let
    fun t typedvs [] = f (rev typedvs)
      | t typedvs (e::es) = 
        let
          val (ce, cty) = tr e
          val (eff, [ty]) = MILTy.fromCmp cty
          fun default () =
          let
            val (x,xv) = freshAnonVar NONE
            val (ce2, cty2) = t ((xv, ty) :: typedvs) es
          in
            case ce of 
              Triv [ve] => (LetVal(x, ve, ce2), cty2)
            | _ => (Let(ce, ([(x,ty)], ce2)), MILTy.unionCmpTypes(cty,cty2))
          end
        in
          case ce of
            Triv [ve as Var v] =>
            t ((ve, ty)::typedvs) es

          | Triv [ve as SCon c] => 
            t ((ve, ty)::typedvs) es
        
          | _ => default ()
        end
  in
    t [] es
  end
(*----------------------------------------------------------------------*)
(* Possible upcast                                                      *)
(*----------------------------------------------------------------------*)
fun upcast (v,ty1,ty2) =
  if MILTy.eq(ty1,ty2) then v
  else As(v, ty2)

(*----------------------------------------------------------------------*)
(* Given a function value whose type is the interop translation of an   *)
(* external method type (with tuples for multiple args, unit for void   *)
(* result), return a "wrapped" version whose function type matches the  *)
(* actual method type (with multiple args and possibly-void result).    *)
(*----------------------------------------------------------------------*)
fun wrapFunAsMeth v (usety,defty) =
let
  val SOME ([useargty],usecty) = MILTy.fromArrow usety
  val SOME ([defargty],defcty) = MILTy.fromArrow defty

  val (_,[useresty]) = MILTy.fromCmp usecty
  val (_,[defresty]) = MILTy.fromCmp defcty

  val (f,fv) = freshAnonVar NONE

  val unitres = MILTy.eq (defresty, MILTys.unit)
  val cty = MILTy.anyeffect (if unitres then [] else [useresty])

  val ((xs, e), actualty) = 
    case MILTy.fromProd defargty of
      SOME defargtys =>
      let 
        val ys = map (fn ty => (#1 (freshAnonVar NONE), ty)) defargtys
        val (p,pv) = freshAnonVar NONE
      in
        ((ys, LetVal(p, Tuple (map (Var o #1 o #1) ys), App(v, [pv]))),
          MILTy.arrow(defargtys, cty))
      end

    | NONE =>
      let 
        val (y,yv) = freshAnonVar NONE
      in
        (([(y,defargty)], App(v, [yv])), MILTy.arrow ([defargty], cty))
      end

  val e = 
    if unitres then Let(e, ([(MILTermOps.dummyBoundVar,MILTys.unit)], Triv [])) else e
in
  (LetFun([], AnyFun, Fun (f, (xs,e)), Triv [fv]), actualty)
end

(*----------------------------------------------------------------------*)
(* Given                                                                *)
(*----------------------------------------------------------------------*)
fun wrapInvoc (info,preargs) (usety,defty) =
let
  val SOME ([useargty],usecty) = MILTy.fromArrow usety
  val SOME ([defargty],defcty) = MILTy.fromArrow defty

  val (_,[useresty]) = MILTy.fromCmp usecty
  val (_,[defresty]) = MILTy.fromCmp defcty

  val (x,xv) = freshAnonVar NONE
  val (f,fv) = freshAnonVar NONE

  val unitres = MILTy.eq (defresty, MILTys.unit)
  val cty = MILTy.anyeffect(if unitres then [] else [useresty])

  val e = 
    case MILTy.fromProd defargty of
      SOME defargtys =>
      let val ys = map (fn ty => (#1 (freshAnonVar NONE), ty)) defargtys
          val n = length ys
      in
        ListOps.foldri 
        (fn (i, (y, ty), e) => LetVal(y, Proj(i, n, xv), e))
        (Special(info, 
          preargs @ 
          map (fn (y, ty) => As(Var (#1 y),ty)) ys, cty))
          ys
      end

    | NONE =>
      if isSome (MILTy.fromArrow defargty)
      then
      let
        val (x',xv') = freshAnonVar NONE
        val (e,x'ty) = wrapFunAsMeth xv (useargty, defargty)
      in
        Let(e, ([(x', x'ty)], Special(info, preargs @ [xv'], cty)))
      end
      else
      Special(info, preargs @ [As(xv,defargty)], cty)

  val e = 
    if unitres then Let(e, ([], Triv [Tuple []])) else e
in
  LetFun([], AnyFun, Fun (f, ([(x, useargty)], e)), Triv [fv])
end


(*@TODO: remove this comment
(*======================================================================*)
(* Main entry point!                                                    *)
(*======================================================================*)
fun trans 
{ sourcemap, SE : TransOps.StrEnv, 
  entity, strexp, tynameTys = TNE, supply } = 
let
*)
fun freshPolyVars (VE, [], tyvars) = 
    (VE, [])

  | freshPolyVars (VE : TransOps.ValEnv, (v,ty)::vs, tyvars) = 
    let
      val (x,xv) = freshBoundVar (NONE,v)
      val (VE, xs) = freshPolyVars (VE, vs, tyvars)
    in
      (Map.insert(VE, v, ((#1 x, MILTy.forall(tyvars, ty)), 
        map (MILTy.tyvar o #1) tyvars)), x::xs)
    end

fun freshVars (VE, []) = (VE, [])
  | freshVars (VE, (v,ty)::vs) = 
    let
      val (x,xv) = freshBoundVar (NONE,v)
      val (VE', vs') = freshVars (VE, vs)
    in
      (Symbol.Map.insert(VE', v, ((#1 x,ty),[])), x::vs')
    end

(*----------------------------------------------------------------------*)
(* Translate an SML longstrid into a series of projections.             *)
(* Return a value expression and a list of value bindings.              *)
(*----------------------------------------------------------------------*)
fun transLongStrid (SE : TransOps.StrEnv) (strid, rest) =
  case Map.find(SE, strid) of

    (* It must be a package/class as structure, so ignore *)
    (*@BUG: e.g. "open System val x = Threading.Thread ()" *)
    NONE => 
    (Debug.print ("\nClass/package structure: " ^ Id.toString strid);
    (Tuple [], MILTy.prod [], []))
  | SOME (milvar, milty) =>
    let
      fun trans (v,milty) [] bindings = (v, milty, rev bindings)
        | trans (v,milty) ((id,i)::rest) bindings =
          case MILTy.fromProd milty of
            NONE =>
            Debug.fail "Trans.transLongStrid: expected product type"

          | SOME vtys =>          
            if i>=length vtys
            then Debug.fail 
              ("Trans.transLongStrid: index " ^ Int.toString i ^ 
              " out of range in " ^ SMLTermOps.expToString 
                 (*@TODO: revise loc*)
                ({left=0,right=0},SMLTerm.Var ((strid,rest), [])) ^ " with MIL type " ^
              MILTy.toString milty)
            else
            let
              val (x,xv) = freshAnonVar NONE              
            in
              trans (xv, List.nth(vtys, i)) rest ((x, Proj(i, length vtys, v))::bindings)
            end
    in
      trans (Var milvar, milty) rest []
    end

(*----------------------------------------------------------------------*)
(* Translate an SML longid into a series of projections.                *)
(* Return a value expression and a list of value bindings.              *)
(*----------------------------------------------------------------------*)
fun transLongid extra (SE : TransOps.StrEnv) (VE : TransOps.ValEnv) (id, []) =
    (case Map.find(VE, id) of
      SOME ((milvar, milty), tys') =>
      if null tys' orelse not extra
      then
      (
        Var milvar,
        milty,
        []
      )
      else
      let
        val SOME (kinds, ty) = MILTy.fromForall milty
      in
        if length tys' <> length kinds
        then Debug.fail ("Trans.transLongid: length mismatch in " ^
          Id.toString id)
        else
        (
          MILTermOps.tapp(Var milvar, tys'),        
          MILTy.app(MILTy.abs (kinds, ty), tys'),
          []
        )    
      end

    | NONE => 
      Debug.fail ("Trans.transLongid: missing identifier " ^ 
        Id.toString id)
    )
  
  | transLongid extra SE VE (strid, rest) = transLongStrid SE (strid, rest)



(*----------------------------------------------------------------------*)
(* Translate an ML `valuable' expression into a MIL value term.         *)
(* Inputs:                                                              *)
(*    SE: map from SML structure identifiers to MIL variables with      *)
(*        tuple types;                                                  *)
(*    TVE:map from SML type variables to MIL types;                     *)
(*    EE: map from SML generative exceptions to MIL variables holding   *)
(*        the dynamically-generated exception tag;                      *)
(*    VE: map from SML value identifiers to MIL variables and types;    *)
(*    funtyvars:                                                        *)
(*    e:  the SML valuable expression that is to be translated.         *)
(* Outputs:                                                             *)
(*    (ve, ty, funbinds, binds)                                         *)
(* where                                                                *)
(*    ve: MIL value term                                                *)
(*    ty: MIL value type                                                *)
(*    funbinds: function bindings                                       *)
(*    binds: value bindings                                             *)
(*----------------------------------------------------------------------*)
fun transVal
  (SE : TransOps.StrEnv)
  (TVE : MILTy.Type TyVar.Map.map)               
  (EE : TransOps.ExEnv)
  (VE : TransOps.ValEnv)
  (funtyvars)
  ((loc,e) : SMLTerm.Exp) =
let


(*......................................................................*)
(* Given a typed abstraction and result type, construct the appropriate *)
(* transVal result as a function binding.                               *)
(*......................................................................*)
fun makeFun (tabs : MILTerm.TAbstr as (typedvars,_), cty) = 
  let
    val (f,fv) = freshAnonVar NONE
  in
    (
      if null funtyvars 
      then fv
      else TApp(fv, map (MILTy.tyvar o #1) funtyvars), 
      MILTy.arrow (map #2 typedvars, cty),
      [(f, tabs)], 
      []
    )
  end

in
case e of

(*----------------------------------------------------------------------*)
(* Convert a resolved constant, possibly reporting an overflow error.   *)
(*----------------------------------------------------------------------*)
  SMLTerm.SCon(scon, ty, loc) => 
  let 
    val ty' = transType TVE TNE ty
    val v = 
    case TransSCon.trans (scon, ty') of
      NONE => 
      (addError(Error.error(loc, "constant too large")); Tuple [])

    | SOME jcon => 
      SCon (ty', jcon)
  in
    (v, ty', [], [])
  end

(*----------------------------------------------------------------------*)
(* Non-overloaded longid                                                *)
(*----------------------------------------------------------------------*)
| SMLTerm.Var (longid, tys) =>
  let
    val (v, ty, bindings) = transLongid (null tys) SE VE longid
  in
    case MILTy.fromForall ty of

  (*..................................................................*)
  (* Polymorphic value                                                *)
  (*..................................................................*)
      SOME (a as (kinds,_)) => 
      let
        val miltys = map (transType TVE TNE) tys         
      in
        if length kinds <> length miltys
        then Debug.fail ("Trans.transVal: length mismatch in " ^
          SMLTermOps.expToString (loc,e))     
        else (TApp(v, miltys), MILTy.app (MILTy.abs a, miltys), [], bindings)
      end

  (*..................................................................*)
  (* Monomorphic value                                                *)
  (*..................................................................*)
    | NONE => 
      (v, ty, [], bindings)
  end

(*----------------------------------------------------------------------*)
(* Overloaded longid                                                    *)
(*----------------------------------------------------------------------*)

| SMLTerm.OverloadedVar (longid, tynameset, tys) =>
  let
    val (v, ty, bindings) = transLongid (null tys) SE VE longid
(*
    val _ = print (MILPretty.valToString v ^ " : " ^ MILTy.toString ty ^ " ")
    val _ = print ("{" ^ Pretty.simpleVec "," SMLTy.toString tys ^ "}\n")
*)
  in
    case tys of
(*
      [] => 
      (v, ty, [], bindings)

    | *) firstty::_ =>

      let
        (* We're expecting the MIL type to be a product *)
        val miltys = 
          case MILTy.fromProd ty of
            SOME tys => 
            tys

          | NONE =>
            Debug.fail ("Trans.transVal: expected product type for \
              \overloaded identifier: " ^ Longid.toString 
              (map #1 (#2 longid)) ^ ":" ^ MILTy.toString ty)
        
        (* The type might be an internal tyvar. If so, default it. *)
        val tynameopt = 
          case SMLTy.fromConsType firstty of
            SOME ([], tyname) => 
            SOME tyname

          | _ => 
            case SMLTy.fromTyVar firstty of
              SOME tyvar =>
              (case TyVar.sort tyvar of
                 TyVar.Overloaded tynames => SOME (TyNames.default tynames)
               | _ => Debug.fail "Trans.transVal: bad overloading type")
            | _ => Debug.fail "Trans.transVal: bad overloading type"

        fun find (i, tyname, [], []) = 
            Debug.fail("Trans.transVal: overloading not implemented at type "
              ^ TyName.toString tyname ^ " for identifier " ^ 
              Longid.toString (map #1 (#2 longid)))

          | find (i, tyname, tyname'::tynames, milty::miltys') =
            if TyName.eq(tyname,tyname') 
            then 
            (
              Proj(i, length miltys, v), 
              milty,
              [],
              bindings
            )
            else find (i+1, tyname, tynames, miltys')

          | find _ =
            Debug.fail("Trans.transVal: tynameset does not match product type "
              ^ MILTy.toString ty)
      in
        case tynameopt of
          SOME tyname => 
          find (0, tyname, TyName.Set.listItems tynameset, miltys)

        | NONE => 
          (
            v,
            ty,
            [],
            bindings
          )
      end
  end

  
(*----------------------------------------------------------------------*)
(* Datatype constructor                                                 *)
(*----------------------------------------------------------------------*)
| SMLTerm.Con (con, (tyvars, tyname, CE), tys) => 
  let 
    val ty = SMLTy.consType(tys, tyname)
    val recty = transType TVE TNE ty
    val S = SMLTy.appSubst (ListPair.zip(tyvars, tys))
    val CE = Map.map (Option.map S) CE
    
    val (k, tyopt) =  case SMLTy.findCon(tyname, CE, con) of
                        SOME res => res
                      | NONE => (0,NONE) (* must be an enum constant *)
                      
    val sumty = transCE TVE TNE CE 
    val singlenullary = case Map.listItems CE of [NONE] => true | _ => false 
    val unaryrep = case Map.listItems CE of [SOME _] => true | _ => false
    fun makeTerm args =
      case InterOp.isEnumType ty of
        SOME _ => (case tyopt of 
                    SOME _ => As(hd args,recty) 
                  | NONE => (* interop: translate enum constant *)
                        case InterOp.getStaticFields (ty, con) of
                            [(_, SOME c)] => 
                                SCon(recty, c)
                          | _ => Debug.fail "Trans.transVal: bad enum constant")
      | NONE =>
      let 
        val ve = 
          if unaryrep
          then hd args 
          else Inj(sumty, k, args, con)
      in
        Fold(ve, recty)
      end
  in
    if singlenullary 
    then 
      (Fold(Tuple [],recty), recty, [], [])
    else
    case tyopt of
      NONE => 
      (makeTerm [], recty, [], [])

    | SOME ty' => 
      let
        val (x,xv) = freshAnonVar NONE
        val milty = transType TVE TNE ty'
      in
        makeFun (([(x, milty)], Triv [makeTerm [xv]]), 
          MILTy.noeffect [recty])
      end
  end

(*----------------------------------------------------------------------*)
(* Fully applied datatype constructor                                   *)
(*----------------------------------------------------------------------*)
| SMLTerm.App((conloc,SMLTerm.Con (con, (tyvars, tyname, CE), tys)), e) => 
  let 
    val ty = SMLTy.consType(tys,tyname)
    val recty = transType TVE TNE ty
    val S = SMLTy.appSubst (ListPair.zip(tyvars, tys))
    val CE = Map.map (Option.map S) CE
    val (k, tyopt) = case SMLTy.findCon(tyname, CE, con) of
                         SOME res => res
                       | NONE => (0,NONE) (* must be an enum constant *)
    val sumty = transCE TVE TNE CE 
    val unaryrep = case Map.listItems CE of [SOME _] => true | _ => false
    fun makeTerm arg =
      case InterOp.isEnumType ty of
          SOME _ => As(arg,recty) 
      |   NONE =>
              let 
                  val ve = 
                      if unaryrep
                          then arg 
                      else Inj(sumty, k, [arg], con)
              in
                  Fold(ve, recty)
              end
  in
    case tyopt of
      NONE => 
      Debug.fail "Trans.transVal: tried to apply nullary con"

    | SOME ty' => 
      let
        val (ve, _, funbinds, binds) = transAtom SE TVE EE VE funtyvars e
      in
        (makeTerm ve, recty, funbinds, binds)
      end
  end

(*----------------------------------------------------------------------*)
(* Exception constructor                                                *)
(*----------------------------------------------------------------------*)
| SMLTerm.ExCon(exname, ty) =>
  let
    val varopt = TyName.Map.find(EE, exname)      
    val extra = map Var (OptionOps.toList varopt)
    val extraty = if isSome varopt then [MILTys.int] else []
  in
  case ty of 
    SOME ty => 
    let
      val (x,xv) = freshAnonVar NONE
      val milty = transType TVE TNE ty
      val milexty = MILTy.exn (Exn.exn [exname], extraty @ [milty])
    in
      makeFun (([(x,milty)], Triv [ExCon(milexty, extra @ [xv])]),
        MILTy.noeffect [MILTys.topExn])
    end

  | NONE =>
    (ExCon(MILTy.exn (Exn.exn [exname], extraty), extra), MILTys.topExn, [], [])
  end

(*----------------------------------------------------------------------*)
(* Fully-applied exception constructor                                  *)
(*----------------------------------------------------------------------*)
| SMLTerm.App((exconloc,SMLTerm.ExCon(exname, ty)), e) =>
  let
    val varopt = TyName.Map.find(EE, exname)
    val extra = map Var (OptionOps.toList varopt)
    val extraty = if isSome varopt then [MILTys.int] else []
  in
  case ty of 
    SOME ty => 
    let
      val (ve, _, funbinds, binds) = transAtom SE TVE EE VE funtyvars e
      val milty = transType TVE TNE ty
      val milexty = MILTy.exn (Exn.exn [exname], extraty @ [milty])
    in
      (ExCon(milexty, extra @ [ve]), MILTys.topExn, funbinds, binds)
    end

  | NONE =>
    Debug.fail "Trans.transVal: tried to apply nullary excon"
  end

(*----------------------------------------------------------------------*)
(* Pattern matching lambda abstraction: use pattern compiler.           *)
(*----------------------------------------------------------------------*)
| SMLTerm.Fn (ty, match) =>
  let
    val (tabs, cty) = 
      Pat.transFn 
      { sourcemap = sourcemap, entity = entity, transExp = transExp SE TVE EE, 
        freshAnonVar = freshAnonVar, freshBoundVar=freshBoundVar,
        transType = transType TVE TNE, VE = VE, EE = EE, smlty = ty, 
        match = match
      }
  in
    makeFun (tabs, cty)
  end

(*----------------------------------------------------------------------*)
(* Record construction (values only)                                    *)
(*----------------------------------------------------------------------*)
| SMLTerm.Record fields =>
  let 
    fun evalfld ([], vmap, funbinds, binds) =         
        let
          val vlist = Id.fixMap vmap
          val (vs, tys) = ListPair.unzip (map #2 vlist)
        in
          (Tuple vs, MILTy.prod tys, funbinds, binds)
        end

      | evalfld ((label, e, _) :: rest, vmap, funbinds, binds) =
        let
          val (ve, ty, funbinds', binds') = transAtom SE TVE EE VE funtyvars e
        in
          evalfld (rest, Map.insert(vmap, label, (ve,ty)), 
          funbinds @ funbinds', binds @ binds')
        end
  in
    evalfld (fields, Map.empty, [], [])
  end

| _ =>
  Debug.fail "Trans.transVal: non-valuable expression"
end

and transAtom SE TVE EE VE tyvars (e as (loc,SMLTerm.SCon _)) =
    transVal SE TVE EE VE tyvars e

  | transAtom SE TVE EE VE tyvars (e as (loc,SMLTerm.Var _)) =
    transVal SE TVE EE VE tyvars e
 
  | transAtom SE TVE EE VE (tyvars : (Var.Var * MILTy.Kind) list) e =
    let
      val (v, ty, funbinds, binds) = transVal SE TVE EE VE tyvars e
      val (x,xv) = freshAnonVar NONE
    in
      (MILTermOps.tapp(xv, map (MILTy.tyvar o #1) tyvars), 
         ty, funbinds, binds @ [(x,MILTermOps.tabs(tyvars,v))])
    end


(*----------------------------------------------------------------------*)
(* Translate an ML typed expression e into a MIL computation term.      *)
(*----------------------------------------------------------------------*)
and transExp
  (SE : TransOps.StrEnv)
  (TVE : MILTy.Type TyVar.Map.map)               
  (EE : TransOps.ExEnv)
  (VE : TransOps.ValEnv)
  (loce as (loc,e): SMLTerm.Exp) =
let
  val transEval = transEval (transExp SE TVE EE VE)
in

(
case e of

(*----------------------------------------------------------------------*)
(* Application: evaluate function expression, evaluate argument, apply  *)
(*----------------------------------------------------------------------*)
  SMLTerm.App (e1, e2) =>
  transEval [e1,e2] (fn [(ve1,funty),(ve2,_)] => 
  case MILTy.fromArrow funty of 
    NONE =>
    Debug.fail ("Trans.transExp: expected arrow type: " ^ MILTy.toString funty
    ^ " in " ^ SMLTermOps.expToString loce)
  | SOME (_,tys) => 
  letLine loc
  (App(ve1, [ve2]), tys))

(*----------------------------------------------------------------------*)
(* Special form for no-op casts.                                        *)
(*----------------------------------------------------------------------*)
| SMLTerm.Special(j as (Ext.NopCast, NONE, NONE), [e], SOME ty, effect) =>
  let
    val ty = transType TVE TNE ty
  in
    transEval [e] (fn [(ve,_)] =>  
    (Triv [As(ve, ty)], MILTy.cmp(effect, [ty])))
  end

(*----------------------------------------------------------------------*)
(* Special form for purity assertions.                                  *)
(*----------------------------------------------------------------------*)
| SMLTerm.Special(j as (Ext.Pure, NONE, NONE), [e], SOME ty, effect) =>
  let
    val (ce, cty) = transExp SE TVE EE VE e
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap ce, MILTy.cmp(Effect.allocs, tys))
  end

(*----------------------------------------------------------------------*)
(* Special operation: evaluate arguments, apply                         *)
(*----------------------------------------------------------------------*)
| SMLTerm.Special(info as (optype,tyopt,nameopt), es, tyopt', effect) =>
  let 
    val info = (optype, Option.map (transType TVE TNE) tyopt, nameopt)
  in
    transEval es (fn typedvs =>
    letLine loc (
    case tyopt' of
      NONE =>
      let
        val cty = MILTy.cmp(effect, [])
      in
        (Let(Special(info, map #1 typedvs, cty), ([],Triv [Tuple []])), 
          MILTy.cmp(effect, [MILTy.prod []]))
      end

    | SOME ty =>
      let
        val cty = MILTy.cmp(effect, [transType TVE TNE ty])
      in
        (Special(info, map #1 typedvs, cty), cty)
      end
    ))
  end

(*----------------------------------------------------------------------*)
(* Partial method/constructor invocation or field access                *)
(*----------------------------------------------------------------------*)
| SMLTerm.Invoc 
  { usety=(useclass,usety), defty=(defclass,defty), name, object, optype } =>
  letLine loc
  let

    (* Distingish interface invoke from ordinary invoke *)
    val optype = 
      case optype of 

        Ext.Invoke => 
        if InterOp.isInterface defclass then Ext.InvokeInterface
        else Ext.Invoke

      | _ => 
        optype
  
    (* Translate method types into MIL *) 
    val usemilty = transType TVE TNE usety
    val defmilty = transType TVE TNE defty
    val usemilclass = transType TVE TNE useclass
    val defmilclass = transType TVE TNE defclass

    val (C, cty', preargs) =
      case object of
        (* Instance method or instance field has expression to left of .# *)
        (* This must be cast up to the definition type *)
        (*@FUTURE: avoid cast if usemilclass=defmilclass *)
        SOME (exp as (exploc,_)) =>
        let 
          val (e, cty) = transExp SE TVE EE VE exp
          val (eff, [objty]) = MILTy.fromCmp cty
          val (obj,objv) = freshAnonVar (SOME exploc)
        in
          case MILTy.fromRefty objty of
            SOME (cellty,_) => 
                let val (aobj, aobjv) = freshAnonVar NONE
                    val aty = MILTy.refty(cellty,MILTys.refKind MILTys.Address)
                in
                (fn e' => Let(e,
                              ([(obj, usemilclass)], 
                               Let(Special((Ext.Address 0,NONE,NONE),[objv],MILTy.anyeffect [aty]),
                                  ([(aobj,aty)],
                                    e')))), 
                 MILTy.cmp(eff,[usemilty]),
                 [As(aobjv, defmilclass)])
                end
          | NONE =>
                let fun castReference() =
                        (fn e' => Let(e, ([(obj, usemilclass)], e')), 
                         MILTy.cmp(eff, [usemilty]), 
                         [As(objv, defmilclass)])
                    fun castValue() =
                    let val (aobj, aobjv) = freshAnonVar NONE
                        val aty = MILTy.refty([objty],MILTys.refKind MILTys.Address)
                    in
                        (fn e' => Let(e,
                                      ([(obj, usemilclass)], 
                                       Let(Special((Ext.Prim (Id.fromString "localAlloc"),NONE,NONE),[objv],MILTy.cmp((* Effect.any *) Effect.allocs,[aty])),
                                           ([(aobj,aty)],
                                            e')))), 
                         MILTy.cmp(eff,[usemilty]),
                         [As(aobjv, defmilclass)])
                    end
                in
                case MILTy.fromTyname objty of
                    SOME tn => if not (TyName.isClass tn)
                               then (* it's a value class *)
                                   castValue() 
                               else (* it's a class *)
                                   castReference()
                   | NONE => (* it's a bool or array *)
                             if MILTy.eq(objty,MILTys.bool) 
                             then (* it's the boolean value class *) 
                                  castValue()
                             else (* assume it's an array *)
                                  castReference()
                end
        end

        (* Static method, static field, or constructor *)
      | NONE =>
        (fn e => e, MILTy.noeffect [usemilty], [])
  in
    case (SMLTy.fromFunType usety, SMLTy.fromFunType defty) of
      (* Field access *)
      (NONE, NONE) =>
      let
        val cty = MILTy.noeffect [usemilty]
      in
        if isSome object then 
            (if isSome(SMLTy.fromRefType usety)
                 then (C (Triv([As(hd preargs,usemilty)])),cty') 
             else
                 (C (Special((Ext.GetField,NONE,name), preargs, cty)), cty'))
        else 
        case InterOp.getStaticFields (defclass, valOf name) of
(*@TODO: it would be simpler to inline literals during translation to typed SML terms *)
(*          [(_, SOME c)] => (C (Triv [SCon(usemilty, c)]), cty') *)
           [(_, SOME c)] => 
               if (SMLTy.eq (SMLPrimTy.optionType SMLPrimTy.stringType,usety)) then
                       let val SOME (0,tys) = MILTy.fromMu(usemilty)
                           val sumty = MILTy.unfold(0,tys)
                           val sum = case c of 
                              Constants.NULL => Inj(sumty,1,[],Id.fromString "NONE")
                            | Constants.STRING _ => Inj(sumty,0,[SCon(MILTys.string,c)],Id.fromString "SOME")
                       in
                           (C (Triv[Fold(sum,usemilty)]), cty')
                       end handle _ => Debug.fail "Trans.transExp: error translating literal of type [string option]"
               else
                   (C (Triv [SCon(usemilty, c)]), cty')
        | _ => 
            (if isSome(SMLTy.fromRefType usety)
                 then (C (Triv([As(SCon(MILTy.tyname TyNames.int32TyName,
                                            Constants.INT(RTInt.fromInt 0)),usemilty)])),cty')
             else
              (C (Special((Ext.GetField,SOME defmilclass,name), [], cty)), cty'))
      end

      (* Method/constructor invocation *)
    | (SOME (usearg, useres), SOME (defarg, defres)) =>
      let
        val info = 
          if isSome name 
          then
            (optype,
             if isSome object then NONE else SOME defmilclass, 
             name)
          else
          let
            val derivesDelegateTy = 
              let val delegateTy = SMLTy.consType([],TyNames.delegateTyName)
                  val multicastDelegateTy = SMLTy.consType([],TyNames.multicastDelegateTyName)
            in (InterOp.sub(defres,delegateTy) andalso not(SMLTy.eq(defres,delegateTy))) orelse
               (InterOp.sub(defres,multicastDelegateTy) andalso not (SMLTy.eq(defres,multicastDelegateTy)))
            end
          in
            (if derivesDelegateTy then Ext.NewDelegate else Ext.New, SOME defmilclass, NONE) 
          end

        val e = wrapInvoc (info,preargs) (usemilty,defmilty)
      in
        (C e, cty')
      end
  end

(*----------------------------------------------------------------------*)
(* Let binding                                                          *)
(*----------------------------------------------------------------------*)
| SMLTerm.Let(dec, exp) =>
  letLine loc  
  let
    val (VE', EE', SE', Cx) = transDec SE TVE EE VE dec
    val (ce, cty) = 
      transExp (mergeSE(SE,SE')) TVE (mergeEE(EE,EE')) (merge(VE,VE')) exp
  in
    Cx (ce, cty)
  end

(*----------------------------------------------------------------------*)
(* Exception handler                                                    *)
(*----------------------------------------------------------------------*)
| SMLTerm.Handle (exp,match) =>
  Pat.transHandle 
  { sourcemap = sourcemap, 
    entity = entity, transExp = transExp SE TVE EE, 
    transType = transType TVE TNE, 
    freshAnonVar = freshAnonVar, freshBoundVar=freshBoundVar,
    VE = VE, EE = EE, exp = exp, 
    match = match 
  }

(*----------------------------------------------------------------------*)
(* Raising an exception                                                 *)
(*----------------------------------------------------------------------*)
| SMLTerm.Raise (e, ty, loc) =>
  letLine loc
  let
    val ty = transType TVE TNE ty
    fun eff v = 
    case v of
      ExCon(ty, _) => Effect.throwsAny (* (valOf(MILTy.fromTyname ty)) *)
    | _ => Effect.throwsAny       
  in
    transEval [e] (fn [(v,_)] => 
    (Throw(v, MILTy.cmp(eff v, [ty]), throwMessage (sourcemap, entity, loc)), 
     MILTy.cmp(eff v, [ty])))
  end

(*----------------------------------------------------------------------*)
(* Record construction                                                  *)
(* Field expressions are evaluated from left to right                   *)
(*----------------------------------------------------------------------*)
| SMLTerm.Record fields =>
  let 
    fun evalfld [] vemap = 
        let
          val vlist = map #2 (Id.fixMap vemap)
          val (vs, tys) = ListPair.unzip vlist
        in
          (Triv [Tuple vs], MILTy.cmp(Effect.none, [MILTy.prod tys]))
        end

      | evalfld ((label, e, _) :: rest) vmap =
        transEval [e] (fn [(ve,ty)] =>
        evalfld rest (Map.insert(vmap, label, (ve,ty))))
  in
    evalfld fields Map.empty
  end

| _ =>
  let
    val (ve, ty, funbinds, binds) = transVal SE TVE EE VE [] loce
  in
    (foldr (fn (fundef, e) => LetFun([], AnyFun, Fun fundef, e))
    (foldr (fn ((v, ve), e) => LetVal(v, ve, e))
       (Triv [ve]) binds) funbinds, MILTy.noeffect [ty])
  end
)
end

(*----------------------------------------------------------------------*)
(* Translate an ML declaration item into a MIL computation context.     *)
(*----------------------------------------------------------------------*)
and transDecItem SE TVE EE VE decitem =
case decitem of

(*......................................................................*)
(* Special form for rebinding overloaded identifiers                    *)
(*......................................................................*)
  SMLTerm.Val(loc, [], _, 
    SMLTerm.PatVar(x,_), (varloc,SMLTerm.OverloadedVar(longid, _, []))) =>
  let
    val (Var milvar, ty, bindings) = transLongid false SE VE longid
    val VE' =  Map.singleton(x,((milvar, ty), []))
  in
    ( VE', TyName.Map.empty, Map.empty, 
      fn (rhs,rhscty) => 
        (foldr (fn ((x, v), e) => LetVal(x, v, e)) rhs bindings, rhscty))
  end        
 
(*......................................................................*)
(* Pattern val binding of the form                                      *)
(*    val pat:ty1 = e1                                                  *)
(* If e1 is not valuable (according to the Defn) or pat is refutable    *)
(* (this is our own restriction), then no generalisation is performed.  *)
(*......................................................................*)
| SMLTerm.Val(loc, tyvars, ty, pat, (e as (loce,_))) =>
  let 
    val (x1,xv1) = 
        (*@NOTE: sad attempt to preserve variable names *)
        case getPatVar pat of 
            SOME v => freshBoundVar(SOME loc,v)
          | NONE => freshAnonVar (SOME loc)
  in
    (* No type variables, so rhs and lhs can be non-valuable *)
    if null tyvars 
    then 
    let
      (* List the free variables of the pattern *)
      val fvtys = SMLTermOps.fvPat pat

      (* Generate MIL variables for each of them; they must be in alphabetical
         order, hence foldri not foldli *)
      val (VE', typedfvs) = 
        Map.foldri (fn (v,ty,(VE, typedfvs)) =>
          let
            val milty = transType TVE TNE ty
            val (x,xv) = freshBoundVar (NONE,v)
          in
            (Map.insert(VE, v, ((#1 x, milty), [])), (x,milty)::typedfvs)
          end) (Map.empty, []) fvtys

      (* If none of the patterns match, throw a Bind exception *)
      val (x,xv) = freshBoundVar (NONE,Id.fromString "Bind");
      val miltys = map #2 typedfvs
      val cty = MILTy.cmp(Effect.throwsAny (* TyName.bindExName *), miltys)
      val failterm = 
        (MILTerm.LetVal(x, MILTerm.ExCon(MILTy.exn(Exn.exn [TyNames.bindTyName],[]),[]),
          MILTerm.Throw (xv, cty, throwMessage (sourcemap, entity, loc))),
        cty)

      val (patterm, patcty) = 
        Pat.transLetPat { transExp = transExp SE TVE EE, 
          transType = transType TVE TNE, VE = VE, EE = EE,
          freshAnonVar = freshAnonVar, freshBoundVar=freshBoundVar,
          var = x1, smlty = ty, fail = failterm, pat = pat, loc = loc }
      val (ce, cty) = transExp SE TVE EE VE e
      val (_,[ty]) = MILTy.fromCmp cty
    in
      (
        VE',
        TyName.Map.empty,
        Map.empty, 
        fn (rhs,rhscty) => 
        (Let(ce, ([(x1,ty)], 
           Let(patterm, (typedfvs, rhs)))), 
             MILTy.unionCmpTypes(MILTy.unionCmpTypes(cty, patcty),rhscty))
      )
    end

    (* Otherwise we can generalise the types *)
    else 
    let
      val { bindings = binds2, VE = VE', TVE = TVE', tyvars = tyvars' } = 
        ValPat.trans { freshAnonVar = freshAnonVar, freshBoundVar=freshBoundVar,
                       TVE = TVE, TNE = TNE, tyvars = tyvars, pat = pat,
                       var = x1, smlty = ty }

      val (ve, ty, funbinds, binds1) = transVal SE TVE' EE VE tyvars' e
    in
      (
        VE',
        TyName.Map.empty,
        Map.empty,
        fn (rhs, cty) => 
          (foldr (fn (fundef, e) => LetFun(tyvars', AnyFun, Fun fundef, e))
            (foldr (fn ((x,v),e) => LetVal(x,v,e)) rhs 
              (binds1 @ (x1, 
                MILTermOps.tabs(tyvars', ve)) :: binds2)) funbinds, cty)
      )
    end
  end

(*......................................................................*)
(* SML (potentially-) mutually-recursive letrec construct.              *)
(* We do a dependency analysis here, using fixpoints only where they    *)
(* are required.                                                        *)
(*......................................................................*)
| SMLTerm.ValRec (tyvars, recbinds) =>
  let 
    val funs = foldl (fn ((f,_,_),s) => Set.add(s,f)) Set.empty recbinds
    fun findDeps (f,e,ty) =
      let val uses = Set.intersection(SMLTermOps.fv e, funs)
      in ((f,e,ty), Set.listItems uses) end

    val defs = map findDeps recbinds

    val sccs = rev (Dep.scc (defs, fn ((f,_,_),f') => 
      Symbol.equal(f,f')))

    val (localTVE, miltyvars) = freshTyVars (TVE, tyvars)
    val (VE', Cx) = transValRec (SE, TVE, EE, localTVE, VE, miltyvars, sccs)
  in   
    (VE', TyName.Map.empty, Map.empty, Cx)
  end

(*......................................................................*)
(* Exception definition.                                                *)
(*......................................................................*)
| SMLTerm.Exception exname =>
  let
    val (x,xv) = freshAnonVar NONE
  in
  (
    Map.empty,
    TyName.Map.insert(EE, exname, #1 x),
    Map.empty,    
    fn (ce,cty) => 
      (Let(Special((Ext.Prim Id.exnSym, NONE, NONE), [],
            MILTy.cmp(Effect.allocs, [MILTys.int])),
            ([(x, MILTys.int)], ce)), 
      MILTy.cmpTypePlus (cty, Effect.allocs))
  )
  end

(*......................................................................*)
(* Local declaration                                                    *)
(*......................................................................*)
| SMLTerm.Local(d1, d2) =>
  let
    val (VE1, EE1, SE1, Cx1) = transDec SE TVE EE VE d1
    val (VE2, EE2, SE2, Cx2) = 
      transDec (mergeSE(SE,SE1)) TVE (mergeEE(EE,EE1)) (merge(VE,VE1)) d2
  in
    (VE2, EE2, SE2, Cx1 o Cx2)
  end

(*......................................................................*)
(* Structure binding                                                    *)
(*......................................................................*)
| SMLTerm.Structure(strid, strexp) => 
  let
    val (ce1, cty1) = transStrExp SE VE strexp
    val (x,xv) = freshBoundVar (NONE,strid)
    val (_, [ty]) = MILTy.fromCmp cty1
  in
    (Map.empty, TyName.Map.empty, Map.singleton(strid, (#1 x,ty)), 
    fn (ce2, cty2) => 
      (Let(ce1, ([(x,ty)], ce2)), MILTy.unionCmpTypes(cty1,cty2))
    )
  end
| SMLTerm.And ds =>
  let
    val tr = map (transDecItem SE TVE EE VE) ds
    val res = List.foldl
        (fn ((VEi,EEi,SEi,Cxi),(VE,EE,SE,Cx)) =>
           (merge(VE,VEi),mergeEE(EE,EEi),mergeSE(SE,SEi),Cx o Cxi ))
        (Map.empty,TyName.Map.empty,Map.empty,Gen.identity) 
        tr
  in
    res
  end


(*......................................................................*)
(* Class type definition                                                *)
(*......................................................................*)
| SMLTerm.ClassType 
  { loc,
    tyname, attributes, flags, conattributes, superty, superarg,  (*@TODO: cvr: attributes *)
    interfaces, methods, localdec, argpat, argty } =>
  let

    val isInterface = Symbol.Set.member(flags,Id.interfaceSym)

    val attributes = List.map (transAttExp SE TVE EE VE) attributes
    val conattributes = List.map (transAttExp SE TVE EE VE) conattributes
                        
    val (argpatvar,_) = case getPatVar(argpat) of
                            SOME v => freshBoundVar (NONE,v)
                          | NONE => freshAnonVar NONE

    (* Translate the constructor argument pattern *)
    val { bindings = patbindings, VE = patVE, ... } = 
      ValPat.trans {freshAnonVar = freshAnonVar, freshBoundVar=freshBoundVar, 
                    TVE = TVE, TNE = TNE, tyvars = [], pat = argpat,
                    var = argpatvar, smlty = argty }

    (* The MIL variable used for the "this" argument to methods *)
    val (thisvar,thisvarterm) = freshBoundVar (NONE,Id.thisSym)

    (* Singleton environment containing "this" *)
    val thisVE = Map.insert(Map.empty, Id.thisSym,((#1 thisvar, MILTy.tyname tyname), []))

    (* Translate the local declarations *)
    val (decVE, decEE, decSE, Cx) = 
      transDec SE TVE EE (merge(thisVE, merge(VE,patVE))) localdec

    (* The value bindings that must be turned into fields *)
    val fieldVE = merge(patVE, decVE)

    (* Merge with enclosing context *)
    val EE' = mergeEE(EE, decEE)
    val SE' = mergeSE(SE, decSE)

    fun transMethod {name,attributes,flags,ty,body } =
      let 
        val attributes = List.map (transAttExp SE TVE EE VE) attributes
        val milty = transType TVE TNE ty
        val (argtys, restyopt) = TransInterop.transMethodType milty
        val flags = Set.add(flags, Id.publicSym)

        (* The MIL variable used for the "this" argument to methods *)
        val (thisvar,thisvarterm) = freshBoundVar (NONE,Id.thisSym)

        (* Singleton environment containing "this" *)
        val thisVE = Map.insert(Map.empty, Id.thisSym,((#1 thisvar, MILTy.tyname tyname), []))

        val VE' = merge(VE, merge(thisVE, fieldVE))

        (* Prologue used for all methods *)
        val prologue = 
          Map.foldri (fn (name, ((milvar, milty), _), Ctx) =>
            fn e => Let(
              Special((Ext.GetField, NONE, SOME name), [thisvarterm],
                MILTy.cmp(Effect.reads, [milty])),
                ([((milvar,[]), milty)], Ctx e))) Gen.identity fieldVE


      in
        case body of
          NONE =>
          (name,attributes,flags, argtys, restyopt, NONE)
        | SOME body =>
          let
            val argvars = map (fn _ => #1 (freshAnonVar NONE)) argtys

            val (e,_) = transExp SE' TVE EE' VE' body

            val (f,fv) = freshAnonVar NONE
            val (g,gv) = freshAnonVar NONE

            val applyterm = 
              case argvars of
                [] => App(gv, [Tuple []])
              | [x] => App(gv, [Var (#1 x)])
              | xs => 
                let val (p,pv) = freshAnonVar NONE
                in LetVal(p, Tuple (map (Var o #1) xs), App(gv, [pv])) end

            val body = Let(e, ([(g, milty)],applyterm))
            val body = 
              if isSome restyopt 
              then body
              else Let(body, ([(MILTermOps.dummyBoundVar, MILTys.unit)], Triv []))
          
          in
            (name, attributes, flags, argtys, restyopt, 
              SOME (f, (thisvar::argvars, prologue body)))
          end
      end

    fun transField (name, ((milvar, milty), _)) =
      (* all fields are immutable and private *)
      (name, Set.addList(Set.empty, [Id.initonlySym,Id.privateSym]), milty, NONE)
 
    val superty = transType TVE TNE superty
    val classinfo = (attributes, 
                     (* potentially public; truly public only if exported *)
                     Set.add(flags, Id.publicSym), 
                     if isInterface then NONE else SOME superty, 
                     map (transType TVE TNE) interfaces)
    val methods = map transMethod methods
    val fields = map transField (Map.listItemsi fieldVE)

    val milty = transType TVE TNE argty
    val conargtys = 
      case MILTy.fromProd milty of
        SOME tys => tys
      | NONE => [milty]
    val argvars = 
      case conargtys of
        [_] => [argpatvar]
      | _ => map (fn _ => #1 (freshAnonVar NONE)) conargtys

    val epilogue = 
      Map.foldri (fn (name, ((milvar, milty), _), e) =>
        Let(Special((Ext.PutField, NONE, SOME name), [thisvarterm, Var milvar],
          MILTy.cmp(Effect.writes, [])), ([], e))) (Triv []) fieldVE

    val body = #1 (Cx (epilogue, MILTy.cmp(Effect.any, [])))

    val conname = RuntimeNames.instanceConstructor
    val (superargterm,cty) = transExp SE TVE EE (merge(patVE,VE)) superarg
    val (superargvar,superargvarterm) = freshAnonVar (SOME (#1 superarg))
    val (_,[milty]) = MILTy.fromCmp cty
    val info = (Ext.InvokeSuper, NONE, SOME conname)
    val prearg = As(thisvarterm, superty)
    val invocation = 
      case MILTy.fromProd milty of
        SOME argtys =>
        let
          val ys = map (fn ty => (#1 (freshAnonVar NONE), ty)) argtys
          val n = length ys
        in
          ListOps.foldri (fn (i, (y,ty),e) => LetVal(y, Proj(i, n, superargvarterm), e))
          (Special(info, prearg :: map (fn (y,ty) => Var (#1 y)) ys, 
            MILTy.cmp (Effect.any, [])))
          ys
        end

      | NONE =>
        Special(info, prearg :: [superargvarterm], 
          MILTy.cmp (Effect.any, []))

    val body =
      Let(superargterm, ([(superargvar, milty)], Let(invocation, ([], body))))

    val body =
      foldr (fn ((x,v),e) => LetVal(x,v,e)) body patbindings
    val body = 
      case argvars of
        [] => LetVal(argpatvar, Tuple [], body)
      | [_] => body
      | xs => LetVal(argpatvar, Tuple (map (Var o #1) xs), body)


    val isDelegate = case MILTy.fromTyname superty of
          SOME supertn => TyName.eq(supertn,TyNames.multicastDelegateTyName) orelse TyName.eq(supertn,TyNames.delegateTyName)
        | _ => false

    val constructormethods = 
        if isInterface 
        then []
        else if isDelegate
        then [(conname,conattributes, Symbol.Set.add(Symbol.Set.empty, Id.publicSym),  
               [MILTy.tyname TyNames.objectTyName, MILTy.tyname TyNames.nativeIntTyName], NONE,NONE)]
        else (* a proper class, with a constructor *)
             [(conname,conattributes, Symbol.Set.add(Symbol.Set.empty, Id.publicSym), 
               conargtys, NONE, 
               SOME (#1 (freshAnonVar NONE), (thisvar::argvars, body)))]


  in        
    (Map.empty, TyName.Map.empty, Map.empty, 
      fn (ce2, cty) => 
      (LetClass(MILTy.tyname tyname, classinfo, fields, 
        constructormethods@methods, ce2), 
        MILTy.cmpTypePlus(cty, Effect.io)))
  end

(*----------------------------------------------------------------------*)
(* Translate an ML declaration into a MIL computation context.          *)
(*----------------------------------------------------------------------*)
and transDec SE TVE EE VE dec =
case dec of
  []    => 
  (Map.empty, TyName.Map.empty, Map.empty, Gen.identity)

| d::ds =>
  let
    val (VE1, EE1, SE1, Cx1) = transDecItem SE TVE EE VE d
    val (VE2, EE2, SE2, Cx2) = 
      transDec (mergeSE(SE,SE1)) TVE (mergeEE(EE, EE1)) (merge(VE,VE1)) ds
  in
    (merge(VE1, VE2), mergeEE(EE1, EE2), mergeSE(SE1, SE2), Cx1 o Cx2)
  end

(*----------------------------------------------------------------------*)
(* Translate a structure expression into a computation term.            *)
(*----------------------------------------------------------------------*)
and transStrExp SE VE exp = 
case exp of

  SMLTerm.Struct(loc,vals, strs) =>  
  transEvalSansLoc (transStrExp SE VE) (map #2 (Id.fixMap strs)) (fn strs =>
  let
    val (strvs, strtys) = ListPair.unzip strs
    val (valvs, valtys) = 
      ListPair.unzip (map (transStrBind VE) (map #2 (Id.fixMap vals)))
  in
  (
    Triv [Tuple(strvs @ valvs)], 
    MILTy.cmp(Effect.none, [MILTy.prod(strtys @ valtys)])
  )
  end)


| SMLTerm.Strid (strid,rest) =>
  let
    val (v, ty, bindings) = transLongStrid SE (strid, rest)
  in
    (foldr (fn ((x,v),e) => LetVal(x,v,e)) (Triv [v]) bindings,
     MILTy.cmp(Effect.none, [ty]))
  end

| SMLTerm.StrLet(strdec, strexp) =>
  let 
    val (VE', _, SE', Cx) = 
      transDec SE TyVar.Map.empty TyName.Map.empty VE strdec
    val (ce,cty) = transStrExp (mergeSE(SE,SE')) (merge(VE,VE')) strexp
  in
    Cx (ce,cty)
  end

| SMLTerm.StrInlined(sourcemap, strexp) =>
  withSource sourcemap (transStrExp SE VE) strexp


and transStrBind VE id =
  case Map.find(VE, id) of
    SOME ((x, milty), _) => (Var x, milty)
  | _ => Debug.fail "Trans.transStrBind: error"

(*----------------------------------------------------------------------*)
(* Translate a val rec binding.                                         *)
(* The type variables have already been translated.                     *)
(* This is complicated because we want to generate let's not letrec's   *)
(* where possible.                                                      *)
(*----------------------------------------------------------------------*)
and transValRec (SE, TVE, EE, defnTVE, VE, miltyvars, sccs) =
  case sccs of      
    [] =>
    (Map.empty, Gen.identity)

  (*..................................................................*)
  (* A recursive set of bindings.                                     *)
  (*..................................................................*)
  | (Dep.Rec scc :: sccs) =>
    let 
      (* The function variables from the SML term *)
      val funvars = map (fn (f,_,ty) => (f, transType defnTVE TNE ty)) scc

      (* Function variables for the body of the MIL letrec *)
      val (bodyVE, bodyfunvars) = freshPolyVars (Map.empty, funvars, miltyvars)

      (* Function variables for the definitions in the MIL letrec *)
      val (defnVE, defnfunvars) = freshVars (VE, funvars)

      (* Translate the other scc's *)
      val (resultVE, Cx) = transValRec (SE, TVE, EE, 
        defnTVE, merge(VE,bodyVE), miltyvars, sccs)

      val defs = map
        (fn ((bodyfunvar, defnfunvar), (_, (fnloc,SMLTerm.Fn (smlty,match)), funty)) =>
        let 
          val ty = transType defnTVE TNE funty
          val (tabs, cty) = 
            Pat.transFn { sourcemap = sourcemap,
              entity = entity, transExp = transExp SE defnTVE EE, 
              transType = transType defnTVE TNE, 
              freshAnonVar = freshAnonVar, freshBoundVar=freshBoundVar,
              VE = defnVE, EE = EE,
              smlty = smlty, match = match }
        in 
          (bodyfunvar, defnfunvar, tabs, cty)
        end) (ListPair.zip(ListPair.zip(bodyfunvars, defnfunvars), scc))
    in
      (
        merge(bodyVE, resultVE),
        fn (ce,cty) => 
        let val (ce,cty) = Cx (ce,cty)
        in (LetFun(miltyvars, AnyFun, RecFun defs, ce), cty) end
      )
    end

  (*..................................................................*)
  (* A non-recursive binding.                                         *)
  (*..................................................................*)
  | (Dep.NonRec (funvar, (fnloc,SMLTerm.Fn (smlty,match)), funty) :: sccs) =>
    let
      (* Variable for the body of the MIL let *)
      val (bodyVE, [bodyfunvar]) = freshPolyVars (Map.empty, [(funvar,
        transType defnTVE TNE funty)], miltyvars)

      (* Translate the other scc's *)
      val (resultVE, Cx) = transValRec 
          (SE, TVE, EE, defnTVE, merge(VE,bodyVE), miltyvars, sccs)
      (* Translate the definition *)
      val (tabs, _) = 
        Pat.transFn { sourcemap = sourcemap, entity = entity,
                      transExp = transExp SE defnTVE EE, transType = transType defnTVE TNE,
                      freshAnonVar = freshAnonVar, freshBoundVar=freshBoundVar,
                     VE = VE, EE = EE, smlty = smlty, match = match }
    in
    (
      merge(bodyVE, resultVE),
      fn (ce,cty) => 
      let
        val (ce,cty) = Cx (ce,cty)
      in (LetFun(miltyvars, AnyFun, Fun (bodyfunvar, tabs), ce), cty) end
    )  
    end
(*@TODO: cvr: deserves a separate file *)
and transAttExp   
    (SE : TransOps.StrEnv)
    (TVE : MILTy.Type TyVar.Map.map)               
    (EE : TransOps.ExEnv)
    (VE : TransOps.ValEnv)
    (SMLTerm.AttApp(loc,(exploc,exp),exprow))
    = 
     case exp of 
         SMLTerm.App((invocloc,SMLTerm.Invoc{defty=(defclass,defty),name=NONE,object=NONE,optype=Ext.Invoke,...}),
             arg) => 
         let 
             val (defargtys,_) = InterOp.unpackMethTy defty
             val defmilargtys = map (transType TVE TNE) defargtys

             val defmilclass = transType TVE TNE defclass
             val prolog = [0wx1,0wx0]:Word8.word list 
             (*@TODO: prolog is a  magic number --- should be defined as a constant somewhere *)
             val trAnonArg = transAttAnonArg SE TVE EE VE loc arg
             val trNamedArgs = transAttNamedArgs SE TVE EE VE loc exprow
             val bytes = Word8Vector.fromList(prolog@trAnonArg@trNamedArgs)
         in  (defmilargtys,defmilclass,bytes)
         end
   | _ => raise Match (*@TODO: error not a constructor app *)
and transAttAnonArg
    (SE : TransOps.StrEnv)
    (TVE : MILTy.Type TyVar.Map.map)               
    (EE : TransOps.ExEnv)
    (VE : TransOps.ValEnv) 
    loc  (locarg as (argloc,arg)) =
    case arg of 
      SMLTerm.Record symexplist => (* n-ary arg, for n=0 or n>1 *)
          List.concat (map (fn (lab,exp,_) => transAttArg SE TVE EE VE loc exp) symexplist)   
    | _ =>(* unary arg n = 1 *) 
          transAttArg SE TVE EE VE loc locarg
and transAttNamedArgs 
  (SE : TransOps.StrEnv)
  (TVE : MILTy.Type TyVar.Map.map)               
  (EE : TransOps.ExEnv)
  (VE : TransOps.ValEnv)
  loc exprow = 
  let val numNamed = List.length exprow
      val a = RTInt.ju2(RTInt.fromInt numNamed)
  in
     List.concat ([Word8Vector.sub(a,1),Word8Vector.sub(a,0)]::(map (transAttNamedArg SE TVE EE VE loc) exprow))
  end
and transAttNamedArg (SE : TransOps.StrEnv)
                     (TVE : MILTy.Type TyVar.Map.map)               
                     (EE : TransOps.ExEnv)
                     (VE : TransOps.ValEnv)
                     loc (fieldorproperty,arg,ty) =
  let
      val SERIALIZATION_TYPE_BOOLEAN = 0wx2:Word8.word
      val SERIALIZATION_TYPE_CHAR = 0wx3:Word8.word
(*    val SERIALIZATION_TYPE_I1 = 0wx4; *) (* not required for CLS *) 
      val SERIALIZATION_TYPE_U1 = 0wx5:Word8.word 
      val SERIALIZATION_TYPE_I2 = 0wx6:Word8.word
(*    val SERIALIZATION_TYPE_U2 = 0wx7; *) (* not required for CLS *) 
      val SERIALIZATION_TYPE_I4 = 0wx8:Word8.word
(*    val SERIALIZATION_TYPE_U4 = 0wx9; *) (* not required for CLS *) 
      val SERIALIZATION_TYPE_I8 = 0wxa:Word8.word
(*    val SERIALIZATION_TYPE_U8 = 0wxb; *) (* not required for CLS *) 
      val SERIALIZATION_TYPE_R4 = 0wxc:Word8.word
      val SERIALIZATION_TYPE_R8 = 0wxd:Word8.word
      val SERIALIZATION_TYPE_STRING = 0wxe:Word8.word
      val SERIALIZATION_TYPE_FIELD = 0wx53:Word8.word
      val SERIALIZATION_TYPE_PROPERTY = 0wx54:Word8.word
      val SERIALIZATION_TYPE_ENUM = 0wx55:Word8.word
      val serTypOrENUMSerTyp = 
         (*TODO if ty is enum then ENUM::underlyingsertype *)
          if SMLTy.eq(ty,SMLPrimTy.boolType) then SOME SERIALIZATION_TYPE_BOOLEAN else
          if SMLTy.eq(ty,SMLPrimTy.charType) then SOME SERIALIZATION_TYPE_CHAR else
(*        if SMLTy.eq(ty,SMLPrimTy.int8Type) then SOME SERIALIZATION_TYPE_I1 else *) (* not required for CLS *)
          if SMLTy.eq(ty,SMLPrimTy.word8Type) then SOME SERIALIZATION_TYPE_U1 else
          if SMLTy.eq(ty,SMLPrimTy.int16Type) then SOME SERIALIZATION_TYPE_I2 else 
(*        if SMLTy.eq(ty,SMLPrimTy.word16Type) then SOME SERIALIZATION_TYPE_U2 else *) (* not required for CLS *) 
          if SMLTy.eq(ty,SMLPrimTy.intType) then SOME SERIALIZATION_TYPE_I4 else
(*        if SMLTy.eq(ty,SMLPrimTy.wordType) then SOME SERIALIZATION_TYPE_U4 else *) (* not required for CLS *)
          if SMLTy.eq(ty,SMLPrimTy.int64Type) then SOME SERIALIZATION_TYPE_I8 else
(*        if SMLTy.eq(ty,SMLPrimTy.word64Type) then SOME SERIALIZATION_TYPE_U8 else *) (* not required for CLS *)
          if SMLTy.eq(ty,SMLPrimTy.real32Type) then SOME SERIALIZATION_TYPE_R4 else
          if SMLTy.eq(ty,SMLPrimTy.realType) then SOME SERIALIZATION_TYPE_R8 else
          if SMLTy.eq(ty,SMLPrimTy.stringType) then SOME SERIALIZATION_TYPE_STRING else
          if SMLTy.eq(ty,SMLPrimTy.optionType SMLPrimTy.stringType) then SOME SERIALIZATION_TYPE_STRING else
(*@TODO   if SMLTy.eq(ty,SMLPrimTy.SystemType) then SOME SERIALIZATION_TYPE_TYPE else
          if SMLTy.eq(ty,SMLPrimTy.optionType SystemType) then SOME SERIALIZATION_TYPE_TYPE else
*)
          (addError(Error.error(loc, "invalid type of attribute argument"));
           NONE)
      val (symbol,FIELDorPROPERTY) = 
          case fieldorproperty of 
              SMLTerm.Field symbol => (symbol,SERIALIZATION_TYPE_FIELD) 
            | SMLTerm.Property symbol => (symbol,SERIALIZATION_TYPE_PROPERTY)
      val name = UString.packWithPackedLen.pack (Symbol.toUString symbol)
      val value = transAttArg SE TVE EE VE loc arg
  in
      case serTypOrENUMSerTyp of 
           SOME serTypOrENUMSerTyp =>
               (FIELDorPROPERTY 
                (*@TODO ::VARIANT  what is this?*)
                :: serTypOrENUMSerTyp   
                :: Word8Vector.foldr (op ::) value name)
         | NONE => []
  end
and transAttArg (SE : TransOps.StrEnv)
                (TVE : MILTy.Type TyVar.Map.map)               
                (EE : TransOps.ExEnv)
                (VE : TransOps.ValEnv)
                loc (locattarg,attarg) = 
     case attarg of 
         (*@TODO: allow values of enum type *)
         (*@TODO: allow values of System.Type *)
           SMLTerm.SCon(scon, ty, loc) => 
               let 
                   val ty' = transType TVE TNE ty
                   val bytes = 
                       case TransSCon.trans (scon, ty') of
                           NONE => 
                               (addError(Error.error(loc, "constant too large"));
                                [])
                         | SOME jcon => 
                               transConstant (jcon,loc)
               in
                   bytes
               end
         (* Variables specialised at a particular types *)
         | SMLTerm.Var (longid,tys) =>  
               (addError(Error.error(loc, "invalid attribute argument expression"));
                []) 
         (* Overloaded variable with given overloaded sort specialised at given types *)
         | SMLTerm.OverloadedVar (longid,tynameset,tys) => 
               (addError(Error.error(loc, "invalid attribute argument expression"));
                [])
         (* Constructors specialised at a particular type, with constructor env *)
         | SMLTerm.Con (con,datdef as (_,tyname,_),[]) => 
               if TyName.eq(tyname,TyNames.boolTyName) 
                   then (if Symbol.equal(con,Id.falseSym)
                             then [0w0] (*@TODO: should be a constant *)
                         else if Symbol.equal(con,Id.trueSym)
                                  then [0w1] (*@TODO: should be a constant *)
                              else Debug.fail "Trans.TransAttArg: expected boolean constructor")
               else let val ty = SMLTy.consType([],tyname) 
                    in case InterOp.isEnumType ty  of
                         SOME _ =>
                             (case InterOp.getStaticFields (ty, con) of
                                  [(_, SOME jcon)] => 
                                     transConstant (jcon,loc)
                                | _ => Debug.fail "Trans.transAttArg: bad enum constant")
                       | NONE =>
                             (addError(Error.error(loc, "invalid type of attribute argument"));
                              [0w0])
                    end
         | SMLTerm.Con (con,datdef as (_,tyname,_),[ty]) => 
                   if TyName.eq(tyname,TyNames.optionTyName) 
                       andalso (SMLTy.eq(ty,SMLPrimTy.stringType) 
                                (*@TODO: orelse SMLTy.eq(ty,SMLTy.SystemType) *)
                                )
                       then (* con must be NONE *) 
                           [0wxff] (*@TODO; should be a constant *)
                   else (addError(Error.error(loc, "invalid type of attribute argument"));
                         [0w0])
         (* Function application *)
         | SMLTerm.App((funcloc,func),arg) => 
           (case func of
                SMLTerm.Con (con,datdef as (_,tyname,_),[]) => (* no type args, only valid if its of enum type *)
                    let val ty = SMLTy.consType([],tyname) 
                    in case InterOp.isEnumType ty of
                         SOME _ => transAttArg SE TVE EE VE loc arg
                       | NONE => (addError(Error.error(loc, "invalid type of attribute argument (enum constructor expected)"));
                                    [])
                    end
               | SMLTerm.Con (con,datdef as (_,tyname,_),[ty]) => (* one type arg, only valid if its of option type *)
                    if TyName.eq(tyname,TyNames.optionTyName) 
                        andalso (SMLTy.eq(ty,SMLPrimTy.stringType) 
                                 (*@TODO: orelse SMLTy.eq(ty,SMLTy.SystemType) *)
                                 )
                    then (* con must be SOME *)
                         transAttArg SE TVE EE VE loc arg
                    else (addError(Error.error(loc, "invalid type of attribute argument (string option expected)"));
                          [])
               | _ => (addError(Error.error(loc, "expected constructed argument of option type or enum type"));
                       []))
         | _ =>
           (addError(Error.error(loc, "invalid attribute argument expression"));
            [])
(*         SMLTerm.ExCon (tyname,tyopt) => 
         | SMLTerm.Fn _ => 
         | SMLTerm.Let _ => 
         | SMLTerm.Handle _ => 
         | SMLTerm.Raise _ => 
         | SMLTerm.Record symexplist =>
         | SMLTerm.Invoc {usety,defty,name,object,optype} => 
         | SMLTerm.Special ((optype,tyopt,symopt),explist,tyopt',eff)=> 
*)
and transConstant (c,loc) = 
   (*@TODO: crusso: it would be more efficient (and simpler) to produce a list of 
     Word8Vectors (of varying lengths, than a list of Word8's *)
    case c of 
      Constants.BOOLEAN ji => [RTInt.ju1 ji]
    | Constants.BYTE ji => [RTInt.ju1 ji]
    | Constants.CHAR ji => 
          let val a = RTInt.ji2 ji
          in
              [Word8Vector.sub(a,1),Word8Vector.sub(a,0)]
          end
    | Constants.SHORT ji => 
          let val a = RTInt.ji2 ji
          in
              [Word8Vector.sub(a,1),Word8Vector.sub(a,0)]
          end
    | Constants.INT ji => 
          let val a = RTInt.ji4 ji
          in
              [Word8Vector.sub(a,3),Word8Vector.sub(a,2),
               Word8Vector.sub(a,1),Word8Vector.sub(a,0)]
          end
    | Constants.LONG jl => 
          let val a = RTLong.pack.pack jl
          in
              [Word8Vector.sub(a,7),Word8Vector.sub(a,6),
               Word8Vector.sub(a,5),Word8Vector.sub(a,4),
               Word8Vector.sub(a,3),Word8Vector.sub(a,2),
               Word8Vector.sub(a,1),Word8Vector.sub(a,0)]
          end
    | Constants.FLOAT jf =>
          let val a = RTFloat.pack.pack jf
          in
              [Word8Vector.sub(a,3),Word8Vector.sub(a,2),
               Word8Vector.sub(a,1),Word8Vector.sub(a,0)]
          end
    | Constants.DOUBLE jd =>
          let val a = RTDouble.pack.pack jd
          in
             (addError(Error.warning(loc, "this Real64.real may not have been serialised correctly due to a compiler bug")); 
              [Word8Vector.sub(a,7),Word8Vector.sub(a,6),
               Word8Vector.sub(a,5),Word8Vector.sub(a,4),
               Word8Vector.sub(a,3),Word8Vector.sub(a,2),
               Word8Vector.sub(a,1),Word8Vector.sub(a,0)])
          end
    | Constants.STRING s =>
          let val a = UString.packWithPackedLen.pack s  
          in Word8Vector.foldr (op ::) [] a 
          end
    | Constants.NULL =>
         Debug.fail "Trans.transConstant: unexpected NULL constant"

val _ = TransOps.initialize supply

val restoreAnonVarName = MILTermOps.setAnonVarName([Id.fromString "!Trans"])
val (e,cty) = transStrExp SE Map.empty strexp
val _ = restoreAnonVarName()
in
  { 
    term = e, 
    cty = cty, 
    errors = TransOps.getErrors(), 
    varsupply = TransOps.getSupply(), 
    tyvarsupply = TransOps.getTyVarSupply()
  }
end
    

end (* of local open *)
end (* of struct *)

















