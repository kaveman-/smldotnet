(*======================================================================*)
(* Typed simplification of (possibly non-atomic) MIL value terms.	*)
(*======================================================================*)
structure SimplifyVal :> SIMPLIFYVAL =
struct

local 
  open MILTerm MILPretty SimplifyEnv
in

  val [prodBeta, prodEta, constValOp] = 
      map (Controls.add true) 
      ["prodBeta", "prodEta", "constValOp"]

(*----------------------------------------------------------------------*)
(* Recursively apply simplify to the subterms of v, then apply          *)
(* rewrites to the resulting term. For variables, apply the             *)
(* substitution from env. Likewise for types.                           *)
(*----------------------------------------------------------------------*)
fun simplify (env : SimplifyEnv.Env) (v : MILTerm.Val) = 
let
  fun common (v,ty) =
    case SimplifyEnv.lookupCommon (env, v) of
      SOME x =>
      if Controls.enabled SimplifyEnv.commonVal
      then
        (Census.addVal(v, ~1); Census.addVar(x, 1);
        (Var x, SimplifyEnv.lookupVarType (env, x)))
      else (v,ty)

    | NONE => 
      (v,ty)
in
  case v of

(*......................................................................*)
(* Sum introduction							*)
(* Note: vs may not be atoms if the injection is "free".		*)
(*......................................................................*)
  Inj(ty, i, vs, si) =>
  common (Inj(ty, i, map (#1 o simplify env) vs,si), ty)

(*......................................................................*)
(* Exception introduction						*)
(*......................................................................*)
| ExCon(exname, vs) =>
  common (ExCon(exname, map (#1 o SimplifyAtom.simplify env) vs),MILTys.topExn)

| Fold(v, ty) =>
  common (Fold(#1 (simplify env v), ty), ty)

(*......................................................................*)
(* Quantifier introduction						*)
(*                                                                      *)
(* forall-eta:								*)
(*    Fn (t_1,...,t_n) => v {t_1,...,t_n}                               *)
(*       -->   v    (if t_1,...,t_n) not free in v)                     *)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val (v, ty) = simplify (SimplifyEnv.envPlusTyVars (env, tyvars)) v
    fun default () = common (TAbs(tyvars, v), MILTy.forall(tyvars, ty))
    fun applyTAbsCC (tyvars, v) =
    case v of
      TApp(polyv, tys) =>
      let
      fun checkArgs [] [] = 
          if Controls.enabled SimplifyAtom.forallEta 
          then (polyv, MILTy.forall(tyvars, ty))
          else default ()

        | checkArgs ((tyvar,k)::tyvars) (ty :: tys) = 
          (case MILTy.fromTyvar ty of
            SOME tyvar' =>
            if Var.eq(tyvar,tyvar') andalso 
              not (MILTermOps.tyVarOccursVal tyvar polyv)
            then checkArgs tyvars tys
            else default ()
          | _ => default ())

        | checkArgs _ _ = 
          default ()
      in
        checkArgs tyvars tys
      end

    | _ => default ()
  in
    applyTAbsCC (tyvars, v)
  end

(*......................................................................*)
(* Product introduction							*)
(*                                                                      *)
(* prod-eta:								*)
(*    (#1 v, ..., #n v) --> v     					*)
(*......................................................................*)
| Tuple vs =>
  let 
    val (vs, tys) = SimplifyAtom.simplifyVec env vs
    val n = length vs
    fun default () = common (Tuple vs, MILTy.prod tys)
  in
    case vs of
      v as (Var firstx) :: _ =>
      let
        fun good (prodx,v,i) =
           case SimplifyEnv.lookupCommon (env, Proj(i,n,v)) of
              SOME prodx' => Var.eq(prodx,prodx')
            | _ => false

        fun test (prodx, [], i) = 
            if Controls.enabled prodEta
            then 
              (Census.addVar (prodx, 1);
               app (fn v => Census.addVal(v, ~1));
               (Var prodx, MILTy.prod tys))
            else default ()

          | test (prodx, v::vs, i) =
            if good (prodx, v, i) then test (prodx, vs, i+1)
            else default ()

      in
        case SimplifyEnv.lookupCommon (env, Proj(0, n, Var firstx)) of
          SOME prodx =>
          let
            val ty = SimplifyEnv.lookupVarType (env, prodx)
          in
            case MILTy.fromProd ty of
              NONE => default ()
            | SOME tys' =>
              if length tys = length tys' 
              then test (prodx, vs, 0)
              else default ()
          end

        | _ => default ()
      end

    | _ => 
      default ()
  end
    
(*......................................................................*)
(* Product elimination							*)
(*                                                                      *)
(* prod-beta:							        *)
(*     #i (v_1, ..., v_i, ..., v_n) --> v_i				*)
(*......................................................................*)
| projv as Proj(i, n, v) =>
  let
    val (v, ty) = SimplifyAtom.simplify env v
    val tys = 
      case MILTy.fromProdCon ty of
        SOME tys => tys
      | NONE => 
        failVal v "SimplifyVal.simplify: expected product/constructor type"

    val resultty = (List.nth(tys, i)) handle Subscript =>
      failVal projv "SimplifyVal.simplify: out of range projection"

    fun default () = common (Proj(i, n, v), resultty)
  in
    case SimplifyEnv.lookupBinding (env, v) of
      Tuple vs => 
      let 
        val v' = List.nth(vs, i)
      in
        if Controls.enabled prodBeta
        then
        (
          Census.addVal (v, ~1); Census.addVal(v', 1);
          (v', resultty)
        )
        else default ()
      end

    | _ => default ()
  end

| _ =>
  SimplifyAtom.simplify env v

end

and simplifyVec env [] = ([], [])
  | simplifyVec env (v::vs) =
    let
      val (v,ty) = simplify env v
      val (vs,tys) = simplifyVec env vs
    in
      (v::vs, ty::tys)
    end

end (* of local open *)
        
end (* of struct *)
