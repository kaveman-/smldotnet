(*======================================================================*)
(* Typed simplification of MIL atomic value terms.         		*)
(* Atoms always simplify to atoms.                                      *)
(*======================================================================*)
structure SimplifyAtom :> SIMPLIFYATOM =
struct

local 
  open MILTerm MILPretty SimplifyEnv
in

  val [coerceId, coerceComp, forallBeta, forallEta, bindBeta, unfoldFold] =
  map (Controls.add true)
  ["coerceId","coerceComp","forallBeta","forallEta","bindBeta","unfoldFold"]

(*----------------------------------------------------------------------*)
(* Recursively apply simplify to the subterms of v, then apply          *)
(* rewrites to the resulting term. For variables, apply the             *)
(* substitution from env. Likewise for types.                           *)
(*----------------------------------------------------------------------*)
fun simplify (env : SimplifyEnv.Env) (v : MILTerm.Val) = 
let
  val sv = simplify
  val svs = simplifyVec
  fun sv' env ve = #1 (sv env ve)
  val st = simplifyType env
in
  case v of

(*......................................................................*)
(* For variables look up (atomic) substitution.                         *)
(*......................................................................*)
  Var x => 
  let
    val (ty, vopt) = SimplifyEnv.lookupVarVal (env, x)
  in
    case vopt of
      SOME v' =>
      if Controls.enabled bindBeta then 
        (Census.addVar (x, ~1); Census.addVal (v', 1); (v', ty))
      else 
        (v, ty)

    | NONE => (v, ty)
  end

(*......................................................................*)
(* Constants: do nothing						*)
(*......................................................................*)
| SCon(ty, jcon) => 
  (v, st ty)

(*......................................................................*)
(* Sum introduction							*)
(*......................................................................*)
| Inj(ty, i, vs, si) =>
  (Inj(st ty, i, map (sv' env) vs, si), ty)

(*......................................................................*)
(* Product introduction							*)
(*......................................................................*)
| Tuple [] =>
  (Tuple [], MILTy.prod [])
    
(*......................................................................*)
(* Quantifier introduction						*)
(*                                                                      *)
(* forall-eta:								*)
(*    Fn (t_1,...,t_n) => v {t_1,...,t_n}                               *)
(*       -->   v    (if t_1,...,t_n) not free in v)                     *)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val (v, ty) = sv (SimplifyEnv.envPlusTyVars (env, tyvars)) v
    fun default () = (TAbs(tyvars, v), MILTy.forall(tyvars, ty))
    fun applyTAbsCC (tyvars, v) =
    case v of
      TApp(polyv, tys) =>
      let
      fun checkArgs [] [] = 
          if Controls.enabled forallEta 
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
(* Mu introduction							*)
(*......................................................................*)
| Fold(v, ty) =>
  (Fold(sv' env v, st ty), ty)

(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold v =>
  let 
    val (v', ty) = sv env v
    val a =
      case MILTy.fromMu ty of
        SOME a => a

      | _ => 
        failVal v ("SimplifyAtom.simplify: expected recursive type, got " ^
        MILTy.toString ty)
    fun default () =  (Unfold v', MILTy.unfold a)
  in
    case SimplifyEnv.lookupBinding (env, v') of
      Fold(v'',_) => 
      if Controls.enabled unfoldFold 
      then (Census.addVal (v',~1); Census.addVal(v'',1); (v'', MILTy.unfold a))
      else default ()
    | _ => default ()
  end

(*......................................................................*)
(* Quantifier elimination                          			*)
(*                                                                      *)
(* forall-beta:                                                         *)
(*     (Fn (t_1,...,t_n) => v) { ty_1, ..., ty_n }                      *)
(* --> v [ty_1/t_1, ..., ty_n/t_n]                                      *)
(*......................................................................*)
| TApp(v, tys) =>
  let
    val (v, polyty) = sv env v
    val a as (tyvars, ty) = 
      case MILTy.fromForall polyty of
        SOME a => a
      | _ => failVal v "SimplifyVal.simplify: expected polymorphic type"
    fun default () = 
      (TApp(v, map st tys), MILTy.app (MILTy.abs a, tys))
  in
    case v of 
      TAbs(tyvars, v') =>
      if Controls.enabled forallBeta  
      then sv (envPlusTySubst (env, tyvars, tys)) v'
      else default ()

    | _ => default ()
  end

(*......................................................................*)
(* runtime coercion							*)
(*                                                                      *)
(* coerce-comp:							        *)
(* coerce(coerce(v, ty1), ty2) --> coerce(v, ty2)                       *)
(*                                                                      *)
(* coerce-id:                                                           *)
(* coerce(v:ty, ty) --> v                                               *)
(*......................................................................*)
| As(v, ty) =>
  let
    val (v, fromty) = sv env v
    fun default () = (As(v, st ty), ty)
  in
    if MILTy.eq (fromty,ty) andalso Controls.enabled coerceId
    then (v, ty)
    else
      case SimplifyEnv.lookupBinding (env,v) of
        As(v', _) =>
        if Controls.enabled coerceComp
        then 
          (Census.addVal (v,~1); Census.addVal (v',1);
          (As(v', st ty), ty))
        else default ()

      | _ => default ()
  end

| _ =>
  failVal v "SimplifyAtom.simplify: illegal atomic value term"
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
