(*======================================================================*)
(* Translation of SML types and environments to MIL.			*)
(*======================================================================*)
structure TransType :> TRANSTYPE =
struct

(*----------------------------------------------------------------------*)
(* Translate a type name application					*)
(*----------------------------------------------------------------------*)
fun transCon TNE (tyname, tys) =
  let
    val ty = getOpt(TyName.Map.find(TNE, tyname), MILTy.tyname tyname)
  in
    MILTy.app (ty, tys)
  end

(*----------------------------------------------------------------------*)
(* Translate an SML type into a MIL value type				*)
(* TVE maps SML type variables to MIL type variables.                   *)
(*----------------------------------------------------------------------*)
fun transType 
  (TVE : MILTy.Type TyVar.Map.map) 
  (TNE : MILTy.Type TyName.Map.map) 
  ty =
let 
  open SMLTy
  fun t ty =
  case proj ty of 
(*......................................................................*)
(* If the type variable is not in the environment (i.e. was not bound   *)
(* by a let or letrec) then it must be `internal' and therefore can be  *)
(* instantiated to anything allowed by its sort.                        *)
(*......................................................................*)
    Var(ref (TyVar tyvar)) => 
    (
      case TyVar.Map.find(TVE, tyvar) of
        NONE => 
        (case TyVar.sort tyvar of
          TyVar.Normal s => 
          MILTys.unit

        | TyVar.Overloaded tynames => 
          transType TVE TNE (SMLTy.baseType (TyNames.default tynames))
        )

      | SOME ty => 
        ty
    )

(*......................................................................*)
(* ML base type names are translated explicitly to MIL base types.	*)
(*......................................................................*)
  | Con(tyname, tys) => 
    if TyName.eq(tyname, TyNames.boolTyName)
    then MILTys.bool
    else
    if TyName.eq(tyname, TyNames.vectorTyName)
    then MILTy.vector (t (hd tys))
    else transCon TNE (tyname, map t tys)

  | Class(MLClass { tyname, ... }) =>
    transCon TNE (tyname, [])

(*......................................................................*)
(* Function types are translated to one-argument-one-result function 	*)
(* types in MIL, with conservative effect info.                         *)
(*......................................................................*)
  | Fun(ty1,ty2) => 
    MILTy.arrow([t ty1], MILTy.cmp(Effect.any, [t ty2]))

(*......................................................................*)
(* Reference types translate straight.					*)
(*......................................................................*)
  | Ref (ty1,ty2) => 
    let val t2 = t ty2 
    in
          MILTy.refty ([t ty1],t ty2)
    end


(*......................................................................*)
(* Array types translate straight.                                      *)
(*......................................................................*)
  | Array ty => MILTy.array (t ty)
    
(*......................................................................*)
(* Record types are translated to products, thus losing the distinction	*)
(* between record types with the same component types but different     *)
(* field labels (ordered alphabetically).                               *)
(*......................................................................*)
  | Rec(ref (fixed,_)) =>
    let
      val fixed = Id.fixMap fixed
    in
      MILTy.prod (map (fn (l,ty) => t ty) fixed)
    end
in
  t ty
end

(*----------------------------------------------------------------------*)
(* Translate a closed SML type scheme					*)
(*----------------------------------------------------------------------*)
fun transOverloaded TNE ([tyvar],ty) = 
  (case TyVar.sort tyvar of
    TyVar.Overloaded tynames =>
    SOME (MILTy.prod (map (fn tyname => 
          transType TyVar.Map.empty TNE
          (SMLTy.appSubst [(tyvar, SMLTy.consType([], tyname))] ty))
          (TyName.Set.listItems tynames)))
  | _ => NONE)

  | transOverloaded TNE _ = NONE

fun transScheme TNE (SMLSch.TypeScheme(sch as (tyvars, ty))) =
case transOverloaded TNE sch of
  SOME ty => ty
| NONE =>
  let
    val (TVE, kinds) = TransOps.freshDebTyVars 0 tyvars
  in
    MILTy.debforall (kinds, transType TVE TNE ty)
  end

(*----------------------------------------------------------------------*)
(* Translate a signature realisation into a map from tynames to types   *)
(*----------------------------------------------------------------------*)
fun transRealisation TNE psi =
  TyName.Map.map (fn (tyvars,ty) =>
    let
      val (TVE, kinds) = TransOps.freshDebTyVars 0 tyvars
      val ty' = transType TVE TNE ty
    in
      if null kinds
      then ty'
      else MILTy.abs(kinds, ty')
    end) 
  psi

  (* Translate a constructor environment as a sum type *)
  fun transCE TVE TNE CE =
  case map #2 (Id.fixMap CE) of
    [SOME ty] => 
    transType TVE TNE ty

  | [NONE] =>
    MILTy.prod []

  | CElist =>
    MILTy.sum  
    (map (fn NONE => [] | SOME ty => [transType TVE TNE ty]) CElist)

(*----------------------------------------------------------------------*)
(* Translate a set of datatype definitions into a tyname-to-type map.	*)
(* Assumption: datatypes are *regular*.                                 *)
(*----------------------------------------------------------------------*)
fun transDE TNE DE =
let

  (* Translate a single recursive or non-recursive datatype decl *)
  (* Add to the type name environment supplied *)
  (*@TODO: CRUSSO the fact that transDef only looks at the first datdef is
          suspect: review! *)
  fun transDef (defs as ((tyvars,_,_)::_), (TNE,resultTNE)) =
  let
    val numdefs = length defs
    val arity = length tyvars

    (* Create type variables for the parameters to the type constructor *)
    val (_,kinds) = TransOps.freshDebTyVars numdefs tyvars

    (* Map recursive occurrences onto MIL type variables *)
    val (_,TNE') = 
      foldl (fn ((tyvars,tyname,_), (i,TNE)) =>
      let
        val K = if TyName.equality tyname = TyName.Eq
                then MILTy.Eq else MILTy.Any
      in
        (i+1, TyName.Map.insert(TNE, tyname, MILTy.abs(kinds, MILTy.deb i)))
      end) (arity, TNE) defs
      
    (* Construct the recursive definitions *)
    val mildefs = 
      map (fn (tyvars, tyname, CE) => 
      let
        val (TVE,_) = TransOps.freshDebTyVars numdefs tyvars
      in
        (tyname, transCE TVE TNE' CE)
      end) defs

  in
    (* Enumerate the projections from the fixed point of the definitions *)
    ListOps.foldli (fn (i, (tyvars, tyname, CE), (TNE,resultTNE)) => 
      let
        val ty = MILTy.abs(kinds, MILTy.mu(i,mildefs))
      in
        (TyName.Map.insert(TNE, tyname, ty), 
         TyName.Map.insert(resultTNE, tyname, ty)) 
      end) (TNE,resultTNE) defs
  end
in
  #2 (foldl transDef (TNE,TyName.Map.empty) DE)
end

(*----------------------------------------------------------------------*)
(* Translate the environment for a top-level structure into its   	*)
(* corresponding MIL tuple type.                                        *)
(*----------------------------------------------------------------------*)
fun transE TNE E =
let
  val SE = EnvOps.SEofE E
  val VE = EnvOps.VEofE E
  fun transValBind (ValBind.VarSch sch) = SOME (transScheme TNE sch)
    | transValBind _ = NONE
in
  MILTy.prod
  (map #2 (Id.fixMap (Symbol.Map.map (transE TNE) SE)) @
   map #2 (Id.fixMap (Symbol.Map.mapPartial transValBind VE)))
end

end (* of struct *)
