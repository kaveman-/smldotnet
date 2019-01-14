(*======================================================================*)
(* Value binding operations.                                            *)
(*======================================================================*)
structure ValBindOps :> VALBINDOPS =
struct

open ValBind

(*----------------------------------------------------------------------*)
(* Pickling								*)
(*----------------------------------------------------------------------*)
local open Pickle in
  val pickler =
  alttag (fn ExTy _ => 0 | VarSch _ => 1 | ConSch _ => 2 | Special _ => 3)
  [
    wrap (ExTy, fn ExTy x => x) (pair(SMLTy.pickler, TyName.pickler)),
    wrap (VarSch, fn VarSch x => x) SMLSchOps.pickler,
    wrap (ConSch, fn ConSch x => x) (pair (SMLSchOps.pickler,SMLTy.datDefPickler)),
    wrap (Special, fn Special x => x) (pair (SMLTy.pickler, option IdPickle.id))
  ] 
end

(*----------------------------------------------------------------------*)
(* Free type variables							*)
(*----------------------------------------------------------------------*)
fun tyvars (ExTy(ty,_))    = SMLTy.tyvars ty
  | tyvars (VarSch sch)    = SMLSchOps.tyvars sch
  | tyvars (ConSch(sch,_)) = SMLSchOps.tyvars sch
  | tyvars (Special _)     = TyVar.Set.empty

(*----------------------------------------------------------------------*)
(* Apply a type name renaming to a binding (including exn names)	*)
(*----------------------------------------------------------------------*)
fun rename r vb =
case vb of
  ExTy(ty,exname) => ExTy(SMLTy.renameType r ty, TyName.rename r exname)
| VarSch(SMLSch.TypeScheme(tyvars, ty)) =>
  VarSch(SMLSch.TypeScheme(tyvars, SMLTy.renameType r ty))
| ConSch(SMLSch.TypeScheme(tyvars, ty), (tyvars', tyname, CE)) =>
  ConSch(SMLSch.TypeScheme(tyvars, SMLTy.renameType r ty), 
    (tyvars', TyName.rename r tyname,
    Symbol.Map.map (Option.map (SMLTy.renameType r)) CE))
| Special _ => vb

(*----------------------------------------------------------------------*)
(* Apply a realisation to a binding					*)
(*----------------------------------------------------------------------*)
fun appRealisation psi vb = 
case vb of
  ExTy(ty, exname) => ExTy(SMLTy.appRealisation psi ty, exname)
| VarSch(SMLSch.TypeScheme(tyvars, ty)) =>
  VarSch(SMLSch.TypeScheme(tyvars, SMLTy.appRealisation psi ty)) 
| ConSch(SMLSch.TypeScheme(tyvars, ty), (tyvars', tyname, CE)) =>
  ConSch(SMLSch.TypeScheme(tyvars, SMLTy.appRealisation psi ty), 
    (tyvars', 
    case TyName.Map.find(psi, tyname) of
      SOME (_,ty) =>
      (case SMLTy.fromConsType ty of
        SOME (_,tyname) => tyname)
    | _ => tyname,
    Symbol.Map.map (Option.map (SMLTy.appRealisation psi)) CE))
| Special _ => vb

(*----------------------------------------------------------------------*)
(* Type names in a binding (excluding exn names)			*)
(*----------------------------------------------------------------------*)
fun tynames (ExTy(ty,_))    = SMLTy.tynames ty
  | tynames (VarSch(sch))   = SMLSchOps.tynames sch
  | tynames (ConSch(sch,_)) = SMLSchOps.tynames sch
  | tynames (Special _)     = TyName.Set.empty

(*----------------------------------------------------------------------*)
(* String representation for diagnostics and signature display		*)
(*----------------------------------------------------------------------*)
fun toStringWith (depth,tnf) (ExTy(ty,_))    = SMLTy.openTypeToStringWith tnf ty
  | toStringWith (depth,tnf) (VarSch(sch))   = SMLSchOps.toStringWith tnf sch
  | toStringWith (depth,tnf) (ConSch(sch,_)) = SMLSchOps.toStringWith tnf sch
  | toStringWith (depth,tnf) (Special(ty,NONE)) = 
    let
      val tys = map #2 (InterOp.getConstructors (fn flags => true) ty)
      val sep = Pretty.newline depth ^ " and "
    in
      Pretty.simpleVec sep  (SMLTy.openTypeToStringWith tnf) tys
    end
  | toStringWith (depth,tnf) (Special(ty,SOME lab))=
    let
      val tys1 = InterOp.getStaticMethods (ty, lab)
      val tys2 = map #1 (InterOp.getStaticFields (ty, lab))
      val sep = Pretty.newline depth ^ " and "
    in
      Pretty.simpleVec sep (SMLTy.openTypeToStringWith tnf) (tys1@tys2)
    end
      
val toString = toStringWith (0,TyNames.toString)

(*----------------------------------------------------------------------*)
(* Equality: used to compare signatures for purposes of recompilation	*)
(*----------------------------------------------------------------------*)
fun eq (VarSch(sch1), VarSch(sch2)) = 
    SMLSchOps.eq (sch1, sch2)

  | eq (ConSch(sch1,CE1), ConSch(sch2, CE2)) =
    SMLSchOps.eq(sch1, sch2) (* ?? andalso CE1=CE2 ?? *)

  | eq (ExTy(ty1,exname1), ExTy(ty2, exname2)) =
    SMLTy.eq(ty1,ty2) andalso TyName.eq(exname1, exname2)

  | eq (Special(ty1,lab1),Special(ty2,lab2)) =
    SMLTy.eq(ty1,ty2) andalso Eq.option Symbol.equal (lab1,lab2)

  | eq _ =
    false    

end
