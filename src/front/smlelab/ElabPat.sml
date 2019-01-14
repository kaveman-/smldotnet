(*=========================================================================*)
(* Elaboration of pattern expressions                                      *)
(* (incl. translation of enum constants to enum constructor applications)  *)
(*=========================================================================*)
structure ElabPat :> ELABPAT =
struct

local 
  open Syntax ElabOps Env EnvOps SMLTy SMLPrimTy ElabTy ElabState
in

structure Map = Symbol.Map

(*----------------------------------------------------------------------*)
(* Patterns (p26-27 Defn)                                               *)
(* Return a quadruple consisting of:                                    *)
(*   the translated expression                                          *)
(*   an environment for the pattern variables                           *)
(*   a type for the pattern                                             *)
(*   whether or not the pattern is `valuable'                           *)
(* NB: infPat is rebound below                                          *)
(*----------------------------------------------------------------------*)

fun infPat (C : Context) ((loc,prepat) : Pat) =
  case prepat of

  (* Rule 32 *)
    PatWild => 
    (SMLTerm.PatWild, Map.empty, freshType (), true)

  (* Rule 33 *)
  | PatSCon scon => 
    (SMLTerm.PatSCon(scon, loc), Map.empty, ElabSCon.typeSCon scon, false)

  (* Rule 34,35 *)
  | PatVar vid =>
    let
      val vid = ElabOps.vidToLongid vid
    in
      case EnvLookup.lookupVid (EofC C, loc, vid) of

      (* Rule 35 *)   
      SOME (ValBind.ConSch(sch,CE as (_,_,m))) =>
      let val (tys, ty) = SMLSchOps.instantiate sch
      in
        case InterOp.isEnumType ty of 
           SOME ty' => (* interop: translate enum constant to enum constructor application *)
           let val [conid] = Map.listKeys m
               val [(_,SOME c)] = InterOp.getStaticFields (ty,List.last vid)      
           in
               (SMLTerm.PatCon(conid,CE,tys,SOME (SMLTerm.PatLiteral c)),Map.empty,ty,true)
           end 
        | NONE  => (* Rule 35 proper *)
              (SMLTerm.PatCon(List.last vid, CE, tys, NONE), Map.empty, ty, 
               Map.numItems m = 1)
      end

    (* Rule 35 *)
    | SOME (ValBind.ExTy(ty, exbind)) => 
      (SMLTyUnify.unify ((SOME loc, "exception pattern", ty),
                   (NONE, "expected", exnType));
      (SMLTerm.PatExCon(exbind, NONE), Map.empty, ty, false))

    (* Rule 34 *)
    | _ =>       
      case vid of
        [v] =>
        let
          val ty = freshType ()
        in
          (SMLTerm.PatVar (v,ty), Map.insert (Map.empty, v, ty), ty, true)
        end

      | _ => 
        (error (Error.error (loc, 
          "found a variable when expecting a constructor: " ^ 
          Longid.toString vid), []);
        (SMLTerm.PatWild, Map.empty, freshMono(), true))
  end

  (* Rule 36 *)
  | PatRecord(openrec, patrow) =>
    let 
      val (row, VE', rho, valuable) = infPatRow C patrow
      val rty = 
        if openrec then openRecType (Map.listItemsi rho)
        else SMLTy.recType (Map.listItemsi rho)
    in
      (SMLTerm.PatRecord(openrec, row), VE', rty, valuable)
    end

  (* Rule 37 elided: parentheses *)

  (* Rule 40 elided: atpat -> pat *)

  (* Special case of rule 35 with references *)
  | PatCon(vid, pat) => 
    if (case vid of [id] => Symbol.equal(id, 
      Symbol.symbol (UString.fromString "ref")) | _ => false)
    then
      let
        val (p, VE', ty', valuable) = infPat C pat
      in
        (SMLTerm.PatRef p, VE', SMLTy.refType (ty',SMLTy.heapRefType), false)
      end
    else
  
  (* Rules 41 *)
    (case EnvLookup.lookupVid (EofC C, loc, vid) of
    SOME (ValBind.ConSch(sch, CE as (_,_,m))) => 
      let
        val ty = freshType ()
        val (tys, ty'') = SMLSchOps.instantiate sch
        val (p, VE', ty', valuable) = infPat C pat
      in
        SMLTyUnify.unify ((SOME loc, "constructor", ty''),
                   (NONE, "expected", funType (ty',ty)));
        (SMLTerm.PatCon(List.last vid, CE, tys, SOME p), VE', ty, 
        Map.numItems m = 1 andalso valuable)
      end

  | SOME (ValBind.ExTy(ty',exbind)) =>
      let
        val (p, VE', ty, valuable) = infPat C pat
      in
        SMLTyUnify.unify ((SOME loc, "exception constructor", ty'),
                 (NONE, "expected", funType (ty, exnType)));
        (SMLTerm.PatExCon(exbind, SOME(ty,p)), VE', exnType, false)
      end

  | _ => 
      (error (Error.error (loc, 
        "non-constructor applied to argument in pattern: " ^
        Longid.toString vid), []);
      (SMLTerm.PatWild, Map.empty, freshMono(), true))
  )

  (* Rule 42 *)
  | PatConstraint(pat as (loc1,_), tyexp as (loc2,_)) =>
    let
      val (p, VE, ty, valuable) = infPat C pat
      val ty' = infTy C tyexp
      val ty'' = SMLTyUnify.unify ((SOME loc1, "pattern", ty),
                 (SOME loc2, "type constraint", ty'))
    in
      (p, VE, ty'', valuable)
    end

  (* Rule 43 *)
  | PatLayer((_,v), tyopt, pat as (loc,_)) => 
    let
      val (p, VE, ty, valuable) = infPat C pat
      val ty = 
        case tyopt of
        NONE => ty
      | SOME (ty' as (loc',_)) => 
        let
          val ty' = infTy C ty'
          val ty = SMLTyUnify.unify ((SOME loc, "pattern", ty),
               (SOME loc', "type constraint", ty'))
        in
          ty'
        end
    in
      (SMLTerm.PatLayer((v,ty), p), Map.insert (VE, v, ty), ty, valuable)
    end

  (* Derived forms *)
  | PatTuple pats =>
    infPat C (patTuple loc pats)

  | PatList pats =>
    infPat C (patList loc pats)

  | OrPat pats =>
    let
      val results = map (infPat C) pats
      val VE::VEs = map #2 results
      val smlty::smltys = map #3 results
      fun unify' (ty1, ty2) =
        SMLTyUnify.unify ((SOME loc, "pattern", ty1), 
                          (SOME loc, "pattern", ty2))
      val unionVE= Map.unionWith unify'
      val VE = foldr unionVE VE VEs
      val ty = foldr unify' smlty smltys
    in
      (SMLTerm.PatOr(map #1 results), VE, ty, 
        List.all Gen.identity (map #4 results))
    end
  | PatCast (id, tyexp) => (*@TODO: akenn to review *)
    let
      val ty1 = freshMono() 
      val ty2 = infTy C tyexp
      val constraint = TyConstraint.Sub(ty1, TyConstraint.Cast, ty2)
    in
      TyConstraint.add (constraint,loc);
      (SMLTerm.PatCast(id, ty2), Map.insert (Map.empty, id, ty2), 
        ty1, false)
    end
(*----------------------------------------------------------------------*)
(* Pattern Rows (p26 Defn)                                              *)
(*----------------------------------------------------------------------*)
and infPatRow C patrow = 
case patrow of
(* Rule 38 *)
  [] => ([], Map.empty, Map.empty, true)

(* Rule 39 *)
| (lab, pat as (loc,_))::patrow =>
  let
    val (p, VE, ty, valuable) = infPat C pat
    val (ps, VE', tyrow, valuable') = infPatRow C patrow
    val VE = patmerge loc (VE, VE')
  in
    ((lab,p)::ps, VE, Map.insert(tyrow, lab, ty), 
    valuable andalso valuable')
  end

val infPat = fn (C : Context) => fn  (pat as (loc,_) : Pat) =>
    if EnvLookup.cacheTypes 
    then
        let val res as (_,VE,ty,_) = infPat C pat
            val _ = EnvLookup.pattern(loc,VE,ty)
        in  
            res
        end
    else infPat C pat

end (* of local open *)

end (* of struct *)
