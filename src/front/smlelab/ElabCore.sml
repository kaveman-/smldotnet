(*======================================================================*)
(* Elaboration of the core language					*)
(*======================================================================*)
structure ElabCore :> ELABCORE =
struct

structure T = SMLTerm

local open 
  ElabOps Env EnvOps SMLTy Syntax SMLPrimTy SMLSch SMLSchOps ElabTy ElabPat 
  ValBind SMLTyUnify ElabState
in

val valueWarning = Controls.add true "warn.value"

structure Map = Symbol.Map

(*----------------------------------------------------------------------*)
(* Fun dec derived form							*)
(*----------------------------------------------------------------------*)
(*@TODO: improve derived locations *)
fun translateFValBindItem ((fvalbinditem as (loc,f,pat,_,_)::_):FValBindItem list) =
  let
    val vs = map (fn (patloc,_) => (patloc,freshVar ())) pat
    val floc = {left = #left loc, right = #right (#1 (List.last fvalbinditem))}
    fun locFromPats pats = {left = #left (#1 (List.hd pat)),right = #right (#1 (List.last pat))}
  in
    ((loc,PatVar (Short f)), abs vs (floc, 
        Case(expTuple (locFromPats pat) (map (fn (patloc,v) => (patloc, LongVid(Short v))) vs), 
             map (fn (loc, f, pats, exp as (loc',_), tyopt) => 
               (patTuple (locFromPats pats) pats, 
               case tyopt of NONE => exp
                           | SOME ty => (loc',Constraint(exp,ty))))
               fvalbinditem)))
  end

fun translateFValBind (loc,(tyvars,fvalbind)) =
  (loc, ValRec (tyvars, map translateFValBindItem fvalbind))

val bogusTerm = T.Record []

(*----------------------------------------------------------------------*)
(* Terms for false, true, nil and ::					*)
(*----------------------------------------------------------------------*)
val falseTerm = T.Con(Id.falseSym, TopEnv.boolCE, [])
val trueTerm  = T.Con(Id.trueSym, TopEnv.boolCE, [])
fun nilTerm ty = T.Con(Id.nilSym, TopEnv.listCE, [ty])
fun consTerm ty = T.Con(Id.consSym, TopEnv.listCE, [ty])



fun clos vs1 vs2 =
  let 
    val tyvars = TyVar.Set.listItems (TyVar.Set.difference (vs2, vs1))
  in
    tyvars
  end

(*----------------------------------------------------------------------*)
(* Detect a valbind that's an overloaded binding.			*)
(*----------------------------------------------------------------------*)
(* SL: or *)
(*
fun fromOverloaded [] = NONE
  | fromOverloaded [_] = NONE
  | fromOverloaded (((_, PatVar (Short id | OpShort id)), exp)::valbind) =
    let
      fun checkrest ([], acc) = SOME (id, exp::rev acc)
        | checkrest (((_, PatVar (Short id' | OpShort id')), exp')::valbind, 
            acc) = 
          if Symbol.equal(id,id') then checkrest (valbind, exp'::acc)
          else NONE
    in
      checkrest (valbind, [])
    end

  | fromOverloaded _ = NONE
*)
fun fromOverloaded [] = NONE
  | fromOverloaded [_] = NONE
  | fromOverloaded (((_, PatVar (Short id)), exp)::valbind) =
    let
      fun checkrest ([], acc) = SOME (id, exp::rev acc)
        | checkrest (((_, PatVar (Short id')), exp')::valbind, acc) = 
          if Symbol.equal(id,id') then checkrest (valbind, exp'::acc)
          else NONE
	| checkrest (((_, PatVar (OpShort id')), exp')::valbind, acc) = 
          if Symbol.equal(id,id') then checkrest (valbind, exp'::acc)
          else NONE
    in
      checkrest (valbind, [])
    end
  | fromOverloaded (((_, PatVar (OpShort id)), exp)::valbind) =
    let
      fun checkrest ([], acc) = SOME (id, exp::rev acc)
        | checkrest (((_, PatVar (Short id')), exp')::valbind, acc) = 
          if Symbol.equal(id,id') then checkrest (valbind, exp'::acc)
          else NONE
	| checkrest (((_, PatVar (OpShort id')), exp')::valbind, acc) = 
          if Symbol.equal(id,id') then checkrest (valbind, exp'::acc)
          else NONE
    in
      checkrest (valbind, [])
    end
  | fromOverloaded _ = NONE

(*----------------------------------------------------------------------*)
(* Something to return in case of error					*)
(*----------------------------------------------------------------------*)
fun bogusResult () = (bogusTerm, freshMono())
fun bogusLocResult loc = ((loc,bogusTerm), freshMono())

(*----------------------------------------------------------------------*)
(* Elaborate a longvid, possibly resulting in an unresolved static	*)
(*----------------------------------------------------------------------*)
fun infVidI C (loc,longvid) =
  case EnvLookup.lookupVid' (EofC C, loc, longvid) of
    NONE => 
    (error (Error.error (loc, "unbound variable or constructor: " ^ 
      Longid.toString longvid), []); bogusResult ())
 
  | SOME (vbind, longid) =>
    case vbind of
      VarSch(sch as SMLSch.TypeScheme(tyvars,_)) => 
      let
        val (tys, ty) = instantiate sch
      in
        case map TyVar.sort tyvars of
          [TyVar.Overloaded tynameset] => 
          (T.OverloadedVar (longid, tynameset, tys), ty)

        | _ =>
          (T.Var (longid, tys), ty)
      end

    | ConSch(sch,CE) =>
      let 
        val con = List.last longvid
        val (tys, ty) = instantiate sch
      in
        (T.Con (con, CE, tys), ty)
      end

    | ExTy(ty, exname) => 
      (T.ExCon(exname, Option.map #1 (fromFunType ty)), ty)

    | Special(classty, NONE) =>
      let
        val usety = SMLTy.funType (freshMono (), classty)
        val defty = SMLTy.funType (freshMono (), classty)
        val constraint1 = TyConstraint.HasStatic(classty, NONE, defty)
        val constraint2 = TyConstraint.Sub(defty, TyConstraint.MembSub, usety)
      in
        TyConstraint.add (constraint1,loc);
        TyConstraint.add (constraint2,loc);
        (T.Invoc { defty = (classty, defty), usety = (classty, usety), 
          name = NONE, object = NONE, optype = Ext.Invoke }, usety)
      end

    | Special(classty, nameopt) =>
      let
        val usety = freshMono ()
        val defty = freshMono ()
        val constraint1 = TyConstraint.HasStatic(classty, nameopt, defty)
        val constraint2 = TyConstraint.Sub(defty, TyConstraint.MembSub, usety)
      in
        TyConstraint.add (constraint1,loc);
        TyConstraint.add (constraint2,loc);
        (T.Invoc { defty = (classty, defty), usety = (classty, usety), 
          name = nameopt, object = NONE, optype = Ext.Invoke }, usety)
      end

(*----------------------------------------------------------------------*)
(* Elaborate expressions (p21 Defn)					*)
(*----------------------------------------------------------------------*)

fun infExp (C : Context) (exp as (loc,preexp)) = 
  if EnvLookup.cacheTypes then
    let val tyvar = freshType ()
        val _ = EnvLookup.expression(loc,tyvar)
        val res as (exp,ty) = infExp' C exp
    in  
	unify ((NONE,"",tyvar),
               (NONE, "", ty));
	res
    end
  else infExp' C exp
    
and infExp' (C : Context) (loc,preexp) = 
 let fun withLoc (pe,ty) = ((loc,pe),ty)
 in
 case preexp of

(* Rule 1 *)
  SCon scon => 
  let
    val ty = ElabSCon.typeSCon scon
  in
    withLoc(T.SCon(scon,ty,loc), ty)
  end

(* Rule 2 *)
| LongVid vid =>
  let val (pe,ty)=infVidI C (loc, ElabOps.vidToLongid vid)
  in (*@TODO:cleanup*)
      withLoc(pe,ty)
  end

(* Rule 3 *)
| Record exps => 
  let
    val exptyrow = infExpRow C exps
  in
    withLoc(T.Record (map (fn (lab,(e,ty)) => (lab,e,ty)) exptyrow), 
	    SMLTy.recType (map (fn (lab,(e,ty)) => (lab,ty)) exptyrow))
  end

(* Rule 4 with new side condition *)
| Let(dec, exp) =>
  let
    val stamp = getStamp ()
    val (d, E') = infDec C false dec
    val (e, ty) = infExp (CplusE C E') exp
  in
    if List.all (fn tn => TyName.earlier(tn,stamp))
       (TyName.Set.listItems (SMLTy.tynames ty))
    then ()
    else error (Error.error (loc, 
      "local datatype declaration escapes scope"), []);
    withLoc(T.Let(d,e), ty)
  end

(* Rule 4 with new side condition and unless clause *)
| LetUnless(dec, exp as (loc2,_), match as ((loc1,_),_)::_) =>
  let
    val stamp = getStamp ()
    val (d, E') = infDec C false dec
    val (e, ty) = infExp (CplusE C E') exp
    val (mrules, ty2, ty3) =
      infMatch C match 
        (fn ty => unify ((SOME loc1, "unless expression", ty),
                         (NONE, "expected", exnType)))
  in
    unify ((SOME loc2, "body of let", ty),
                (NONE, "handlers", ty3));
    if List.all (fn tn => TyName.earlier(tn,stamp))
       (TyName.Set.listItems (SMLTy.tynames ty))
    then ()
    else error (Error.error (loc, 
      "local datatype declaration escapes scope"), []);
    withLoc(T.LetUnless(d,e,(ty3,mrules)), ty)
  end

(* Rule 5 elided: parentheses *)
(* Rule 6 below *)
(* Rule 7 elided: atexp to exp *)

(* All of these uses are illegal *)
| DotHash _ =>
  (error (Error.error (loc, "illegal use of .#"), []); bogusLocResult loc)

(* All other uses are illegal *)
| DotHashHash _ =>
  (error (Error.error (loc, "illegal use of .##"), []); bogusLocResult loc)

(*......................................................................*)
(* For _synchronized check that the first arg is a class or ref type    *)
(*......................................................................*)
| Synchronized(exp1 as (loc1,_), exp2) =>
  let
    val (e1, ty1) = infExp C exp1
    val (e2, ty2) = infExp C exp2
  in
    if SMLTy.isClass ty1 
    then (withLoc(T.Special ((Ext.Synchronized,NONE,NONE),
			     [e1,e2],SOME ty2,Effect.any),
		  ty2))
    else
    let
      val ty = freshType ()
    in
      unify ((SOME loc1, "synchronize on", ty1), 
                 (NONE, "expected", SMLTy.refType(ty,SMLTy.heapRefType)));
      withLoc(T.Special ((Ext.Synchronized,NONE,NONE),
			  [e1,e2],SOME ty2,Effect.any),
	      ty2)
    end
  end

(*..................................................................*)
(* Non-static method invocation/field access			    *)
(*..................................................................*)
| App(exp, (locLab, DotHash lab)) =>
  let
    val (e, ty) = infExp C exp
    val defty = freshMono ()
    val usety = freshMono ()
    val classty = freshMono ()
    val constraint1 = TyConstraint.Has(ty, lab, (classty,defty))
    val constraint2 = TyConstraint.Sub(defty, TyConstraint.MembSub, usety)
  in
    EnvLookup.instance(ty,locLab); (* cache this type for the language service *)
    TyConstraint.add (constraint1,loc);
    TyConstraint.add (constraint2,loc);
    withLoc(T.Invoc { defty = (classty,defty), usety = (ty,usety), name = SOME lab, 
		      object = SOME e, optype = Ext.Invoke }, 
	    usety)
  end

| App(exp, (locLab, DotHashHash lab)) =>
  let
    val (e, ty) = infExp C exp
    val defty = freshMono ()
    val usety = freshMono ()
    val superty = freshMono ()
    val classty = freshMono ()
    val constraint1 = TyConstraint.Has(superty, lab, (classty,defty))
    val constraint2 = TyConstraint.Sub(defty, TyConstraint.MethSub, usety)
    val constraint3 = TyConstraint.Sub(ty, TyConstraint.Super, superty)
  in
    EnvLookup.instance(superty,locLab); (* cache the supertype for the language service *)
    TyConstraint.add (constraint1,loc);
    TyConstraint.add (constraint2,loc);
    TyConstraint.add (constraint3,loc);
    withLoc(T.Invoc { defty = (classty, defty), usety = (ty, usety), name = SOME lab,
		      object = SOME e, optype = Ext.InvokeSuper }, 
            usety)
  end

(*......................................................................*)
(* Rule 8								*)
(*......................................................................*)
| App(funexp as (loc1,_), argexp as (loc2, _)) =>
  let
    val (arge, argty) = infExp C argexp
    val (fune, funty) = infExp C funexp
  in
    case fromFunType funty of
      SOME (ty1, ty3) =>
      (unify ((SOME loc2, "actual argument type", argty), 
              (SOME loc1, "function argument type", ty1));
       withLoc(T.App(fune, arge), ty3))

    | NONE =>
      let
        val tyvar = freshType ()
      in
        unify ((SOME loc1, "function expression", funty), 
               (SOME loc2, "expected", SMLTy.funType (argty, tyvar)));
        withLoc(T.App(fune, arge), tyvar)
      end
  end
        
(*......................................................................*)
(* Rule 9								*)
(*......................................................................*)
| Constraint(exp as (loc1,_), typ as (loc2,_)) =>
  let
    val (e, ty1) = infExp C exp
    val ty2 = infTy C typ
    val ty3 = unify ((SOME loc1, "expression", ty1), 
                 (SOME loc2, "type", ty2))
  in
    (e, ty2)
  end
 
(*......................................................................*)
(* For cast check that the types are interop types and that there's a   *)
(* valid cast.								*)
(*......................................................................*)
| ConstraintGt(exp as (loc1,_), typ as (loc2,_)) =>
  let
    val (e, ty0) = infExp C exp
    (* force ty0 to be mono *)
    val ty1 = unify ((SOME loc1, "expression type", ty0),
		      (SOME loc2, "mono type",freshMono ()))
    val ty2 = infTy C typ
    val constraint = TyConstraint.Sub(ty1, TyConstraint.Cast, ty2)
  in
    TyConstraint.add (constraint,loc);
    withLoc(T.Special ((Ext.Cast,SOME ty2,NONE),[e],SOME ty2,Effect.throwsAny),
            ty2)
  end

(*......................................................................*)
(* Purity 'assertion'							*)
(*......................................................................*)
| Pure exp =>
  let
    val (e, ty) = infExp C exp
  in
    withLoc(T.Special((Ext.Pure, NONE, NONE), [e], SOME ty, Effect.allocs),
	    ty)
  end

(* Rule 10 *)
| Handle(exp as (loc1,_), match) =>
  let
    val (e, ty1) = infExp C exp
    val (mrules, ty2, ty3) =
    infMatch C match 
      (fn ty => unify ((SOME loc1, "handle expression", ty),
                       (NONE, "expected", exnType)))
  in
    unify ((SOME loc1, "handle expression", ty1),
                (NONE, "handlers", ty3));
    withLoc(T.Handle(e,(ty3,mrules)),
	    ty3)
  end

(* Rule 11 *)
| Raise (exp as (loc',_)) =>
  let
    val (e, ty) = infExp C exp
    val ty' = freshType ()
  in
    unify ((SOME loc', "expression", ty),
                (NONE, "expected", exnType));
    withLoc(T.Raise(e, ty', loc),
	    ty')
  end
 
(* Rule 12 *)
| Fn match => 
  let
    val (mrules, ty1, ty2) = infMatch (lamC C) match Gen.identity
  in
    withLoc(T.Fn (ty1, (ty2,mrules)),
	    SMLTy.funType(ty1,ty2))
  end

(*......................................................................*)
(* Tuple expression derived form:					*)
(* (exp_1, ..., exp_n) ~~> {1=exp_1, ..., n=exp_n}                      *)
(*......................................................................*)
| Tuple [exp] => 
  Debug.fail "Elab.infExp: singleton tuple"

| Tuple exps =>
  let
    val etys = map (infExp C) exps
    val tys = List.map #2 etys 
  in
    (tupleTerm (loc,etys), SMLTy.tupleType tys) 
  end

(*......................................................................*)
(* Field selection derived form:					*)
(* #lab ~~> fn {lab=var, . . .} => var                                  *)
(*......................................................................*)
| Hash lab =>
  let
    val v = freshVar ()
    val ty = freshType ()
    val rty = openRecType [(lab,ty)]
  in
    withLoc(T.Fn (rty, (ty, 
			[(loc, T.PatRecord(true, [(lab, T.PatVar(v,ty))]), monovar (loc,v))])),
            SMLTy.funType (rty, ty))
  end

(*......................................................................*)
(* Case expression derived form:					*)
(* case exp of match ~~> (fn match)(exp)                                *)
(*......................................................................*)
| Case (exp, match) =>
  let
    val (e, ty1) = infExp C exp
    val (mrules, ty2, ty3) = infMatch C match 
      (fn ty => unify ((SOME loc, "expression", ty1), 
                       (NONE, "pattern", ty)))
  in
    (caseTerm(loc,e,ty1,(ty3, mrules)), ty3)
  end

(*......................................................................*)
(* Conditional derived form:						*)
(* if exp_1 then exp_2 else exp_3 ~~>                                   *)
(* case exp_1 of true => exp_2 | false => exp_3                         *)
(*......................................................................*)
| If (exp1 as (loc1,_), exp2 as (loc2,_), exp3 as (loc3,_)) =>
  let
    val (e1, ty1) = infExp C exp1
    val (e2, ty2) = infExp C exp2
    val (e3, ty3) = infExp C exp3
  in
    unify ((SOME loc1, "expression", ty1),
                (NONE, "expected", boolType));
    unify ((SOME loc2, "then branch", ty2),
                (SOME loc3, "else branch", ty3));
    (condTerm (loc,e1, (e2,loc2), (e3,loc3), ty3), ty3)
  end

(*......................................................................*)
(* orelse derived form:							*)
(* exp_1 orelse exp_2 ~~> if exp_1 then true else exp_2                 *)
(*......................................................................*)
| Orelse (exp1 as (loc1,_), exp2 as (loc2,_)) =>
  let
    val (e1, ty1) = infExp C exp1
    val (e2, ty2) = infExp C exp2
  in
    unify ((SOME loc1,"expression",ty1),
                (NONE, "expected", boolType));
    unify ((SOME loc2,"expression",ty2),
                (NONE, "expected", boolType));
    (condTerm (loc, e1, ((loc1,trueTerm), loc1), (e2, loc2), boolType), boolType)
  end

(*......................................................................*)
(* andalso derived form:						*)
(*......................................................................*)
| Andalso (exp1 as (loc1,_), exp2 as (loc2,_)) =>
  let
    val (e1, ty1) = infExp C exp1
    val (e2, ty2) = infExp C exp2
  in
    unify ((SOME loc1,"expression",ty1),
                (NONE, "expected", boolType));
    unify ((SOME loc2,"expression",ty2),
                (NONE, "expected", boolType));
    (condTerm (loc, e1, (e2,loc1), ((loc2,falseTerm), loc2), boolType), boolType)
  end

(*......................................................................*)
(* Sequence derived form: (slightly simpler translation than p67 Defn)	*)
(* (exp_1; ... ; exp_n ; exp) ~~>                                       *)
(* let val _ = exp_1 in ...                                             *)
(* let val _ = exp_n in exp end ... end                                 *)   
(*......................................................................*)
| Sequence [] =>
  Debug.fail "Elab.infExp: empty sequence expression"

| Sequence [exp] =>
  infExp C exp

| Sequence ((exp as (locexp,_))::(exps as (locexps,_)::_)) =>
  let
    val (e1, ty1) = infExp C exp
    val (e2, ty2) = infExp C (locexps, Sequence exps)
    val v = freshVar ()
  in
    (withLoc(T.Let([T.Val(locexp, [], ty1, T.PatWild, e1)], e2),
	     ty2))
  end

(*......................................................................*)
(* while derived form:							*)
(*......................................................................*)
| While (exp1 as (loc1,_), exp2) =>
  let
    val (e1, ty1) = infExp C exp1
    val _ = unify ((SOME loc1, "expression", ty1),(NONE, "expected", boolType))
    val (e2, ty2) = infExp C exp2
  in
    (makeWhileTerm (loc1, e1, e2), ty2)
  end

(*......................................................................*)
(* List expression derived form:					*)
(* [exp_1, ..., exp_n] ~~>                                              *)
(* exp_1 :: ... :: exp_n :: nil                                         *)
(*......................................................................*)
| List es =>
  let
    val (e, ty) = infList C loc es
  in
    (e, listType ty)
  end

(*......................................................................*)
(* Anonymous inner class hack						*)
(*......................................................................*)
| ClassWith(exp as (loc',App((_,LongVid vid), exparg)), methbind) =>
  let
    val id = Id.fromString "$"
    (*@TODO: should we support attributes --- or just inherit those we should? *)
    val decitem = 
    (loc,ClassDec (
      ClassType {
        attributes=[(*@TODO: what goes in here?*)], modifiers=[], conattributes=[(*@TODO: what goes in here?*)],
	tycon=id, pat=(loc',PatTuple []), 
        inherits=[Extends((loc',TyCon([], ElabOps.vidToLongid vid)), exparg)],
        localdec=[], 
        methoddec=methbind}))
  in
    infExp C 
    (loc,Let([decitem], 
      (loc',App((loc',LongVid (Short id)), (loc',Tuple [])))))
  end

| ClassWith _ =>
  Debug.fail "ElabCore.infExp: illegal ClassWith form"

| FlatApp _ =>
  Debug.fail "ElabCore.infExp: illegal FlatApp form"
end
and infList C loc [] = 
    let
      val ty = freshType ()
    in
      ((loc,nilTerm ty), ty)
    end

  | infList C loc [exp] =
    let
      val (e, ty) = infExp C exp
    in
      ((loc,T.App((loc,consTerm ty), 
		  tupleTerm (loc,[(e,ty), 
				  ((loc (*@TODO: revise loc *),nilTerm ty),
				   listType ty)]))), 
       ty)
    end

  | infList C loc ((exp as (loc1 ,_))::(exps as (loc2,_)::_)) =
    let
      val (e1, ty1) = infExp C exp
      val (e2, ty2) = infList C {left=(#left loc2),right=(#right loc)} exps
      val ty3 = unify ((SOME loc1, "list element", ty1),
		       (SOME loc2, "rest of list", ty2))
    in
      ((loc,
	T.App((loc,consTerm ty1), 
	      tupleTerm ({left=(#left loc1),right=(#right loc)},
			 [(e1,ty3), 
			  (e2,listType ty3)]))), 
       ty3)
    end
    
(*----------------------------------------------------------------------*)
(* Expression Rows (p22 Defn)						*)
(*----------------------------------------------------------------------*)
(* Rule 6 *)
and infExpRow C exprow = map (fn (lab,exp) => (lab, infExp C exp)) exprow

(*----------------------------------------------------------------------*)
(* Matches (p22 Defn)   						*)
(*----------------------------------------------------------------------*)
(* Rule 13 *)
and infMatch C [] unif = 
    Debug.fail "ElabCore.infMatch: empty match"

  | infMatch C [(mrule as ((loc,_),_))] unif =
    let
      val (pair, ty1, ty1') = infMRule C mrule unif
    in
      ([pair], ty1, ty1')
    end

  | infMatch C ((mrule as ((loc,_),_))::match) unif =
    let
      val (pair, ty1, ty1') = infMRule C mrule unif
      val (pairs, ty2, ty2') = infMatch C match unif
      val ty3 = unify((SOME loc, "pattern", ty1), (NONE, "pattern",ty2))
      val ty3' = unify((SOME loc, "expression", ty1'),(NONE,"expression",ty2'))
    in
      (pair::pairs, ty3, ty3')
    end

(*----------------------------------------------------------------------*)
(* Match Rules (p23 Defn)                                		*)
(*----------------------------------------------------------------------*)
(* Rule 14 with new side condition *)
and infMRule C (pat as (loc,_),exp) unif = 
  let
    val stamp = getStamp ()
    val (p, VE, ty, valuable) = infPat C pat
    val VE = Map.map (fn ty => ValBind.VarSch (monoType ty)) VE
    val _ = unif ty
    val (e, ty') = infExp (CplusVE C VE) exp
  in
    if List.all (fn tn => TyName.earlier(tn,stamp)) 
    (TyName.Set.listItems (tynamesVE VE))
    then ()
    else error (Error.error (loc, "local datatype declaration escapes scope"),
      []);
    ((loc, p, e), ty, ty')
  end

(*----------------------------------------------------------------------*)
(* Declarations (p23 Defn)						*)
(* Given a context and a declaration return				*)
(* 1. a typed value declaration                                         *)
(* 2. an environment E which binds variables and type constructors	*)
(* The boolean flag indicates whether this is a `top-level' declaration;*)
(* that is, one in which ungeneralizable type variables should be       *)
(* flagged as errors.                                                   *)
(*----------------------------------------------------------------------*)
and infDecItem C toplevel ((loc,dec) : DecItem) = 
 let fun withLoc d = ((* loc, *) d) (*@TODO: remove *)
 in
case dec of

(* Rule 15 *)  
  Val({explicit, implicit}, valbind) =>
  (case fromOverloaded valbind of
    NONE =>
    let
      val U = TyVar.Set.union(
        TyVar.Set.addList(TyVar.Set.empty, map TyVar.explicit explicit),
        TyVar.Set.difference (
          TyVar.Set.addList(TyVar.Set.empty, map TyVar.explicit implicit), 
          UofC C))
      val (ds, VE) = infValBind U (CplusU C U) toplevel loc valbind
    in
      ( [withLoc(T.And ds)], VEinE VE)
    end

  | SOME (id, exps) =>
    let
      val pairs = map (infExp C) exps 
      val SOME (S, ty') = SMLTyUnify.antiunifylist (map #2 pairs)
      (*
      val _ = 
	  (Debug.print "\nanityunifying\n";
	   Debug.print ((Pretty.simpleVec "," SMLTy.toString (map #2 pairs))^"\n");
	   Debug.print ((SMLTy.toString ty')^"\n");
	   Debug.print (Pretty.simpleVec ",\n" 
	   (fn (tyvar,basetys) =>
	      TyVar.toString tyvar  ^ "->" ^
	      (Pretty.simpleVec "," SMLTy.toString basetys))
	      (TyVar.Map.listItemsi S)))
       *)
    in
      case TyVar.Map.listItemsi S of
        [(tyvar, basetys)] =>
        let
          val (tynameset, exptymap) =
            ListPair.foldl (fn ((exp, ty), basety, (tynameset, exptymap)) =>
            let
              val SOME ([], tyname) = SMLTy.fromConsType basety
            in
              (TyName.Set.add(tynameset, tyname), 
              TyName.Map.insert(exptymap, tyname, (exp,ty)))
            end) (TyName.Set.empty, TyName.Map.empty) (pairs, basetys)

          val exptys  = (* as (exps,tys) =  ListPair.unzip *) (TyName.Map.listItems exptymap)
          val prodty = SMLTy.tupleType (* tys *) (List.map #2 exptys)
          val prodexp = tupleTerm (loc,exptys) (*@TODO: revise loc *)
          val tyvar' = freshTyVar (TyVar.Overloaded tynameset)
          val ty = SMLTy.appSubst [(tyvar, tyVarType tyvar')] ty'
        in
          ([withLoc(T.Val(loc, [], prodty, T.PatVar(id, prodty), prodexp))],
          VEinE (Map.insert(Map.empty, id, VarSch(polyType ty))))
        end

      | _ =>
        (error (Error.error (loc, 
          "expected just one type variable in overloaded type"), 
           [("type", ty')]);
        ([], emptyE))
    end)    

| ValRec({explicit, implicit}, valrecbind) =>
  let
    val triples = map (fn ((_,PatVar (Short f)), exp) =>    
       (f, exp, freshType ())) valrecbind
    val U = TyVar.Set.union(
      TyVar.Set.addList(TyVar.Set.empty, map TyVar.explicit explicit),
      TyVar.Set.difference (
        TyVar.Set.addList(TyVar.Set.empty, map TyVar.explicit implicit), 
        UofC C))
    val VE = List.foldr 
        (fn ((f,exp,ty), VE) => Map.insert 
          (VE, f, VarSch(monoType ty))) Map.empty triples
    val (VE', bindings) = infValRecBind (CplusVE (CplusU (CwithClass C NONE) U) VE) loc triples
    val Cfvs = freeVE (VEofE (EofC C))
    val tyvarsVE' = map (SMLTyUnify.resolve loc Cfvs) (Map.listItems VE')
    val tyvarsVE' = (foldr TyVar.Set.union TyVar.Set.empty tyvarsVE')
    val tyvars= clos Cfvs tyvarsVE'

    (* calculate generalized variables (empty if in a localdec of a ClassDec) *)
    val tyvars = List.filter 
	(fn tyvar => 
	 if TyVar.isExplicit tyvar then TyVar.Set.member(U,tyvar)
	 else true) tyvars

    val tyvars =
	case classofC C of
	  NONE =>
	      tyvars
	(*@HACK: don't generalize if in a localdec of a ClassDEc *)
        | SOME  _ =>
	     if null tyvars orelse (not (Controls.get valueWarning)) then
		 []
	     else 
		 (error (Error.warning(loc,"class-local function restriction prevents type variable generalization"),
			 []);
		  [])

    val VE'' = Map.map (fn ty => VarSch(TypeScheme(tyvars, ty))) VE'
  in
    ([withLoc(T.ValRec (tyvars, bindings))], VEinE VE'')
  end

| Fun(tyvars, fvalbind) =>
  infDecItem C toplevel (translateFValBind (loc, (tyvars,fvalbind)))

(* Rule 16 *)
| Type typbind =>
  let
    val TE = infTypBind C typbind
  in
    ([], TEinE TE)
  end

(* Rule 17 -- modified *)
| Datatype (datbind,typbindopt) =>
  let
    val (VE, TE, r) = infDatBind false C (datbind,typbindopt)
  in
    ([], VETEinE (VE,TE))
  end

(* Rule 18 *)
| DatatypeCopy(tycon, longtycon) =>
  let
    val E = infDatCopy C loc (tycon, longtycon)
  in
    ([], E)
  end

(* Rule 19 *)
| Abstype (datbind, typbindopt, dec) =>
  let
    val (VE, TE, tynames) = infDatBind false C (datbind, typbindopt)
    val (d, E') = infDec (CplusE C (VETEinE (VE,TE))) false dec
      val r = foldl (fn (tyname, r) =>
              TyName.Map.insert(r, tyname, 
                TyName.newEquality tyname TyName.NotEq))
                TyName.Map.empty tynames
  in
    (d, renameE r (EplusTE E' TE))
  end
        
(* Rule 20 *)
| Exception exbind =>
  let
    val (ds, EE) = infExBind C loc exbind
  in
    ([withLoc(T.And ds)], VEinE (Map.map ExTy EE))
  end

(* Rule 21 *)
| Local(dec1, dec2) =>
  let
    val (d1, E1) = infDec C false dec1
    val (d2, E2) = infDec (CplusE C E1) toplevel dec2
  in
    ([withLoc(T.Local(d1,d2))], E2)
  end

(* Rule 22 *)

| Open longids =>
  let val (ds,E) = infOpen C loc longids 
  in
      ([withLoc(T.And ds)],E)
  end


(* SL: or *)
(*
| (Infix _ | Infixr _ | Nonfix _) =>
  ([], emptyE)
*)
| Infix _ =>
  ([], emptyE)
| Infixr _ =>
  ([], emptyE)
| Nonfix _ =>
  ([], emptyE)


(* New rule for classes and interfaces*)  
(*@FUTURE: split off interface code *)  
| ClassDec (
  ClassType {tycon, attributes, modifiers, conattributes, pat, inherits, localdec, methoddec }) =>
  let
    (* get the class flags *)
    val flags = Symbol.Set.addList(Symbol.Set.empty, modifiers)
    
    val isInterface = Symbol.Set.member(flags,Id.interfaceSym)

    (* type check attributes *)
    val infAttributes = List.map (infAttExp C) attributes
    val infConAttributes = List.map (infAttExp C) conattributes

    (* Create a fresh type name for this class *)
    val tyname = freshClass (pathofC C @ [tycon])

    (* Also generate a tyvar type for it, to be unified with the defn later *)
    val classty = SMLTy.tyVarType (freshTyVar (TyVar.Normal TySort.class))

    (* Put it in a singleton type environment *)
    val TE = Map.insert(Map.empty, tycon, TyStr.makeConcrete([], classty))

    (* Type check the constructor pattern *)
    val (pat, VEpat, patty, _) = infPat (CplusTE C TE) pat
    val VEpat = Map.insert(VEpat, Id.thisSym, classty)
    val VEpat = Map.map (ValBind.VarSch o monoType) VEpat

    (* Create a constructor binding for a non-interface class *)
    val VEcon = if isInterface then Map.empty else Map.insert(Map.empty, tycon, ValBind.Special (classty, NONE))

    (* Create a context with the new type, with "this", and with the 
       pattern-bound variables *)
    val C' = CplusVE (CplusVE (CplusTE C TE) VEcon) VEpat

    (* Guess types for the methods and any implicit [abstract] modifier *)
    fun typeMeth (attributes,modifiers,(loc,f,bodyopt,tyopt)::MATCHRULES) =
    (*@FUTURE: handle > 1 mathrules *)
    let
      val (methodflags,ty) = 
        case tyopt of 
          SOME ty => 
	   let val funTy = SMLTy.funType(freshMono(),freshMono()) 
	       val ty = infTy C' ty
	   in
	      (ignore(unify((SOME loc,"expected abstract method of function type",funTy),
			      (SOME loc,"found abstract method of non-function type",ty)));
	       (Symbol.Set.addList(Symbol.Set.singleton(Id.abstractSym),modifiers),
		ty))
	   end
        | NONE =>
          let val SOME (pat,exp) = bodyopt 
              val (_, _, ty, _) = infPat C' pat
	      val rng = case exp of 
		          (loc,Constraint(_,sty)) => infTy C' sty
		        | (loc,ConstraintGt(_,sty)) => infTy C' sty
			| _ => freshMono() (*@TODO: should this be a monotype? *)
          in (Symbol.Set.addList(Symbol.Set.empty,modifiers),
	      SMLTy.funType(ty, rng)) end
    in
      { flags = methodflags,
        name = f, 
	ty = ty }
    end
    val methods = map typeMeth methoddec

    (* Check superclass and interfaces *)
    (*@TODO: enforce class/interface inheritance restrictions grammatically *)
    fun checkInherits (result, canExtend, []) = result
      | checkInherits ((superty, superarg, interfaces),canExtend,inh::inherits) =
        case inh of
          Implements ty => 
	  let val ty = infTy C' (*TODO:review just C,not C'?*) ty 
	  in
	  if not(InterOp.isInterface ty)
	  then (error(Error.error (loc, "class cannot implement the non-interface type " ^ SMLTy.toString ty),
		      []); 
	       checkInherits ((superty,superarg,interfaces),false,inherits))
	  else checkInherits ((superty, superarg, ty::interfaces),false,inherits)
	  end
        | Extends (tyexp, exp) => 
	  if canExtend then 
          let
            val ty = infTy C' tyexp
            val (e, argty) = infExp C' exp
	    fun publicOrProtected flags =
		Symbol.Set.member(flags,Id.publicSym) orelse
		Symbol.Set.member(flags,Id.protectedSym) 
            val tys = InterOp.getConstructors publicOrProtected ty
	    val delegateTy = SMLTy.consType([],TyNames.multicastDelegateTyName) 
	    val valueType = SMLTy.consType([],TyNames.valueTypeTyName) 
	    val enumType = SMLTy.consType([],TyNames.enumTyName) 
	    val arrayType = SMLTy.consType([],TyNames.arrayTyName) 
          in
	    if InterOp.sub(ty,delegateTy) 
		then (error(Error.error (loc, "class deriving from " ^ SMLTy.toString delegateTy ^
					" can only be declared with a delegate declaration"),
			    []);
		      checkInherits ((superty,superarg,interfaces),false,inherits))
	    else 
	    if not (SMLTy.isClass(ty)) 
		then (error(Error.error (loc, "class cannot extend the non-class type " ^ SMLTy.toString ty), []);
		      checkInherits ((superty,superarg,interfaces),false,inherits))
	    else 
	    if SMLTy.eq(ty,valueType) 
		orelse SMLTy.eq(ty,arrayType) 
		orelse SMLTy.eq(ty,enumType) 
		orelse SMLTy.eq(ty,valueType)  
		then (error(Error.error (loc, "class cannot extend the special class type " ^ SMLTy.toString ty), []);
		      checkInherits ((superty,superarg,interfaces),false,inherits))
	    else 
            if InterOp.isInterface ty
            then  (error(Error.error (loc, "class cannot extend the interface " ^ SMLTy.toString ty),[]); (*HERE *)
		  checkInherits ((superty,superarg,interfaces),false,inherits))
            else
            ((case tys of
              [mty as (classty, funty)] =>
              if InterOp.argsMoreSpecific (argty,mty)
              then ()            
              else
              let val SOME (defargty, resty) = SMLTy.fromFunType funty
              in          
                ignore (unify ((SOME loc, 
                  "method/constructor argument type", defargty), 
                  (SOME loc, "actual argument type", argty)))
              end
            | [] => 
	      error (Error.error (loc, "no such constructor"), [])
            | _ =>
              let val validtys = 
                List.filter (fn mty =>InterOp.argsMoreSpecific (argty,mty)) tys
              in
	        case InterOp.mostSpecific validtys of
                  NONE =>
                  let
                    val message =
                      if null validtys
                      then 
                        "cannot apply constructor of type " ^
                        Pretty.simpleVec " or " (SMLTy.toString o #2) tys ^
                        " to argument of type " ^ SMLTy.toString argty
                      else 
                        "ambiguous constructor invocation: argument type is "
                        ^ SMLTy.toString argty ^ " and possible types are " ^ 
                        Pretty.simpleVec " and " (SMLTy.toString o #2) validtys
                  in
	            error (Error.error (loc, message), [])
                  end

                | SOME (mty as (classty, funty)) => ()
              end    
            ); checkInherits ((ty, e, interfaces),false, inherits))
          end
          else ((error(Error.error (loc, "class cannot extend more than one class"), []));
		checkInherits ((superty,superarg,interfaces),false,inherits))
    val object = SMLTy.baseType TyNames.objectTyName
    val (superty, superarg, interfaces) = checkInherits
      ((object,(loc,T.Record []), []),
       true,
       inherits) (*@TODO: revise loc*)

    (* Disgusting check to see that an interface is only declared with the correct derived form,
       (empty local dec and all methods abstract checked elsewhere) *)
    val _ = case (isInterface,SMLTy.eq(superty,object),pat,superarg) of
		(true,true,T.PatRecord(false,[]),(_,T.Record[])) => ()
	      | (false,_,_,_) => ()
	      | _ =>  error(Error.error (loc, "interface class must extend " ^ SMLTy.toString object ^ 
					 " with dummy constructor pattern [()] and super-argument [()] "), [])

    val mlclass = 
    SMLTy.MLClass
    {
      tyname = tyname,
      flags = flags,
      super = superty,
      interfaces = interfaces,
      initargty = if isInterface then NONE else SOME patty,
      methods = methods
    }

    val mlclassty = SMLTy.classType mlclass

    val _ = unify ((SOME loc, "class type", classty),
                   (SOME loc, "class def", mlclassty))

    (* Check the local declarations *)
    (* CwithClass(C',tyname) is used to disable generalisation in localdecl *)
    (* For interfaces only, localdec must be empty *)
    val (d, E) = 
	((case (isInterface,localdec) of
	      (true,_::_) =>  error(Error.error (loc, "interface cannot have an empty local declaration"), [])
	    | _ => ());
	 infDec (CwithClass C'(SOME tyname)) false localdec)

    (* Check the methods *)
    val C'' = CplusE C' E
    fun checkMethod ({ty,flags,...}:SMLTy.Method, 
		     (attributes,qualifiers,(_,f,bodyopt,tyopt)::MATCHRULES)) =
    (*@FUTURE: cvr: fix grammar to allow > 1 matchrules and deal with MATCHRULES*)
	let val infMethodAttributes = List.map (infAttExp C'') attributes
	in
	    case bodyopt of
		SOME (pat,exp) =>
		    let 
			val (p, VE, argty, valuable) = infPat C'' pat
			val VE = Map.map (ValBind.VarSch o monoType) VE
			val (e, resty) = infExp (CplusVE C'' VE) exp
		    in
			unify ((SOME loc, "method type", SMLTy.funType (argty, resty)),
			       (SOME loc, "method def", ty));
			SMLTyUnify.resolve loc TyVar.Set.empty ty;
			{name = f,
			 attributes=infMethodAttributes,
			 flags = flags,
			 ty = ty,    (*@TODO: revise loc *)
			 body = SOME ((loc,T.Fn (argty, (resty, [(loc, p, e)]))))}
		    end
	      | NONE =>
		    {name = f, 
		     attributes=infMethodAttributes,
		     flags = Symbol.Set.add(flags,Id.abstractSym), 
		     ty = ty, 
		     body = NONE }
	end 

    val actualmethods = ListPair.map checkMethod (methods, methoddec)
    val decitem = 
    T.ClassType
    { loc = loc,
      attributes = infAttributes, 
      tyname = tyname,
      conattributes = infConAttributes, 
      flags = flags,
      superty = superty,
      superarg = superarg,
      interfaces = interfaces,
      methods = actualmethods,
      localdec = d,
      argpat = pat,
      argty = patty
    }

    val TE = Map.insert(Map.empty, tycon, TyStr.makeClasstype mlclass)
  in
    ([withLoc decitem], VETEinE (VEcon,TE))
  end

| ClassDec (
  DelegateType {tycon, attributes, modifiers, conattributes,ty}) =>
  let
    (* type check attributes *)
    val infAttributes = List.map (infAttExp C) attributes
    val infConAttributes = List.map (infAttExp C) conattributes

    (* Create a fresh type name for this class *)
    val tyname = freshClass (pathofC C @ [tycon])

    val superty = SMLTy.baseType (TyNames.multicastDelegateTyName)

    (* Also generate a tyvar type for it, to be unified with the defn later *)
    val classty = SMLTy.tyVarType (freshTyVar (TyVar.Normal TySort.class))

    (* Put it in a singleton type environment *)
    val TE = Map.insert(Map.empty, tycon, TyStr.makeConcrete([], classty))

    val VEcon = Map.insert(Map.empty, tycon, ValBind.Special (classty, NONE))

    (* a delegate class must be final(sealed) *)
    val flags = Symbol.Set.addList(Symbol.Set.empty, Id.sealedSym::modifiers)

    val ty = infTy C ty
    val _ = ignore(unify ((SOME loc, "specified argument type", ty),
		          (SOME loc, "expected function type",SMLTy.funType (freshMono (), freshMono()))))
    val invokeSym = Id.fromString "Invoke"
    val methods = [{flags = Symbol.Set.empty ,		
		    name = invokeSym,
		    ty = ty}]
    val mlclass = 
    SMLTy.MLClass
    {
      tyname = tyname,
      flags = flags,
      super = superty,
      interfaces = [],
      initargty = SOME ty,
      methods = methods
    }

    val mlclassty = SMLTy.classType mlclass

    val _ = unify ((SOME loc, "class type", classty),
                   (SOME loc, "class def", mlclassty))

    val methoddec = [{attributes = [], 
		      flags = Symbol.Set.empty,
		      name = invokeSym,
		      ty = ty,
		      body = NONE}] 
    val decitem = 
     T.ClassType
    { loc = loc,
      attributes = infAttributes, 
      tyname = tyname,
      conattributes = infConAttributes, 
      flags = flags,
      superty = superty,
      superarg = (loc,T.Record []), (* dummy *) (*@TODO:revise loc *)
      interfaces = [],
      methods = methoddec,
      localdec = [],
      argpat = T.PatWild, (* dummy *)
      argty = ty
    }
    val TE = Map.insert(Map.empty, tycon, TyStr.makeClasstype mlclass)
  in
    ([withLoc decitem], VETEinE (VEcon,TE))
  end
| _ =>
  Debug.fail "ElabCore.infDecItem: non-core declaration"
end (* infDecItem *)

(*----------------------------------------------------------------------*)
(* Open translation							*)
(*----------------------------------------------------------------------*)
and infOpen C loc [] = 
    ([], emptyE)

  | infOpen C loc (longid::longids) =
    let
      val (E', longid') = EnvLookup.lookupStr (EofC C, loc, longid)
(*      val (d, E'') = infOpen (CplusE C E') loc longids *)
      val (d, E'') = infOpen C loc longids  (*@NOTE: open is parallel, not sequential, binding *)
    in
      (makeOpen (loc, E', longid') @ d, EplusE E' E'')
    end

and infDec C toplevel (dec : Dec) = 
case dec of
(* Rule 23 *)
  [] => 
  ([], emptyE)
    
(* Rule 24 *)
| (onedec as (loc,_))::dec =>
  let
    val (d1, E1) = infDecItem C toplevel onedec
    val (d2, E2) = infDec (CplusE C E1) toplevel dec
  in
    (d1 @ d2, EplusE E1 E2)
  end

(*----------------------------------------------------------------------*)
(* Value Bindings (p26 Defn)						*)
(* In contrast to the definition, we do closure of a type wrt E at this *)
(* point, in order to check the value restriction by looking at the     *)
(* translated term.							*)
(*----------------------------------------------------------------------*)
(* Rule 25 *)
and infValBind U C toplevel loc [] = 
    ([], Map.empty)

  (* For (pat = exp) do pattern compilation and binding *)
  | infValBind U C toplevel loc ((pat as (loc1,_), exp as (loc2,_))::valbind) =
    let

     (*@HACK: we don't generalize variables if in a localdec of a ClassDec *)
      val inClassLocalDec = case classofC C of NONE => false | SOME tyname => true
     
      val (p, VE2, ty1, patValuable) = infPat (CwithClass C NONE) pat
      val (e1, ty1') = infExp (CwithClass C NONE) exp
      val ty1' = unify ((SOME loc1, "val pattern", ty1),
                 (SOME loc2, "val expression", ty1'))
      val Cfvs = freeVE (VEofE (EofC C))
      val tyvarsVE2 = map (SMLTyUnify.resolve loc1 Cfvs) (Map.listItems VE2)

      val tyvars = foldr TyVar.Set.union TyVar.Set.empty tyvarsVE2
      val tyvars = clos Cfvs tyvars
      val tyvars = List.filter 
        (fn tyvar => if TyVar.isExplicit tyvar then TyVar.Set.member(U,tyvar)
          else true) tyvars
      val expValuable = SMLTermOps.isValuable e1
      val restrictedTyvars = 
        if expValuable andalso patValuable andalso 
	   (*@HACK: we don't generalise variables if in a localdec of a ClassDec *)
	   not (inClassLocalDec)
        then tyvars else []
      val VE2' = 
        Map.map (fn ty => VarSch(TypeScheme(restrictedTyvars, ty))) VE2
    in
      if null tyvars orelse (expValuable andalso patValuable andalso not (inClassLocalDec))
         orelse (not (Controls.get valueWarning) 
         andalso not toplevel)
      then () 
      else 
        let val message = (if expValuable 
			   then (if patValuable
				 then "class-local value restriction"
				 else "pattern value restriction")
			   else "value restriction") ^ " prevents type variable generalization"
        in
          error 
          (if toplevel then Error.error (loc1, message)
                       else Error.warning (loc1, message), [])
        end;
			       
      let
        val (d, VE1) = infValBind U C toplevel loc valbind
        val VE = patmerge loc1 (VE1, VE2')
      in
        ((T.Val(loc1, restrictedTyvars, ty1', p, e1))::d, VE)
      end
    end

(* Rule 26 *)
and infValRecBind C loc [] = 
    (Map.empty, [])

  | infValRecBind C loc ((f, exp, ty1) :: valbind) =
    let
      val (e, ty2) = infExp C exp
      val (VE, bindings) = infValRecBind C loc valbind
      val ty3 = unify ((SOME loc, "fun pattern", ty1),
                 (NONE, "fun expression", ty2))
    in
      (Map.insert (VE, f, ty3), (f, e, ty1)::bindings)
    end
  
(*----------------------------------------------------------------------*)
(* Exception Bindings (p25 Defn)					*)
(*----------------------------------------------------------------------*)
(* Rules 30-31 *)
and infExBind C loc [] = 
    ([], Map.empty)

  | infExBind C loc (((_,excon),e)::exbind) =
    let
      val (d, EE) = infExBind C loc exbind
    in
      case e of
        ExDesc NONE => 
        let
          val exname = freshTyName (pathofC C @ [excon], TyName.NotEq)
        in
          (if lamofC C then (T.Exception exname)::d else d, 
          Map.insert(EE, excon, (exnType, exname)))
        end

      | ExDesc (SOME typ) => 
        let
          val ty = infTy C typ
	  val _ = ElabCheck.checkType (loc,"exception argument type contains a byref") ty
          val exname = freshTyName(pathofC C @ [excon], TyName.NotEq)
        in
          (if lamofC C then (T.Exception exname)::d else d, 
          Map.insert(EE, excon, (SMLTy.funType (ty, exnType), exname)))
        end

      | ExBind oplongid =>
        let
          val vid = ElabOps.vidToLongid oplongid
        in
          case EnvLookup.lookupVid (EofC C, loc, vid) of
            NONE => 
            (case EnvLookup.lookupTyCon (EofC C, loc, vid) of
              SOME tystr =>
              let 
                val ty = TyStr.apply(tystr, [])
              in
                if InterOp.sub (ty, SMLPrimTy.exnType)
                then
                let
                  val SOME([],exname) = SMLTy.fromConsType ty
                in
                 ([], Map.insert(EE, excon, (exnType, exname)))
        	end
                else
                (error(Error.error(loc, 
                   "class does not subclass exn: " ^ SMLTy.toString ty), []);
                ([], EE))
              end

            | NONE =>
              (error (Error.error (loc, "unbound exception: " ^
                Longid.toString vid), []); ([], EE))
           )

         | SOME vbind =>
            case vbind of
              (VarSch _) => 
              (error (Error.error (loc,
                "found variable instead of exception: " ^ Longid.toString
                vid), []); ([], EE))

            | ConSch _ => 
              (error (Error.error (loc, 
                "found data constructor instead of exception: " ^ 
                Longid.toString vid), []); ([], EE))

            | ExTy(exbind as (_,exname)) => 
              ([], Map.insert (EE, excon, exbind))

            | Special(classty, _) =>
              let
                val SOME ([], exname) = SMLTy.fromConsType classty
              in
                ([], Map.insert (EE, excon, (exnType, exname)))
              end
        end
      end
and infAttExp C (loc,preattexp as AttApp(exp,namedargs)) = 
    let 
        val (e, ty) = infExp C exp
	val namedargs = map (fn (fieldorproperty,exp) => (fieldorproperty,infExp C exp)) namedargs
	val attributeType = SMLTy.baseType(TyNames.attributeTyName)
        val constraint = TyConstraint.Sub(ty, TyConstraint.RefSub, attributeType) 
	fun addConstraints (Field lab,(e,argty)) =
                  let val defty = freshMono ()
		  in
         	     TyConstraint.add(TyConstraint.Has(ty,lab,
						      (defty,SMLTy.refType(argty,SMLTy.fieldRefType(defty,
												    SMLTy.baseType(TyName.external(Id.fromString "", [lab],0)))))),loc)
		  end
        |   addConstraints (Property lab,(e,argty)) = 
		    let 
			val methty = SMLTy.funType (freshMono (), SMLPrimTy.unitType)
			val usety = SMLTy.funType (argty, SMLPrimTy.unitType)
			val defty = freshMono ()
			val set_lab = 
			    Symbol.symbol(UString.concat
					  [UString.fromString "set_",
					   Symbol.toUString lab])
			val constraint1 = TyConstraint.Has(ty, set_lab, (defty,methty))
			val constraint2 = TyConstraint.Sub(methty, TyConstraint.MethSub,usety)
		        (*@TODO: we only require that lab is a method, not that it is property (as we should).
			 To do this properly requires an extension to the constraint solver, or a final check during
			 translation to MIL. *)
 		    in
			 TyConstraint.add (constraint1,loc)
		       ; TyConstraint.add (constraint2,loc)
		    end
    in  
	  TyConstraint.add (constraint,loc)
	; List.app addConstraints namedargs
	; T.AttApp(loc,e,map (fn (Field lab,(e,ty)) => (T.Field lab,e,ty) 
			      |  (Property lab,(e,ty)) => (T.Property lab,e,ty))
			      namedargs)
    end


end (* of local open *)

end (* of struct *)








