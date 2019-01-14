(*======================================================================*)
(* Check the syntactic restrictions for a declaration expression, and	*)
(* resolve                                                              *)
(*   (a) infix expressions, patterns and fun decs;                      *)
(*   (b) implicitly scoped type variables.                              *)
(*                                                                      *)
(* In more detail, the syntactic restrictions are:                      *)
(* (1) those described in Sections 2.9 and 3.5 of the Defn;             *)
(* (2) our own restrictions on the extensions to ML;	                *)
(* (3) datatype definitions must be uniform, that is, in a definition   *)
(*     datatype ('a_1,...,'a_n) T all occurrences of T must be of the   *)
(*     form ('a_1,...,'a_n) T.                                          *)
(*                                                                      *)
(* Also make sure that the entity type matches the declaration type and *)
(* that the entity name matches the identitier name.			*)  
(*======================================================================*)
structure SyntaxCheck :> SYNTAXCHECK =
struct


open Syntax SyntaxCheckOps

fun spanLoc (({left=l1,right=r1},_),({left=l2,right=r2},_)) = 
    let val left = Int.min(l1,l2)
        val right = Int.max(r1,r2) 
    in
        {left=left,right=right}
    end

val resolveExp = ResolvePrec.parse
  {
    pair = fn (exp1 as ({left,...},_),exp2 as ({right,...},_))=> (spanLoc(exp1,exp2), Tuple [exp1, exp2]),
    asId = fn (loc,LongVid (Short id)) => SOME id
            | _ => NONE,
    apply = fn (exp1 as ({left,...},_),exp2 as ({right,...},_)) => (spanLoc(exp1,exp2),App(exp1,exp2))
  }

val resolvePat = ResolvePrec.parse
  {
    pair = fn (pat1 as (loc,_),pat2) => (spanLoc(pat1,pat2), PatTuple [pat1, pat2]),
    asId = fn (loc,PatVar (Short id)) => SOME id
            | _ => NONE,
    apply = fn (var as (_,PatVar(Short id)),pat) => (spanLoc(var,pat), PatCon([id], pat))
             | (var as (_,PatVar(Long longid)),pat) => (spanLoc(var,pat), PatCon(longid, pat))
             | (var as (_,PatVar(OpShort id)),pat) => (spanLoc(var,pat), PatCon([id], pat))
             | ((loc,_),pat) =>
               raise ResolvePrec.InfixError "non-constructor applied to argument in pattern"
  }

(*----------------------------------------------------------------------*)
(* Check and translate an AST.						*)
(*----------------------------------------------------------------------*)
fun check { AST, sourcemap } =

let

(*----------------------------------------------------------------------*)
(* List the value and type identifiers bound in a specification, paired *)
(* with their source locations.                                         *)
(* Accumulate the results in ids and tycons.                            *)
(*----------------------------------------------------------------------*)
local
  fun bs ([], ids, tycons) = (ids, tycons)
    | bs (((loc,prespecitem) : Syntax.SpecItem) :: rest, ids, tycons) =
  case prespecitem of

  (* Just value identifiers *)
  ValDesc valdesc => 
  bs (rest, map (fn (a,_) => (a,loc)) valdesc @ ids, tycons)

  (* Just type identifiers *)
| (TypeDesc typdesc) => 
  bs (rest, ids, map (fn (_,a,_) => (a,loc)) typdesc @ tycons)

| (EqTypeDesc typdesc) => 
  bs (rest, ids, map (fn (_,a) => (a,loc)) typdesc @ tycons)

  (* Both type (datatype+withtype) and value (constructor) identifiers *)
| DatatypeDesc (datdesc, typbindopt) => 
  bs (rest,
  map (fn ((_,a),_) => (a,loc)) (List.concat (map #3 datdesc)) @ ids, 

   map (fn (_,a,_) => (a,loc)) datdesc @
   (case typbindopt of NONE => []
                    | SOME typbind => map (fn (_,a,_) => (a,loc)) typbind)
   @ tycons)

  (* Just a type identifier; the imported constructors must be checked later *)
| DatatypeDescCopy(id, longid) =>
  bs (rest, ids, (id,loc)::tycons)

  (* Just value identifiers *)
| ExceptionDesc exdesc => 
  bs (rest, map (fn ((_,a),_) => (a,loc)) exdesc @ ids, tycons)

(*
  (* Just a type identifier *)
| ClassDesc (ClassType { modifiers, tycon, super, implements, body })=>
  bs (rest, ids, (tycon,loc) :: tycons)
*)

| _ => 
  bs (rest, ids, tycons)

in
  fun specIds spec = bs (spec, [], [])
end 

(*----------------------------------------------------------------------*)
(* Check type expressions and collect free type variables, accumulating *)
(* them in the last argument.                                           *)
(*----------------------------------------------------------------------*)
fun checkTy level (ty as (loc, prety) : Syntax.Ty) (acc as (errors, tyvars)) =
case prety of
  TyVar tyvar =>
  (errors, Symbol.Map.insert(tyvars, tyvar, level))

| TyCon(tys, longid) =>
  checkTys level tys acc

| TyFun(ty1, ty2) => 
  checkTy level ty1 (checkTy level ty2 acc)

(*......................................................................*)
(* Section 2.9: no type row may bind the same label twice	        *)
(*......................................................................*)
| TyRecord tyrow => 
  let 
    val (labels, tys) = ListPair.unzip tyrow
    val errors = 
      checkDupAtoms (loc, labels, "duplicate labels in record type") errors
  in
    checkTys level tys (errors, tyvars)
  end

| TyTuple tys => 
  checkTys level tys acc

(*----------------------------------------------------------------------*)
(* Check a list of types						*)
(*----------------------------------------------------------------------*)
and checkTys level tys acc =
  foldl (fn (ty, acc) => checkTy level ty acc) acc tys

(*----------------------------------------------------------------------*)
(* Check an optional type						*)
(*----------------------------------------------------------------------*)
fun checkTyOpt level NONE p = p
  | checkTyOpt level (SOME ty) p = checkTy level ty p

(*----------------------------------------------------------------------*)
(* Section 2.9: no tyvarseq may contain the same tyvar twice.      	*)
(*----------------------------------------------------------------------*)
fun checkTyVars (loc, tyvarseq) errors = 
  checkDupAtoms (loc, tyvarseq, "duplicate type variable names") errors

fun getTyCons ids (loc,prety) = 
case prety of
  TyVar _ => []
| TyCon (args, [id]) =>
  List.concat (map (getTyCons ids) args) @
  (if List.exists (fn id' => Symbol.equal (id, id')) ids
  then [(loc,args)]
  else [])
| TyFun(ty1,ty2) => getTyCons ids ty1 @ getTyCons ids ty2
| TyRecord row => List.concat (map (getTyCons ids o #2) row)
(* SL: or *)
(*
| (TyTuple tys | TyCon (tys, _)) => List.concat (map (getTyCons ids) tys)
*)
(*
| TyTuple tys => List.concat (map (getTyCons ids) tys)
| TyCon (tys, _) => List.concat (map (getTyCons ids) tys)
*)
fun substTyCon S (ty as (loc,prety)) =
case prety of
  TyVar _ => ty
| TyCon (args, [id]) =>
  let
    val args = map (substTyCon S) args
    fun find [] = (loc,TyCon(args, [id]))
      | find ((tyvarseq, id', ty)::S) =
        if Symbol.equal(id,id')
        then substTyVar (ListPair.zip(tyvarseq,args)) ty
        else find S
  in
    find S
  end
| TyFun(ty1,ty2) => (loc,TyFun(substTyCon S ty1, substTyCon S ty2))
| TyRecord row => (loc,TyRecord(map (fn (lab,ty) =>(lab,substTyCon S ty)) row))
| TyTuple tys => (loc,TyTuple (map (substTyCon S) tys))
| TyCon(tys,longid) => (loc,TyCon(map (substTyCon S) tys, longid))

and substTyVar S (ty as (loc,prety)) =
case prety of
  TyVar id => 
  let
    fun find [] = ty
      | find ((id',ty)::S) = if Symbol.equal(id,id') then ty else find S
  in
    find S
  end
| TyCon (args, longid) => (loc, TyCon(map (substTyVar S) args, longid))
| TyFun(ty1,ty2) => (loc,TyFun(substTyVar S ty1, substTyVar S ty2))
| TyRecord row => (loc,TyRecord(map (fn (lab,ty) =>(lab,substTyVar S ty)) row))
| TyTuple tys => (loc,TyTuple (map (substTyVar S) tys))

(*----------------------------------------------------------------------*)
(* Check pattern expression and collect free type variables.   		*)
(* The variable ie is bound to the current infix environment.           *)
(*----------------------------------------------------------------------*)
fun checkPat 
  (env as (level,ie))
  (pat as (loc, prepat) : Syntax.Pat) 
  (acc as (errors, tyvars)) =

case prepat of
(* SL: or *)
(*
  (PatWild | PatVar _) =>
  (pat, acc)
*)
   PatWild => (pat, acc)
 | PatVar _ => (pat, acc)

(*......................................................................*)
(* Section 2.9: no pattern row may bind the same lab twice.             *)
(*......................................................................*)
| PatRecord (flex, patrow) =>
  let 
    val (labels, pats) = ListPair.unzip patrow
    val errors = 
      checkDupAtoms (loc, labels, "duplicate labels in record pattern") errors
    val (pats, acc) = checkPats env pats (errors, tyvars)
  in
    ((loc,PatRecord (flex, ListPair.zip(labels, pats))), acc)
  end

| PatConstraint(pat, ty) => 
  let
    val (pat, acc) = checkPat env pat (checkTy level ty acc)
  in
    ((loc,PatConstraint(pat, ty)), acc)
  end

| PatCast(id, ty) => 
  let
    val acc = checkTy level ty acc
  in
    ((loc,PatCast(id, ty)), acc)
  end

| PatLayer(id, tyopt, pat) => 
  let
    val (pat, acc) = checkPat env pat (checkTyOpt level tyopt acc)
  in
    ((loc,PatLayer(id, tyopt, pat)), acc)
  end

| PatTuple pats =>
  let
    val (pats, acc) = checkPats env pats acc
  in
    ((loc,PatTuple pats), acc)
  end

| PatList pats => 
  let
    val (pats, acc) = checkPats env pats acc
  in
    ((loc,PatList pats), acc)
  end

| OrPat pats => 
  let
    val (pats, acc) = checkPats env pats acc
    val acc = ((Error.warning(loc, "non-portable OR pattern "))::(#1 acc),#2 acc)
  in
    ((loc,OrPat pats), acc)
  end

| PatParen pat =>
  checkPat env pat acc


(*......................................................................*)
(* Section 2.9: no real constant ma => (pat, acc)y occur in a pattern			*)
(*......................................................................*)
| PatSCon (SCon.RealCon _) =>
  (pat, (Error.error(loc, "real constant in pattern")::errors, tyvars))

| PatSCon _ =>
  (pat, acc)

| PatCon _ =>
  Debug.fail "SyntaxCheck.checkPat: found unary constructor"

| FlatPat pats =>
  let
    val (pats, acc as (errors,tyvars)) = checkPats env pats acc
  in
    (resolvePat (ie, pats), acc) handle 
     ResolvePrec.InfixError s =>
    (pat, (Error.error(loc, s)::errors, tyvars))
  end

(*----------------------------------------------------------------------*)
(* Check several patterns.						*)
(*----------------------------------------------------------------------*)
and checkPats env [] acc = ([], acc)
  | checkPats env (pat::pats) acc =
    let
      val (pat,acc) = checkPat env pat acc
      val (pats,acc) = checkPats env pats acc
    in
      (pat::pats, acc)
    end

(*----------------------------------------------------------------------*)
(* Syntactic restrictions on type/datatype declarations.         	*)
(*----------------------------------------------------------------------*)
fun checkTypBind level (loc, datbindopt, typbindopt) (acc as (errors, tyvars))=
let 
  val tycons1 =
    case datbindopt of 
      NONE => [] 
    | SOME datbind => map (fn (x,y,z) => (x,y)) datbind

  val tycons2 = 
    case typbindopt of 
      NONE => []
    | SOME typbind => map (fn (x,y,z) => (x,y)) typbind

  val (tyvarseqs, tycons) = ListPair.unzip(tycons1 @ tycons2)

(*
  val errors =
    case datbindopt of
      NONE => errors
    | SOME datbind => 
      let
        val S = getOpt(typbindopt, [])
        val ids = map #2 tycons1
      in
        List.concat (map (fn (tyvarseq, tycon, conbinds) =>
        let 
          val argss = 
            List.concat 
              (map (fn (_,NONE) => [] | (_,SOME ty) => 
                getTyCons ids (substTyCon S ty)) conbinds)
          fun checkArgs (loc,args) =
          let
            fun check ([],[]) = true
              | check (tyvar::tyvars, (_,TyVar tyvar')::tys) =
                Symbol.equal(tyvar,tyvar') andalso check (tyvars, tys)
              | check _ = false
          in
            if check (tyvarseq, args)
            then NONE
            else SOME (Error.error(loc, 
              "datatype is used non-uniformly: "
              ^ Pretty.simpleVec "," Id.toString tyvarseq ^ " against "
              ^ Int.toString (length args)))
          end              
        in
          List.mapPartial checkArgs argss
        end) datbind) @ errors
      end
*)
      
  (*..................................................................*)
  (* No typbind/datbind may bind the same type identifier twice.      *)
  (*..................................................................*)
  val errors = checkDupAtoms (loc, tycons, "duplicate type definition") errors

  (*..................................................................*)
  (* Type variables must be distinct				      *)
  (*..................................................................*)
  val errors = foldl (fn (tyvarseq,errors) => 
    checkTyVars (loc, tyvarseq) errors) errors tyvarseqs

  val (cons, tyopts) = 
    case datbindopt of 
      NONE => ([], [])
    | SOME datbind => ListPair.unzip(List.concat (map #3 datbind))

  (*..................................................................*)
  (* No datbind may bind the same value identifier twice.	      *)
  (*..................................................................*)
  val errors = checkDupAtoms (loc, map (fn (_,a) => a) cons, 
    "duplicate constructor names in datatype declaration") errors

  val tyopts = 
    case typbindopt of 
      NONE => tyopts 
    | SOME typbind => map (SOME o #3) typbind @ tyopts

in
  foldl (fn (tyopt, acc) => checkTyOpt level tyopt acc) (errors, tyvars) tyopts
end

(*----------------------------------------------------------------------*)
(* Syntactic restrictions on expressions.                   		*)
(*----------------------------------------------------------------------*)
fun checkExp 
  (env as (level,ie)) 
  (exp as (loc, preexp) : Exp) 
  (acc as (errors, tyvars)) =

case preexp of
  LongVid _ =>
  (exp, acc)

| SCon _ =>
  (exp, acc)

| App _ =>
  Debug.fail "SyntaxCheck.checkExp: function application found"

| FlatApp exps =>
  let
    val (exps,acc as (errors, tyvars)) = checkExps env exps acc
  in
     (resolveExp (ie,exps), acc) 
     handle ResolvePrec.InfixError s => 
     (exp, (Error.error(loc, s)::errors, tyvars))
  end

| Fn match => 
  let
    val (match, acc) = checkMatch env match acc
  in
    ((loc, Fn match), acc)
  end

| Let(dec, exp) =>
  let
    val (dec, acc, ie) = checkDec env dec acc
    val (exp, acc) = checkExp (level,ie) exp acc
  in
    ((loc, Let(dec, exp)), acc)
  end

| LetUnless(dec, exp, match) =>
  let
    val (dec, acc, ie) = checkDec env dec acc
    val (exp, acc) = checkExp (level,ie) exp acc
    val (match, acc) = checkMatch env match acc
  in
    ((loc, LetUnless(dec, exp, match)), acc)
  end

| Constraint(exp, ty) => 
  let
    val (exp, acc) = checkExp env exp acc
    val acc = checkTy level ty acc
  in
    ((loc, Constraint(exp, ty)), acc)
  end

| ConstraintGt(exp, ty) => 
  let
    val (exp, acc) = checkExp env exp acc
    val acc = checkTy level ty acc
  in
    ((loc, ConstraintGt(exp, ty)), acc)
  end

| Handle(exp, match) => 
  let
    val (exp, acc) = checkExp env exp acc
    val (match, acc) = checkMatch env match acc
  in
    ((loc, Handle(exp, match)), acc)
  end

| Raise exp => 
  let
    val (exp, acc) = checkExp env exp acc
  in
    ((loc, Raise exp), acc)
  end

| Pure exp => 
  let
    val (exp, acc) = checkExp env exp acc
  in
    ((loc, Pure exp), acc)
  end

| Synchronized(exp1,exp2) =>
  let
    val (exp1, acc) = checkExp env exp1 acc
    val (exp2, acc) = checkExp env exp2 acc
  in
    ((loc, Synchronized(exp1,exp2)), acc)
  end

| ClassWith(exp,methbind) =>
  let
    val (exp, acc) = checkExp env exp acc
    val (methbind, acc) = checkMethBind env methbind acc
  in
    ((loc, ClassWith(exp,methbind)), acc)
  end

(*......................................................................*)
(* Section 2.9: no expression row may bind the same lab twice.		*)
(*......................................................................*)
| Record exprow => 
  let 
    val (labels, exps) = ListPair.unzip exprow
    val errors = 
      checkDupAtoms (loc, labels, "duplicate labels in record") errors
    val (exps, acc) = checkExps env exps (errors, tyvars)
  in
    ((loc, Record(ListPair.zip(labels, exps))), acc)
  end

| Tuple exps =>
  let
    val (exps, acc) = checkExps env exps acc
  in
    ((loc, Tuple exps), acc)
  end

(* SL: or *)
(*
| (Hash _ | DotHash _ | DotHashHash _) =>
  (exp, acc)
*)
| Hash _ => (exp, acc)
| DotHash _ => (exp, acc)
| DotHashHash _ => (exp, acc)

| Case(exp, match) =>
  let
    val (exp, acc) = checkExp env exp acc
    val (match, acc) = checkMatch env match acc
  in
    ((loc, Case(exp, match)), acc)
  end

| If (exp1, exp2, exp3) =>
  let
    val (exp1, acc) = checkExp env exp1 acc
    val (exp2, acc) = checkExp env exp2 acc
    val (exp3, acc) = checkExp env exp3 acc
  in
    ((loc, If(exp1, exp2, exp3)), acc)
  end

| Orelse (exp1, exp2) =>
  let
    val (exp1, acc) = checkExp env exp1 acc
    val (exp2, acc) = checkExp env exp2 acc
  in
    ((loc, Orelse(exp1, exp2)), acc)
  end

| Andalso (exp1, exp2) =>
  let
    val (exp1, acc) = checkExp env exp1 acc
    val (exp2, acc) = checkExp env exp2 acc
  in
    ((loc, Andalso(exp1, exp2)), acc)
  end

| Sequence exps =>
  let
    val (exps, acc) = checkExps env exps acc
  in
    ((loc, Sequence exps), acc)
  end

| While (exp1, exp2) =>
  let
    val (exp1, acc) = checkExp env exp1 acc
    val (exp2, acc) = checkExp env exp2 acc
  in
    ((loc, While(exp1, exp2)), acc)
  end

| List exps =>
  let
    val (exps, acc) = checkExps env exps acc
  in
    ((loc, List exps), acc)
  end

and checkExps env [] acc = ([], acc)
  | checkExps env (exp::exps) acc =
    let
      val (exp,acc) = checkExp env exp acc
      val (exps,acc) = checkExps env exps acc
    in
      (exp::exps, acc)
    end

(*----------------------------------------------------------------------*)
(* Check a match							*)
(*----------------------------------------------------------------------*)
and checkMatch env [] acc = 
    ([], acc)

  | checkMatch env ((pat,exp)::match) acc = 
    let
      val (pat,acc) = checkPat env pat acc
      val (exp, acc) = checkExp env exp acc
      val (match, acc) = checkMatch env match acc
    in
      ((pat,exp)::match, acc)
    end

and makeBound level ({ explicit, implicit }, freetyvars) = 
  { explicit = explicit, 
    implicit = 
      List.filter 
      (fn tyvar => not (List.exists 
        (fn tyvar' => Symbol.equal(tyvar,tyvar')) explicit))
      (map #1 (Symbol.Map.listItemsi 
        (Symbol.Map.filter (fn level' => level' = level+1) freetyvars)))
  }

(*----------------------------------------------------------------------*)
(* Check a declaration item.               			        *)
(* Return a transformed decitem, accumulated type variables/errors list *)
(* and a new infix environment.                                         *)
(*----------------------------------------------------------------------*)
and checkDecItem 
  (env as (level,ie)) 
  (decitem as (loc, predecitem) : Syntax.DecItem) 
  (acc as (errors, tyvars)) =

case predecitem of
  Val(boundtyvars, match) =>
  let
    val (match, acc as (errors,freetyvars)) = checkMatch (level+1,ie) match acc
  in
    ((loc, Val(makeBound level (boundtyvars, freetyvars), match)), acc, ie)
  end
          
| ValRec(boundtyvars, match) =>
  let
    val (match, acc as (errors,freetyvars)) = checkMatch (level+1,ie) match acc
  in
    ((loc, ValRec(makeBound level (boundtyvars, freetyvars), match)), acc, ie)
  end

| FlatFun(boundtyvars, fvalbinds) =>
  let
    val (fvalbinds, acc as (errors,freetyvars)) = 
      checkFValBind (level+1,ie) fvalbinds acc
  in
    ((loc, Fun(makeBound level (boundtyvars, freetyvars), fvalbinds)), acc, ie)
  end

| Fun _ =>
  Debug.fail "SyntaxCheck.checkDecItem: found fun dec"

| Type typbind =>
  (decitem, checkTypBind level (loc, NONE, SOME typbind) acc, ie)

| Datatype (datbind, typbindopt) =>
  (decitem, checkTypBind level (loc, SOME datbind, typbindopt) acc, ie)

| DatatypeCopy _ =>
  (decitem, acc, ie)
  
| Abstype (datbind, typbindopt, dec) =>
  let
    val acc = checkTypBind level (loc, SOME datbind, typbindopt) acc
    val (dec, acc, ie) = checkDec env dec acc
  in
    ((loc, Abstype (datbind, typbindopt, dec)), acc, ie)
  end

| Exception exbind =>
  let
    val errors = checkDupAtoms (loc, map (#2 o #1) exbind, 
      "duplicate exception declaration") errors
    val acc = checkExBind level exbind (errors,tyvars)
  in
    (decitem, acc, ie)
  end

| Local (dec1, dec2) =>
  let
    val (dec1, acc, ie1) = checkDec env dec1 acc
    val (dec2, acc, ie2) = checkDec (level, ie1) dec2 acc
  in
    ((loc, Local (dec1, dec2)), acc, ie2)
  end

| Open strids =>
  (decitem, acc, ie)

| ClassDec classdec =>
  let
    val (classdec, acc) = checkClassDec false env (loc, classdec) acc
  in
    ((loc, ClassDec classdec), acc, ie)
  end

| Structure strbinds =>
  let
    val errors = 
      checkDupAtoms (loc, map #1 strbinds, 
        "duplicate structure identifiers") errors

    val (strbinds, acc) = 
      foldr (fn (strbind, (strbinds,acc)) =>
      let 
        val (strbind,acc) = checkStrBind env strbind acc
      in
        (strbind::strbinds, acc)
      end) ([],(errors,tyvars)) strbinds
  in
    ((loc, Structure strbinds), acc, ie)
  end

| Signature sigbinds =>
  let
    val errors = 
      checkDupAtoms (loc, map #1 sigbinds, 
        "duplicate signature identifiers") errors

    val (sigbinds, acc) = 
      foldr (fn (sigbind, (sigbinds,acc)) =>
      let 
        val (sigbind,acc) = checkSigBind env sigbind acc
      in
        (sigbind::sigbinds, acc)
      end) ([],(errors,tyvars)) sigbinds
  in
    ((loc, Signature sigbinds), acc, ie)
  end

| Functor funbinds =>
  let
    val errors =
      checkDupAtoms (loc, map #1 funbinds, 
        "duplicate functor identifiers") errors
  
    val (funbinds, acc) = 
      foldr (fn (funbind, (funbinds,acc)) =>
      let 
        val (funbind,acc) = checkFunBind env funbind acc
      in
        (funbind::funbinds, acc)
      end) ([],(errors,tyvars)) funbinds
  in
    ((loc, Functor funbinds), acc, ie)
  end
      
| Infix (prec, symbols) =>
  (decitem, acc, Fixity.updateEnv(ie, symbols, Fixity.Infix(prec,false)))

| Infixr (prec, symbols) =>
  (decitem, acc, Fixity.updateEnv(ie, symbols, Fixity.Infix(prec,true)))

| Nonfix symbols =>
  (decitem, acc, Fixity.updateEnv(ie, symbols, Fixity.Nonfix))

and checkDec (env as (level,ie)) [] acc = ([], acc, ie)
  | checkDec (env as (level,ie)) (decitem::dec) acc = 
    let
      val (decitem,acc,ie1) = checkDecItem env decitem acc
      val (dec,acc,ie2) = checkDec (level, ie1) dec acc
    in
      (decitem::dec, acc, ie2)
    end

and checkStrBind env (strid, strexp, siginfo) acc =
let
  val (strexp, acc) = checkStrExp env strexp acc
  val (siginfo, acc) = checkSigInfo env siginfo acc
in
  ((strid, strexp, siginfo), acc)
end

and checkFunBind env (funid, funarg, siginfo, strexp) acc =
let
  val (funarg, acc) = checkFunArg env funarg acc
  val (strexp, acc) = checkStrExp env strexp acc
  val (siginfo, acc) = checkSigInfo env siginfo acc
in
  ((funid, funarg, siginfo, strexp), acc)
end

and checkFunArg env funarg acc =
case funarg of
  StructArg(id, sigexp) =>
  let
    val (sigexp, acc) = checkSigExp env sigexp acc
  in
    (StructArg(id, sigexp), acc)
  end

| SpecArg spec =>
  let
    val (spec, acc) = checkSpec env spec acc
  in
    (SpecArg spec, acc)
  end

and checkSigBind env (sigid, sigexp) acc =
let
  val (sigexp, acc) = checkSigExp env sigexp acc
in
  ((sigid, sigexp), acc)
end

and checkSigInfo env SigNone acc = 
    (SigNone, acc)

  | checkSigInfo env (SigConcrete sigexp) acc = 
    let 
      val (sigexp, acc) = checkSigExp env sigexp acc
    in 
      (SigConcrete sigexp, acc) 
    end

  | checkSigInfo env (SigAbstract sigexp) acc = 
    let 
      val (sigexp, acc) = checkSigExp env sigexp acc     
    in 
      (SigAbstract sigexp, acc) 
    end

(*----------------------------------------------------------------------*)
(* Check a class declaration						*)
(*----------------------------------------------------------------------*)
and checkClassDec insig (env as (level,ie)) (loc,classdec) 
  (acc as (errors, tyvars)) =
case classdec of
  (*..................................................................*)
  (* Check that the class modifiers are distinct (8.1,JLS) and do     *)
  (* not contain both _abstract and _final (8.1.2.2, JLS).            *) 
  (*..................................................................*)
  ClassType {attributes,modifiers,conattributes,tycon, pat, inherits, localdec, methoddec} =>
  let
    val errors = 
      checkDupAtoms (loc, modifiers, "duplicate class modifiers") errors
 
    fun checkInherits (Implements ty) acc = 
        let 
          val acc = checkTy level ty acc
        in
          (Implements ty, acc)
        end

      | checkInherits (Extends (ty,exp)) acc =
        let 
          val acc = checkTy level ty acc
          val (exp, acc) = checkExp env exp acc
        in
          (Extends (ty,exp), acc)
        end           

    fun checkInheritss [] acc = ([], acc)
      | checkInheritss (x::xs) acc =
        let
          val (x',acc) = checkInherits x acc
          val (xs',acc) = checkInheritss xs acc
        in
          (x'::xs',acc)
        end      

(*
    val errors = 
      if List.exists (fn m => m=Syntax.ABSTRACT) modifiers
        andalso List.exists (fn m => m=Syntax.FINAL) modifiers
      then Error.error(loc,"class is both abstract and final")::errors
      else errors
*)

    val (attributes, errors) = checkAttributes env attributes errors
    val (conattributes, errors) = checkAttributes env conattributes errors
    val acc = (errors,tyvars)
    val (pat, acc) = checkPat env pat acc
    val (inherits, acc) = checkInheritss inherits acc
    val (localdec, acc, ie) = checkDec env localdec acc
    val (methoddec, acc as (errors,freetyvars)) = 
      checkMethBind (level+1,ie) methoddec acc
  in
    (ClassType {attributes = attributes, modifiers = modifiers, 
 		conattributes = conattributes, tycon = tycon, inherits = inherits,
		pat = pat, localdec = localdec, methoddec = methoddec }, acc)
  end
(*----------------------------------------------------------------------*)
(* Check a class description						*)
(*----------------------------------------------------------------------*)
| DelegateType {attributes,modifiers,conattributes,tycon,ty} =>
  let
    val errors = checkDupAtoms (loc, modifiers, "duplicate class modifiers") errors
    val (attributes, errors) = checkAttributes env attributes errors
    val (conattributes, errors) = checkAttributes env conattributes errors
    val acc = checkTy level ty (errors,tyvars)
  in
    (DelegateType {attributes = attributes, modifiers = modifiers, 
		   conattributes = conattributes, tycon = tycon,
		   ty=ty}, acc)
  end

and checkClassDesc insig (env as (level,ie)) (loc,classdesc) 
  (acc as (errors, tyvars)) =

case classdesc of
  (*..................................................................*)
  (* Check that the class modifiers are distinct (8.1,JLS) and do     *)
  (* not contain both _abstract and _final (8.1.2.2, JLS).            *) 
  (*..................................................................*)
  ClassTypeSpec {modifiers, tycon, conty, inherits, methodspec} =>
  let
    val errors = 
      checkDupAtoms (loc, modifiers, "duplicate class modifiers") errors
 
(*
    val errors = 
      if List.exists (fn m => m=Syntax.ABSTRACT) modifiers
        andalso List.exists (fn m => m=Syntax.FINAL) modifiers
      then Error.error(loc,"class is both abstract and final")::errors
      else errors
*)

    val acc = (errors,tyvars)

    val acc = checkTyOpt level conty acc
    val acc = checkTys 0 (map #2 methodspec) acc
    val acc = checkTys 0 inherits acc
  in
    (ClassTypeSpec { modifiers = modifiers, tycon = tycon, conty = conty,
       inherits = inherits, methodspec = methodspec }, acc)
  end

(*----------------------------------------------------------------------*)
(* Check a flat fun binding and produce a non-flat one.			*)
(*----------------------------------------------------------------------*)
and checkFValBind (env as (level,ie)) [] acc = ([], acc)
  | checkFValBind (env as (level,ie)) fvalbind acc =
let 
  fun checkOneFValBind binds acc =
  let
    fun check (fvar,len) [] acc = ([], acc)

      | check (fvar,len) ((loc, pats, exp, tyopt)::rest) acc =         
        let 
          val (exp, acc) = checkExp env exp acc
          val acc = checkTyOpt level tyopt acc
          fun default () =
            case pats of

              (* Expected: 
		   f pat1 ... patn
                   op f pat1 ... patn *)
(* SL: or *)
(*
              (_,PatVar(Short f | OpShort f))::pats => 
              let
                val (pats, acc) = checkPats env pats acc
              in
                (SOME f, pats, acc)
              end
*)
              ((_,PatVar(Short f))::pats) => 
              let
                val (pats, acc) = checkPats env pats acc
              in
                (SOME f, pats, acc)
              end
	    | ((_,PatVar(OpShort f))::pats) => 
              let
                val (pats, acc) = checkPats env pats acc
              in
                (SOME f, pats, acc)
              end

              (* Anything else is an error *)
            | (loc,_)::pats =>
              let
                val (pats, (errors,tyvars)) = checkPats env pats acc
                val errors = 
                  Error.error(loc, "expected function identifier")::errors
              in
                (NONE, pats, (errors,tyvars))
              end

          val (fvar', pats, acc) =
            case pats of

              (* Check for infix operator of form x ++ y ... *)
(* SL: or *)
(*
              (pat1::(loc,PatVar(Short f))::pat2::pats 
              | (_,PatParen (_,FlatPat [pat1, (loc,PatVar(Short f)), pat2]))
                :: pats) =>
              (case Fixity.lookup(ie, f) of
                Fixity.Infix _ =>
                let
                  val (pat1, acc) = checkPat env pat1 acc
                  val (pat2, acc) = checkPat env pat2 acc
                  val (pats, acc) = checkPats env pats acc
                in
                  (SOME f, (loc,PatTuple [pat1,pat2])::pats, acc)
                end
*)
              (pat1::(loc,PatVar(Short f))::pat2::pats) =>
              (case Fixity.lookup(ie, f) of
                Fixity.Infix _ =>
                let
                  val (pat1, acc) = checkPat env pat1 acc
                  val (pat2, acc) = checkPat env pat2 acc
                  val (pats, acc) = checkPats env pats acc
                in
                  (SOME f, (loc,PatTuple [pat1,pat2])::pats, acc)
                end
              | _ => 
                default ())
            | ((_,PatParen (_,FlatPat [pat1, (loc,PatVar(Short f)), pat2]))
                :: pats) =>
              (case Fixity.lookup(ie, f) of
                Fixity.Infix _ =>
                let
                  val (pat1, acc) = checkPat env pat1 acc
                  val (pat2, acc) = checkPat env pat2 acc
                  val (pats, acc) = checkPats env pats acc
                in
                  (SOME f, (loc,PatTuple [pat1,pat2])::pats, acc)
                end
              | _ => 
                default ())

            | (_,FlatPat pats)::pats' =>
              let
                val (pats, acc) = checkPats env pats acc
                val (pats', acc) = checkPats env pats' acc
                val pat = resolvePat (ie, pats)
              in
                case pat of
                  (_,PatCon([f],pat)) => 
                  (SOME f, pat::pats', acc)
              end        

            | _ => 
              default ()

          val (rest, acc as (errors,tyvars)) = 
            check (fvar',length pats) rest acc        

          val errors = 
            if null pats 
            then Error.error(loc, "missing arguments in function declaration")
              ::errors
            else errors
        in
          case fvar' of
            NONE => (rest, (errors, tyvars))
          | SOME fvar' =>
            let
              val errors =
              case fvar of 
                NONE => errors
              | SOME fvar => 
                if not(Symbol.equal(fvar,fvar'))
                then Error.error(loc,"clauses don't all have function name " ^ 
                  Id.toString fvar)::errors
                else if len = length pats 
                then errors
                else Error.error(loc, 
                  "clauses don't all have same number of patterns")::errors
            in
              ((loc, fvar', pats, exp, tyopt)::rest, 
              (errors, tyvars))
            end
        end
  in
    check (NONE,0) binds acc
  end

  fun checkAll [] acc = ([], acc)
    | checkAll (bind::binds) acc =
      let
        val (bind, acc) = checkOneFValBind bind acc
        val (binds, acc) = checkAll binds acc
      in
        (bind::binds, acc)
      end

  val (fvalbind, (errors,tyvars)) = checkAll fvalbind acc
  val validfvalbind = List.filter (not o null) fvalbind
  val errors =
    case validfvalbind of
      [] => errors
    | x::xs =>
      checkDupAtoms (#1 (hd x), map (#2 o hd) validfvalbind, 
        "duplicate function names in fun declaration") errors
in
  (fvalbind, (errors,tyvars))
end

(*----------------------------------------------------------------------*)
(* Check a set of method bindings.					*)
(*----------------------------------------------------------------------*)
and checkMethBind (env as (level,ie)) [] acc = ([], acc)
  | checkMethBind (env as (level,ie)) (methbind : Syntax.MethBind list) acc =
let 
  fun checkOneMethBind binds acc =
  let
    val acc  =
	(* check binds contain exactly one abstract clause or n>=1 concrete clauses *)
	case binds of 
	           [] => Debug.fail "SyntaxCheck: found zero-length method bindings" 
	         | [(loc,fvar,NONE,SOME _)] => acc
		 | _ =>
		    case List.find (fn(loc,fvar,_,SOME _) => true | _ => false) binds of
  		      NONE => acc
 		    | SOME(loc,_,_,_) =>
			    (Error.error(loc,"concrete method clauses contain an illegal abstract clause") :: #1 acc,
			     #2 acc)
		    
    fun check fvar [] acc = ([], acc)
      | check fvar ((loc, fvar', bodyopt, tyopt)::rest) acc =         
        let 
	  val acc =  case fvar of 
	                  NONE => acc
	                | SOME fvar =>
			   if not(Symbol.equal(fvar,fvar'))
			      then (Error.error(loc,"clauses don't all have same method name " ^ 
						Id.toString fvar):: #1 acc,
				    #2 acc)
			   else acc
          val (bodyopt, acc) = 
          case bodyopt of
            NONE => (NONE, acc)
          | SOME (pat,exp) =>
            let val (exp,acc) = checkExp env exp acc
                val (pat,acc) = checkPat env pat acc
            in (SOME (pat,exp), acc) end

          val acc = checkTyOpt level tyopt acc
          val (rest, acc as (errors,tyvars)) = check (SOME fvar') rest acc    

        in
          ((loc,fvar',bodyopt,tyopt)::rest, acc) 
        end
  in
    check NONE binds acc
  end

  fun checkAll [] acc = ([], acc)
    | checkAll ((attributes,mods,bind)::binds) (acc as (errors,tyvars))  =
      let
        val (attributes,errors) = checkAttributes env attributes errors
        val (bind, acc) = checkOneMethBind bind (errors,tyvars)
        val (binds, acc) = checkAll binds acc
      in
        ((attributes,mods,bind)::binds, acc)
      end

in
  checkAll methbind acc
end

(*----------------------------------------------------------------------*)
(* Syntactic restrictions on CLR Attributes.		                *)
(*@TODO: should we allow/accumulate free type variables? 	        *)
(*----------------------------------------------------------------------*)
and checkAttribute env (loc,AttApp(exp,exprow)) errors =
  let 
    val (exp,(errors,tyvars)) = checkExp env exp (errors,Symbol.Map.empty)
    val (fieldsandproperties, exps) = ListPair.unzip exprow
    val fields = List.foldr (fn (Field s,fields) => s :: fields | (_,fields) => fields) [] fieldsandproperties
    val properties = List.foldr (fn (Property s,properties) => s :: properties | (_,properties) => properties) [] fieldsandproperties
    val errors = checkDupAtoms (loc, fields, "duplicate field initialiser") errors
    val errors = checkDupAtoms (loc, properties, "duplicate property initialiser") errors
    val (exps, (errors,tyvars)) = checkExps env exps (errors, tyvars)
    val errors = if Symbol.Map.numItems(tyvars)=0
	 then errors
	 else Error.error(loc, 
			   "free type variables in attribute expression")
	       ::errors
  in
     ((loc,AttApp(exp,ListPair.zip(fieldsandproperties,exps))), errors)
  end

and checkAttributes env [] acc = ([],acc)
  | checkAttributes env (att::atts) acc =
    let
      val (att,acc) = checkAttribute env att acc
      val (atts,acc) = checkAttributes env atts acc
    in
      ((att::atts), acc)
    end

(*----------------------------------------------------------------------*)
(* Syntactic restrictions on exception declarations.		        *)
(* Section 2.9: no binding exbind may bind the same identifier twice.   *)
(*----------------------------------------------------------------------*)
and checkExBind level [] acc = acc
  | checkExBind level ((_,bind)::rest) acc =
    case bind of
      ExDesc tyopt => checkExBind level rest (checkTyOpt level tyopt acc)
    | _ => checkExBind level rest acc

(*----------------------------------------------------------------------*)
(* Check a specification item						*)
(*----------------------------------------------------------------------*)
and checkSpecItem env (specitem as (loc,prespecitem) : Syntax.SpecItem) acc =
case prespecitem of
  ValDesc valdesc => 
  (specitem, checkTys 0 (map #2 valdesc) acc)

| TypeDesc typdesc => 
  (specitem, foldl (fn ((tyvarseq,_,tyopt),acc as (errors,tyvars)) =>
    checkTyOpt 0 tyopt (checkTyVars (loc,tyvarseq) errors, tyvars)) 
    acc typdesc)
       
| EqTypeDesc eqtypdesc =>
  (specitem, foldl (fn ((tyvarseq,_),acc as (errors,tyvars)) =>
    (checkTyVars (loc,tyvarseq) errors,tyvars)) acc eqtypdesc)

| DatatypeDesc (datbind, typbindopt) =>
  (specitem, checkTypBind 0 (loc, SOME datbind, typbindopt) acc)

| DatatypeDescCopy _ =>
  (specitem, acc)

| ExceptionDesc exdesc =>
  (specitem, foldl (fn ((_,tyopt),acc) => checkTyOpt 0 tyopt acc) acc exdesc)

| ClassDesc desc =>
  let 
    val (desc, acc) = checkClassDesc true env (loc,desc) acc
  in 
    ((loc,ClassDesc desc), acc)
  end

| StructureDesc strdesc =>
  let
    val (strdesc, acc) = 
      foldr (fn ((strid,sigexp), (strdesc,acc)) =>
      let
        val (sigexp,acc) = checkSigExp env sigexp acc
      in
        ((strid,sigexp)::strdesc, acc)
      end) ([],acc) strdesc
  in 
    ((loc,StructureDesc strdesc), acc)
  end

| Include sigexp =>
  let
    val (sigexp, acc) = checkSigExp env sigexp acc
  in
    ((loc,Include sigexp), acc)
  end

| Sharing longids =>
  (specitem, acc)

| SharingType longids =>
  (specitem, acc)

(*----------------------------------------------------------------------*)
(* Check a specification						*)
(*----------------------------------------------------------------------*)
and checkSpec env spec (errors,tyvars) =
let 
  val (vars, tycons) = specIds spec

  (*..................................................................*)
  (* No description may describe the same value identifier twice      *)
  (*..................................................................*)
(*
  val errors =
    checkDupLocAtoms 
    (vars, "duplicate specifications for variable or constructor") errors
*)

  (*..................................................................*)
  (* No description may describe the same type identifier twice       *)
  (*..................................................................*)
  val errors = 
    checkDupLocAtoms 
    (tycons, "duplicate specifications for type constructor") errors

in
  foldr (fn (specitem, (spec,acc)) =>
    let
      val (specitem,acc) = checkSpecItem env specitem acc
    in
      (specitem::spec, acc)
    end) ([], (errors,tyvars)) spec
end

(*----------------------------------------------------------------------*)
(* Check a signature expression						*)
(*----------------------------------------------------------------------*)
and checkSigExp env (sigexp as (loc,presigexp) : Syntax.SigExp) acc =
case presigexp of
  SigSpec spec => 
  let
    val (spec, acc) = checkSpec env spec acc
  in
    ((loc,SigSpec spec), acc)
  end

| Sigid _ =>
  (sigexp, acc)

| Where(sigexp, tyvars, longtycon, ty) =>
  let
    val (sigexp, acc) = checkSigExp env sigexp acc
    val acc = checkTy 0 ty acc
  in
    ((loc, Where(sigexp, tyvars, longtycon, ty)), acc)
  end

(*----------------------------------------------------------------------*)
(* Check a structure expression						*)
(*----------------------------------------------------------------------*)
and checkStrExp 
  (env as (level,ie)) 
  (strexp as (loc,prestrexp) : Syntax.StrExp) 
  acc =
case prestrexp of
  Struct dec => 
  let
    val (dec, acc, ie) = checkDec env dec acc
  in
    ((loc, Struct dec), acc)
  end

| Strid longid =>
  (strexp, acc)

| StrTransparent(strexp, sigexp) => 
  let
    val (strexp, acc) = checkStrExp env strexp acc
    val (sigexp, acc) = checkSigExp env sigexp acc
  in
    ((loc, StrTransparent(strexp, sigexp)), acc)
  end

| StrOpaque(strexp, sigexp) => 
  let
    val (strexp, acc) = checkStrExp env strexp acc
    val (sigexp, acc) = checkSigExp env sigexp acc
  in
    ((loc, StrOpaque(strexp, sigexp)), acc)
  end

| FunApp(id, strexp) => 
  let
    val (strexp, acc) = checkStrExp env strexp acc
  in
    ((loc, FunApp(id, strexp)), acc)
  end

| StrLet(dec, strexp) => 
  let
    val (dec, acc, ie) = checkDec env dec acc
    val (strexp, acc) = checkStrExp (level,ie) strexp acc
  in
    ((loc, StrLet(dec, strexp)), acc)
  end


fun checkSource(AST) =
let
  val msg = "(compiler restriction: a source file must be a sequence of distinct structure, signature or functor bindings)"

  fun loop ([],strids,sigids,funids,errors) = errors
    | loop ((decitem as (loc, predecitem))::rest,strids,sigids,funids,errors) =
      case predecitem of
        Structure bindings =>
	let val strids = strids @ (map #1 bindings)
	    val errors = checkDupAtoms (loc, strids,"duplicate top-level structure bindings")  errors
	in	
           loop (rest,strids,sigids,funids,errors)
	end
      | Signature bindings =>
	let val sigids = sigids @ (map #1 bindings)
	    val errors = checkDupAtoms (loc, strids,"duplicate top-level signature bindings")  errors
        in
           loop (rest,strids,sigids,funids,errors)
	end
      | Functor bindings =>
	let val funids = funids @ (map #1 bindings)
	    val errors = checkDupAtoms (loc, funids,"duplicate top-level functor bindings")  errors
	in
           loop (rest,strids,sigids,funids,errors)
	end
      | Local(_,bindings) => 
	loop (bindings@rest,strids,sigids,funids,Error.warning(loc, "skipping local declarations in source file "^msg)::errors)
      | _ =>
	loop (rest,strids,sigids,funids,Error.error(loc, "illegal top-level declaration in source file "^msg)::errors)


in
  loop (AST,[],[],[],[])
end

  val errors = checkSource(AST)

  val (AST, (errors,_), _) = checkDec (0,Fixity.initialEnv) AST (errors, Symbol.Map.empty)

in
  { AST = AST, errors = errors }
end (* of let *)

(*----------------------------------------------------------------------*)
(* Given a topdec list, find a particular entity binding.		*)
(*----------------------------------------------------------------------*)
fun find (AST, entity as (enttype, id)) =
let
  fun loop [] = NONE

    | loop ((decitem as (loc, predecitem))::rest) =
      case predecitem of
        Structure bindings =>
        if enttype = Entity.Str
        then
        let
          fun loop2 [] = loop rest
            | loop2 (entry as (id', _, _)::rest2) =
              if Symbol.equal(id,id') 
              then SOME (loc, Structure entry)
              else loop2 rest2
        in
          loop2 bindings
        end
        else loop rest

      | Signature bindings =>
        if enttype = Entity.Sig
        then
        let
          fun loop2 [] = loop rest
            | loop2 (entry as (id', _)::rest2) =
              if Symbol.equal(id,id') 
              then SOME (loc, Signature entry)
              else loop2 rest2
        in
          loop2 bindings
        end
        else loop rest

      | Functor bindings =>
        if enttype = Entity.Fun
        then 
        let
          fun loop2 [] = loop rest
            | loop2 (entry as (id', _, _, _)::rest2) =
              if Symbol.equal(id,id') 
              then SOME (loc, Functor entry)
              else loop2 rest2
        in
          loop2 bindings
        end
        else loop rest
      | Local(_,bindings) => loop (bindings@rest)
      | _ => loop rest
in
  loop AST
end


end (* of struct *)
