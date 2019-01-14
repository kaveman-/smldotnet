(*======================================================================*)
(* Convert SML source syntax to the reduced syntax sufficient for       *)
(* dependency analysis.							*)
(*======================================================================*)
structure SyntaxConvert :> SYNTAXCONVERT =
struct

open Syntax
structure SS = SmallSyntax
structure LS = Longid.Set

(*----------------------------------------------------------------------*)
(* Strip a longid down to its qualifying structure.			*)
(*----------------------------------------------------------------------*)
val strip : Syntax.longid -> Syntax.longid = rev o tl o rev

fun mentions longids = 
  Longid.Set.addList(Longid.Set.empty, map strip longids)

(*----------------------------------------------------------------------*)
(* Extend the set S with the structures referenced in a type.		*)
(*----------------------------------------------------------------------*)
fun convertTy ((loc, prety) : Ty, S : LS.set) =
case prety of
  TyVar _             => S
| TyCon(tys, [_])     => convertTys (tys, S)
| TyCon(tys, longid)  => convertTys (tys, LS.add(S, strip longid))
| TyFun(ty1, ty2)     => convertTy (ty1, convertTy (ty2, S))
| TyRecord tyrow      => convertTys (map #2 tyrow, S)
| TyTuple tys         => convertTys (tys, S)

(* Do the same for a list of types... *)
and convertTys (tys, S) = foldl convertTy S tys

(* ...and for an optional type... *)
and convertTyOpt (NONE, S) = S
  | convertTyOpt (SOME ty, S) = convertTy (ty, S)

(* ...and for a list of optional types *)
and convertTyOpts (tyopts, S) = foldl convertTyOpt S tyopts

(* ..and for type and datatype bindings *)
and convertTypBind (typbind : TypBind, S) = convertTys (map #3 typbind, S)
and convertDatBind (datbind : DatBind, S) = 
    foldl (fn ((_,_,conbinds), S) => convertTyOpts (map #2 conbinds, S))
    S datbind
    
(*----------------------------------------------------------------------*)
(* Extend the set S with the structures referenced in a pattern.	*)
(*----------------------------------------------------------------------*)
fun convertPat ((loc, prepat) : Pat, S) =
case prepat of
(* SL: or *)
(*
  (PatWild | PatSCon _ | PatVar (Short _)) => S
| PatVar(Long longid) => LS.add(S, strip longid)
| PatVar _ => S
| PatCon([_], pat)    => convertPat (pat, S)
| PatCon(longid, pat) => convertPat (pat, LS.add(S, strip longid))
| PatRecord(_,patrow) => convertPats (map #2 patrow, S)
| PatConstraint(pat,ty) => convertPat (pat, convertTy (ty, S))
| PatCast(_, ty) => convertTy (ty, S)
| PatLayer(_,tyopt,pat) => convertPat (pat, convertTyOpt (tyopt, S))
| (PatTuple pats | PatList pats | OrPat pats) => convertPats (pats, S)
*)
  PatWild => S
| PatSCon _ => S
| PatVar (Short _) => S
| PatVar(Long longid) => LS.add(S, strip longid)
| PatVar _ => S
| PatCon([_], pat)    => convertPat (pat, S)
| PatCon(longid, pat) => convertPat (pat, LS.add(S, strip longid))
| PatRecord(_,patrow) => convertPats (map #2 patrow, S)
| PatConstraint(pat,ty) => convertPat (pat, convertTy (ty, S))
| PatCast(_, ty) => convertTy (ty, S)
| PatLayer(_,tyopt,pat) => convertPat (pat, convertTyOpt (tyopt, S))
| PatTuple pats => convertPats (pats, S)
| PatList pats => convertPats (pats, S)
| OrPat pats => convertPats (pats, S)


(* Do the same for a list of patterns *)
and convertPats (pats, S) = foldl convertPat S pats

(*----------------------------------------------------------------------*)
(* Construct a declaration representing a local or let construct.	*)
(* If the lhs of the construct is empty, just emit the rhs.		*)
(*----------------------------------------------------------------------*)
fun makeLocal ([], []) = []
  | makeLocal ([], dec) = dec
  | makeLocal (dec1, dec2) = [SS.Local(dec1, dec2)]

fun join (S,SS.Mention S'::D) = SS.Mention (LS.union(S,S')) :: D
  | join (S,D) = if LS.isEmpty S then D else SS.Mention S :: D

fun convertExp ((loc,preexp) : Exp, D : SS.Dec) =
case preexp of
(* SL: or *)
(*
  (SCon _ | Hash _ | DotHash _ | DotHashHash _) => D
| LongVid(Long longid)=> join (LS.singleton(strip longid), D)
| LongVid _           => D
| Fn match            => convertMatch (match, D)
| Let(dec, exp)       => 
  makeLocal(rev (convertDec (dec, [])), rev (convertExp (exp, []))) @ D

| LetUnless(dec, exp, match) => 
  makeLocal(convertMatch (match, []) @ rev (convertDec (dec, [])), rev (convertExp (exp, []))) @ D

| (Constraint(e, ty) | ConstraintGt(e,ty)) => 
  join (convertTy (ty, LS.empty), convertExp (e, D))

| ClassWith(e,m) => 
  convertMethBind (m, convertExp (e, D))

| (Raise e | Pure e) => 
  convertExp (e, D)

| Record exprow       => 
  foldl (fn ((_,e),D) => convertExp (e,D)) D exprow

| (Case(e, m) | Handle(e, m)) => 
  convertMatch (m, convertExp (e, D))

| If(e1, e2, e3) => 
  convertExp (e1, convertExp (e2, convertExp (e3, D)))

| (Orelse(e1, e2) | Andalso(e1,e2) | While(e1,e2) | App(e1,e2)
   | Synchronized(e1,e2)) => 
  convertExp (e1, convertExp (e2, D))

| (Sequence es | List es | Tuple es) => 
  foldl convertExp D es

| FlatApp _ =>
  Debug.fail "SyntaxConvert.convertExp: FlatApp"
*)
  SCon _ => D
| Hash _ => D
| DotHash _ => D
| DotHashHash _ => D
| LongVid(Long longid)=> join (LS.singleton(strip longid), D)
| LongVid _           => D
| Fn match            => convertMatch (match, D)
| Let(dec, exp)       => 
  makeLocal(rev (convertDec (dec, [])), rev (convertExp (exp, []))) @ D

| LetUnless(dec, exp, match) => 
  makeLocal(convertMatch (match, []) @ rev (convertDec (dec, [])), rev (convertExp (exp, []))) @ D

| Constraint(e, ty) => 
  join (convertTy (ty, LS.empty), convertExp (e, D))
| ConstraintGt(e,ty) => 
  join (convertTy (ty, LS.empty), convertExp (e, D))

| ClassWith(e,m) => 
  convertMethBind (m, convertExp (e, D))

| Raise e => convertExp (e, D)
| Pure e => convertExp (e, D)

| Record exprow       => 
  foldl (fn ((_,e),D) => convertExp (e,D)) D exprow

| Case(e, m) => convertMatch (m, convertExp (e, D))
| Handle(e, m) => convertMatch (m, convertExp (e, D))

| If(e1, e2, e3) => 
  convertExp (e1, convertExp (e2, convertExp (e3, D)))

| Orelse(e1, e2) => convertExp (e1, convertExp (e2, D))
| Andalso(e1,e2) => convertExp (e1, convertExp (e2, D))
| While(e1,e2) => convertExp (e1, convertExp (e2, D))
| App(e1,e2) => convertExp (e1, convertExp (e2, D))
| Synchronized(e1,e2) => convertExp (e1, convertExp (e2, D))

| Sequence es => foldl convertExp D es
| List es => foldl convertExp D es
| Tuple es => foldl convertExp D es

| FlatApp _ =>
  Debug.fail "SyntaxConvert.convertExp: FlatApp"

and convertMatch (match,res) =
    foldl (fn ((pat,exp),D) => 
      convertExp (exp, join (convertPat (pat,LS.empty), D)))
    res match

and convertFValBind (binds,res) =
    foldl (fn ((_, _, pats, exp, tyopt), D) =>
      convertExp (exp, 
        join (convertPats (pats, convertTyOpt (tyopt, LS.empty)), D)))
    res binds

and convertBodyOpt (NONE,D) = D
  | convertBodyOpt (SOME (pat,exp),D) =
    convertExp (exp, join (convertPat (pat, LS.empty), D))

(*@TODO: cvr: complete and review calls *) 
and convertAttribute ((loc,AttApp(e,namedargs)),D) =
    foldl (fn ((_,attarg),D) => convertExp (attarg,D)) (convertExp(e,D)) namedargs

and convertAttributes(atts,D) = foldl convertAttribute D atts
 
and convertMethBindItem ((attributes,mods,binds),res) =
    foldl (fn ((_, _, bodyopt, tyopt), D) =>
      convertBodyOpt (bodyopt, 
        join (convertTyOpt (tyopt, LS.empty), D)))
    (convertAttributes (attributes,res)) binds

and convertMethBind (bind,D) = foldl convertMethBindItem D bind

(*----------------------------------------------------------------------*)
(* Given a declaration item, extend D with the reduced version.		*)
(*----------------------------------------------------------------------*)
and convertDecItem ((loc,predecitem) : DecItem, D) =
case predecitem of
(* SL: or *)
(*
  (Val(_, match) | ValRec(_, match)) => 
  convertMatch (match, D)
*)
  Val(_, match) => convertMatch (match, D)
| ValRec(_, match) => convertMatch (match, D)

| Fun(_, bindings)    => 
  foldl convertFValBind D bindings

| Type typbind        => 
  join (convertTypBind (typbind, LS.empty), D)

| Datatype(datbind, NONE) => 
  join (convertDatBind (datbind, LS.empty), D)

| Datatype(datbind, SOME typbind) =>
  join (convertDatBind (datbind, convertTypBind (typbind, LS.empty)), D)

| Abstype(datbind, NONE, dec) =>
  join (convertDatBind (datbind, LS.empty), convertDec (dec, D))

| Abstype(datbind, SOME typbind, dec) =>
  convertDec (dec, join 
    (convertDatBind (datbind, convertTypBind (typbind, LS.empty)), D))

| DatatypeCopy(_, [id]) =>
  D
  
| DatatypeCopy(_, longid) =>
  join (LS.singleton (strip longid), D)

| Exception exbinds =>
  join (convertExBinds (exbinds, LS.empty), D)

| Local(dec1, dec2)   => 
  let
    val D1 = convertDec (dec1, [])
    val D2 = convertDec (dec2, [])
  in
    makeLocal(rev D1, rev D2) @ D
  end

| Open longids        => 
  SS.Open longids :: D

| ClassDec dec         => 
  convertClassDec (dec, D)

| Structure bindings  => 
  SS.Structure 
  (map (fn (strid, strexp, siginfo) =>     
    (strid, convertSigInfo (siginfo, convertStrExp strexp))) bindings) :: D

| Signature bindings  => 
  SS.Signature (map (fn (sigid, sigexp) =>     
    (sigid, convertSigExp sigexp)) bindings) :: D

| Functor bindings =>
  SS.Functor (map (fn (funid, funarg, siginfo, strexp) =>
    (funid, convertFunArg funarg, convertSigInfo (siginfo, convertStrExp strexp)))
     bindings) :: D

(* SL: or *)
(*
| (Infix _ | Infixr _ | Nonfix _) =>
  D
*)
| Infix _ => D
| Infixr _ => D
| Nonfix _ => D

and convertDec (decs, D) = foldl convertDecItem D decs

and convertExBind ((_, ExDesc (SOME ty)), S) = convertTy (ty, S)
  | convertExBind ((_, ExBind (Long longid)), S) = LS.add(S, strip longid)
  | convertExBind (_, S) = S

and convertExBinds (exbinds, S) = foldl convertExBind S exbinds

and convertClassDec (ClassType {attributes, conattributes,  pat, inherits, localdec, methoddec, ...}, D) =
      convertAttributes(attributes,[]) @ (*@TODO: cvr: review *)
        convertAttributes(conattributes,[]) @ (*@TODO: cvr: review *)
          SS.Mention (convertPat (pat, LS.empty)) :: 
	    makeLocal (rev (convertDec (localdec, [])), 
		       rev (convertMethBind (methoddec, []))) @ D
|   convertClassDec (DelegateType {attributes, conattributes, ty,...}, D) =
      convertAttributes(attributes,[]) @ (*@TODO: cvr: review *)
        convertAttributes(conattributes,[]) @ (*@TODO: cvr: review *)
          (join (convertTy (ty, LS.empty),D))
and convertStrExp ((loc,prestrexp) : StrExp) = 
case prestrexp of
  Struct dec          => SS.Struct (rev (convertDec (dec, [])))
| Strid longid        => SS.Strid longid
| StrTransparent(strexp, sigexp) => 
  SS.StrConstraint(convertStrExp strexp, convertSigExp sigexp)
| StrOpaque(strexp, sigexp) => 
  SS.StrConstraint(convertStrExp strexp, convertSigExp sigexp)
| FunApp(id, strexp)  => SS.FunApp(id, convertStrExp strexp)
| StrLet(dec, strexp) => 
  SS.StrLet(rev (convertDec(dec,[])), convertStrExp strexp)

and convertSigExp ((loc,presigexp) : SigExp) =
case presigexp of
  SigSpec spec     => SS.SigSpec(convertSpec spec)
| Sigid id         => SS.Sigid id
| Where(sigexp, tyvars, longtycon, ty) =>
  SS.Where(convertSigExp sigexp, convertTy (ty, LS.empty))

(* SL: or *)
(*
and convertSigInfo ((SigConcrete sigexp | SigAbstract sigexp), strexp) =
    SS.StrConstraint(strexp, convertSigExp sigexp)
  | convertSigInfo (SigNone, strexp) = strexp
*)
and convertSigInfo (SigConcrete sigexp, strexp) =
    SS.StrConstraint(strexp, convertSigExp sigexp)
  | convertSigInfo (SigAbstract sigexp, strexp) =
    SS.StrConstraint(strexp, convertSigExp sigexp)
  | convertSigInfo (SigNone, strexp) = strexp

and convertFunArg (StructArg(strid, sigexp)) = 
    [SS.StructureDesc [(strid, convertSigExp sigexp)]]

  | convertFunArg (SpecArg spec) = 
    convertSpec spec

and convertClassDesc (ClassTypeSpec {conty,inherits,methodspec,...}) =
    [SS.SpecMention (convertTys (map #2 methodspec, 
    (convertTyOpt (conty, convertTys (inherits, LS.empty)))))]
    
and convertSpecItem ((loc,prespecitem) : SpecItem) =
case prespecitem of 
  ValDesc valdesc     => 
  [SS.SpecMention (convertTys (map #2 valdesc, LS.empty))]

| TypeDesc typdesc    => 
  [SS.SpecMention (convertTyOpts (map #3 typdesc, LS.empty))]

| EqTypeDesc _        => 
  []

| DatatypeDesc(datbind, NONE) => 
  [SS.SpecMention (convertDatBind (datbind, LS.empty))]

| DatatypeDesc(datbind, SOME typbind) =>
  [SS.SpecMention (convertDatBind (datbind, convertTypBind (typbind, LS.empty)))]

| DatatypeDescCopy(_, [id]) =>
  []

| DatatypeDescCopy(_, longid) =>
  [SS.SpecMention (LS.singleton (strip longid))]

| ExceptionDesc conbinds => 
  [SS.SpecMention (convertTyOpts (map #2 conbinds, LS.empty))]

| StructureDesc bindings => 
  [SS.StructureDesc 
  (map (fn (strid, sigexp) => (strid,convertSigExp sigexp)) bindings)]

| Include sigexp =>
  [SS.Include (convertSigExp sigexp)]

| ClassDesc d =>  
  convertClassDesc d

(* SL: or *)
(*
| (Sharing longids | SharingType longids) =>
  [SS.SpecMention (LS.addList (LS.empty, List.mapPartial 
  (fn [id] => NONE | longid => SOME (strip longid)) longids))]
*)
| Sharing longids =>
  [SS.SpecMention (LS.addList (LS.empty, List.mapPartial 
  (fn [id] => NONE | longid => SOME (strip longid)) longids))]
| SharingType longids =>
  [SS.SpecMention (LS.addList (LS.empty, List.mapPartial 
  (fn [id] => NONE | longid => SOME (strip longid)) longids))]

and convertSpec spec = List.concat (map convertSpecItem spec)

val convert = fn x => rev (convertDec (x, []))

end
