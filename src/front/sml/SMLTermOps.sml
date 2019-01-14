structure SMLTermOps :> SMLTERMOPS =
struct

local 
  open SMLTerm Pretty
in

val showPaths = Controls.add false "showPaths"
val showSMLAttributes = Controls.add false "showSMLAttributes"

structure S = Symbol.Set
structure M = Symbol.Map

(*----------------------------------------------------------------------*)
(* Pretty print various lists						*)
(*   Types are written between braces      				*)
(*   Tuple values are written between round brackets			*)
(*   Multiple arguments are written between angle brackets		*)
(*----------------------------------------------------------------------*)
fun tyvec f = vec ("", " {", "}", " {", "}", ",") f
fun argvec f = vec (" <>", " ", "", " <", ">", ",") f
fun bindvec f = vec ("", "Fn ", " => ", "Fn <", "> => ", ",") f
fun conargvec f = vec ("", " ", "", "<", ">", ",") f
fun tuplevec f = vec ("()", "", "", "(", ")", ",") f

fun pathToString [] = ""
  | pathToString ((id,i)::rest) = 
    "." ^ Id.toString id ^ 
    (if Controls.get showPaths then "[" ^ Int.toString i ^ "]" else "") ^
    pathToString rest

fun displayDecItem depth dec =
case dec of
  Val(loc, tyvars, ty1, pat, e) =>
  "val " ^ displayPat pat ^ " = " ^ 
  bindvec TyVar.toString tyvars ^ display (depth+1) e

| ValRec(tyvars, defs) => 
    "val " ^ bindvec TyVar.toString tyvars ^ 
    "rec" ^ newline (depth+1) ^
    vec("", "", "", "", "", " and" ^ newline (depth+1))
    (displayRecBind (depth+1)) defs

| Exception exname =>
  "exception " ^ TyName.toString exname

| Structure(strid, strexp) =>
  "structure " ^ Id.toString strid ^ " = " ^ displayStrExp depth strexp

| ClassType 
  { tyname, attributes,flags,conattributes, superty, superarg, interfaces, methods, localdec, ... } =>
  "_classtype " ^
  displayAttributes depth attributes ^
  (*@TODO: displayFlags depth flags ^ *)
  displayAttributes depth conattributes ^
  TyName.toString tyname ^ " : " ^ 
  SMLTy.toString superty ^ display depth superarg ^ 
  (if null interfaces then "" else " ," ^ Pretty.simpleVec "," 
   SMLTy.toString interfaces) ^ 
  " with local " ^ displayDec depth localdec ^ " in " ^
  Pretty.simpleVec " and " (fn { attributes, flags, name, ty, body } =>
    displayAttributes depth attributes ^
   (*@TODO: displayFlags depth flags ^ *)
    Id.toString name ^ (case body of NONE => "" | SOME e =>
    " = " ^ display depth e)) methods

| Local(dec1, dec2) =>
  "local" ^ newline (depth+1) ^ displayDec (depth+1) dec1 ^ newline depth ^ 
  "in" ^ newline (depth+1) ^ displayDec (depth+1) dec2 ^ newline depth ^ "end"

| And(ds) =>
  Pretty.simpleVec " and " (displayDecItem (depth+1)) ds

and displayDec depth [] = ""
  | displayDec depth (decitem::dec) = 
    displayDecItem depth decitem ^ newline depth ^ displayDec depth dec

and display depth (loc,e) =
let
  val nl = newline depth

  fun displayLet (dec, le as (loc,e), match) =
    newline (depth+1) ^ displayDec (depth+1) dec ^
    (case e of
      Let (dec,e) =>
      displayLet (dec,e,[])

    | LetUnless (dec,e,match) =>
      displayLet (dec,e,#2 match)

    | _ =>
      nl ^ "in" ^ newline (depth+1) ^ display (depth+1) le ^ nl ^ "end")

in
    
  case e of
    SCon (scon, ty, loc) => 
    SCon.toString scon ^ ":" ^ SMLTy.toString ty

  | Var ((id,rest), tys) => 
    Id.toString id ^ pathToString rest ^ tyvec SMLTy.toString tys

  | OverloadedVar ((id,rest), tynameset, tys) => 
    Id.toString id ^ pathToString rest ^ "@" ^ 
      tyvec TyName.toString (TyName.Set.listItems tynameset) ^
      tyvec SMLTy.toString tys

  | Con(con, (_,tyname,_), tyargs) => 
    Id.toString con ^ "{" ^ 
      SMLTy.toString (SMLTy.consType(tyargs,tyname)) ^ "}"

  | ExCon(exname,tyopt) =>
    TyName.toString exname

  | App(e1, e2) =>
    display depth e1 ^ " " ^ display depth e2

  | Special((optype, tyopt, idopt), es, tyopt', effect) => 
    "special " ^ 
    vec("()", "(", ")", "(", ")", ",") (display depth) es

  | Invoc {defty = (classty,d), name = SOME name, object = NONE, 
           usety = (_,usefun), ... } =>
    SMLTy.toString classty ^ "." ^ Id.toString name ^
    "[at " ^ SMLTy.toString usefun ^ "]"

  | Invoc {defty = (classty,_), name = SOME n, object = SOME e, optype, 
           usety = (_,usefun), ...} =>
    display depth e ^ ".#" ^ 
    (case optype of Ext.InvokeSuper => "#" | _ => "") ^ Id.toString n ^
    "[at " ^ SMLTy.toString usefun ^ "]"

  | Invoc { defty = (classty,_), name = NONE, object = NONE, ... } =>
    "_new " ^ SMLTy.toString classty

  | Fn (ty, match) => 
    "(fn {" ^ SMLTy.toString ty ^ "}" ^ displayMatch depth match ^ ")"

  | Let(dec,exp) =>
    nl ^ "let" ^ displayLet (dec,exp,[])

  | LetUnless(dec,exp,match) =>
    nl ^ "let" ^ displayLet (dec,exp,#2 match)

  | Raise(e, ty, loc) =>
    "raise " ^ display depth e ^ " : " ^ SMLTy.toString ty

  | Record exprow =>
    vec("()", "{", "}", "{", "}", ",")
      (fn (lab,exp,ty) => Id.toString lab ^ "=" ^ display depth exp) exprow

  | Handle(exp, match) =>
    "(" ^ display depth exp ^ " handle " ^ displayMatch depth match ^ ")"

end

and displayStrExp depth strexp =
let
  val nl = newline depth

  fun displayLet (dec,strexp) =
    newline (depth+1) ^ displayDec (depth+1) dec ^
    (case strexp of
      StrLet(dec, strexp) =>
      displayLet (dec, strexp)

    | _ =>
      nl ^ "in" ^ 
      newline (depth+1) ^ displayStrExp (depth+1) strexp ^ nl ^ "end")
in
  case strexp of

    Struct (_,vals, strs) =>
    "struct " ^ vec("", "", "", "", "", ",")
      (fn (id, var) => Id.toString id ^ " = " ^ Id.toString var) 
      (M.listItemsi vals)
    ^ ";" ^ vec("", "", "", "", "", ",")
      (fn (strid, strexp) => "structure " ^ Id.toString strid ^ " = " ^ 
      displayStrExp depth strexp) (M.listItemsi strs) ^ " end"

  | Strid(id,rest) =>
    Id.toString id ^ pathToString rest

  | StrLet(dec, strexp) =>
    nl ^ "let" ^ displayLet (dec,strexp)
  | StrInlined(sourcemap,strexp) =>
    nl ^ "(inlined " ^ displayStrExp (depth+1) strexp ^ ")"

end

and displayRecBind depth (funvar, body, funty) =
    Id.toString funvar ^ " =" ^ newline depth ^ display depth body

and displayMatch depth (ty, mrules) =
  vec("", "","", "", "", newline depth ^ "| ")
  (displayMRule depth) mrules

and displayMRule depth (loc,pat,exp) =
  displayPat pat ^ " => " ^ display depth exp

and displayPat pat =
case pat of
  PatWild => "_"
| PatSCon(scon, loc) => SCon.toString scon
| PatLiteral con => Constants.constant_toString con
| PatVar (v,ty) => Id.toString v
| PatRef pat => "ref " ^ displayPat pat
| PatCon(con, _, _, NONE) => Id.toString con
| PatCon(con, _, _, SOME pat) => Id.toString con ^ " " ^ displayPat pat
| PatExCon(en, NONE) => TyName.toString en
| PatExCon(en, SOME(ty,pat)) => TyName.toString en ^ " " ^ displayPat pat
| PatOr pats => Pretty.simpleVec " | " displayPat pats
| PatVec pats => "#[" ^ Pretty.simpleVec "," displayPat pats ^ "]"
| PatCast(x,ty) => Id.toString x ^ ":>" ^ SMLTy.toString ty

| PatRecord(isopen, patrow) => 
  vec("{}", 
      "{", if isopen then ",...}" else "}",
      "{", if isopen then ",...}" else "}", ",")
  (fn (lab,pat) => Id.toString lab ^ "=" ^ displayPat pat) patrow
| PatLayer((v,ty), pat) => Id.toString v ^ " as " ^ displayPat pat

and displayAttributes depth atts = 
       let fun dispAttributes depth [] =  ""
	     | dispAttributes depth (attexp::atts) =
	       dispAttribute depth attexp ^ newline depth  ^ dispAttributes depth atts
	   and dispAttribute depth (AttApp(_,exp,args)) = 
	       (display depth exp ^
		(case args of 
		     [] => "" 
		   | _ => " where " ^
			  (vec("", "", "", " ", "", ",") 
			      (fn (Field lab,exp,ty) => Id.toString lab ^ "=" ^ display depth exp ^ ":" ^ SMLTy.toString ty  
			       |  (Property lab,exp,ty) => Id.toString lab ^ "(" ^ display depth exp ^ ")" ^ ":" ^ SMLTy.toString ty)
			      args) ^ 
			  " end")) 
       in
	   if Controls.get showSMLAttributes then  newline (depth + 1) ^ "{ " ^ dispAttributes (depth + 2) atts ^ "}" else ""
       end



val toString = displayStrExp 0
val expToString = display 0
val patToString = displayPat
(*----------------------------------------------------------------------*)
(* Is a typed SML term `valuable' (`non-expansive' in Defn)?            *)
(* We extend the definition to permit field access and method lookup    *)
(*----------------------------------------------------------------------*)
fun isValuable (loc,e) =
case e of
  SCon _ => true
| Var _  => true
| Con _  => true
| ExCon _ => true
| Fn _   => true
| Record fields => List.all (fn (lab,e,ty) => isValuable e) fields
| App((loc,Con _), e) => isValuable e
| App((loc,ExCon _), e) => isValuable e
| _ => false

(*----------------------------------------------------------------------*)
(* Variables bound in a pattern, with their types                       *)
(*----------------------------------------------------------------------*)
fun fvPat pat =
case pat of
(* SL: or *)
(*
  (PatVar (v,ty) | PatCast (v,ty)) => 
  M.insert(M.empty, v, ty)

| (PatRef pat | PatCon(_, _, _, SOME pat) | PatExCon(_, SOME(_,pat))) => 
  fvPat pat
*)
  PatVar (v,ty) =>
  M.insert(M.empty, v, ty)
| PatCast (v,ty) => 
  M.insert(M.empty, v, ty)

| PatRef pat => 
  fvPat pat
| PatCon(_, _, _, SOME pat) => 
  fvPat pat
| PatExCon(_, SOME(_,pat)) => 
  fvPat pat


| PatRecord(isopen, patrow) => 
  foldr (M.unionWith #1) M.empty 
    (map (fvPat o #2) patrow)

| PatLayer((v,ty), pat) => 
  M.insert(fvPat pat, v, ty)

| PatOr (pat::pats) =>
  fvPat pat (* We know that the free vars for pats must be the same *)

| PatVec pats =>
  foldr (M.unionWith #1) M.empty (map fvPat pats)

| _ =>
  M.empty

(*----------------------------------------------------------------------*)
(* Free variables in a term						*)
(*----------------------------------------------------------------------*)
fun fv (loc,e) =
  case e of
    Var ((v, []), tys) => 
    S.singleton v

  | App(e1, e2) =>
    S.union(fv e1, fv e2)

  | Fn (ty, match) => 
    fvMatch match

  | Let(d, e) =>
    let  
      val (bound,free) = fvDec d      
    in
      S.union (free, S.difference (fv e, bound))
    end

  | LetUnless(d, e, match) =>
    let  
      val (bound,free) = fvDec d      
    in
      S.union (fvMatch match, S.union (free, S.difference (fv e, bound)))
    end

  | Handle(exp, match) =>
    S.union (fv exp, fvMatch match)

  | Raise(e, ty, loc) =>
    fv e

  | Record exprow =>
    foldr S.union S.empty (map (fv o #2) exprow)

  | Special(j, es, tyopt, effect) => 
    foldr S.union S.empty (map fv es) 

  | _ =>
    S.empty

and fvDecItem d =
case d of

  Val(_, _, _, pat, exp) =>
  (M.foldli (fn (v,_,s) => S.add(s,v)) 
    S.empty (fvPat pat), fv exp)

| ValRec(tyvars, defs) => 
  let 
    val funs = foldr (fn ((f,_,_),s) => S.add(s,f)) 
    S.empty defs
  in
    (funs, S.difference (
      foldr (fn ((_,body,_),s) => S.union(s, fv body)) 
      S.empty defs,
      funs))
  end

| Local(d1, d2) =>
  let
    val (bvs1, fvs1) = fvDec d1
    val (bvs2, fvs2) = fvDec d2
  in
    (bvs2, S.union(fvs1, S.difference(fvs2, bvs1)))
  end
(*@BUG: these definitions may be wrong *)
| ClassType   {  
    loc,
    tyname (* TyName.TyName*),
    attributes(* AttExp list*),
    flags (* Symbol.Set.set*),
    conattributes(* AttExp list*),
    superty (* SMLTy.Type*),
    superarg (* Exp*),
    interfaces (* SMLTy.Type list*),
    methods (* Method list*),
    localdec (* Dec*),
    argpat (* Pat*),
    argty (* SMLTy.Type *)
  } =>
  let val bvsargpat = M.foldli(fn(v,_,s) => S.add(s,v)) S.empty (fvPat argpat)
      val bvsthis = S.singleton(Id.thisSym)
      val fvssuperarg = fv(superarg)
      val (bvslocal,fvslocal) = fvDec localdec
      val fvsmethods = 
	 List.foldl (fn({body,...},fvs) => 
		     case body of 
			 SOME exp => S.union(fvs,fv exp)
                       | NONE => fvs)
	            S.empty 
		    methods
  in (S.empty,S.difference(S.union(S.union(fvssuperarg,fvslocal),
				   S.difference(fvsmethods,bvslocal)),
			   S.union(bvsargpat,bvsthis)))
  end
| Exception _ =>
  (S.empty, S.empty)
(*@TODO: complete *)
| Structure _ =>
  (S.empty, S.empty)
| And ds =>
  (*@TODO: review *)
  List.foldl (fn (d,(bvs,fvs)) => 
	      let val (bvs1,fvs1) = fvDecItem d
	      in  (S.union(bvs,bvs1),S.union(fvs,fvs1))
	      end) (S.empty,S.empty) ds

and fvDec [] = (S.empty, S.empty)
  | fvDec (d::ds) =
    let 
      val (bvs1, fvs1) = fvDecItem d
      val (bvs2, fvs2) = fvDec ds
    in
      (S.union(bvs1, bvs2), S.union(fvs1, S.difference(fvs2, bvs1)))
    end

and fvMatch (ty, mrules) =
  foldr 
    (fn ((_,pat,e),s) => S.union(s, S.difference(fv e, 
      M.foldli (fn (v,_,s) => S.add(s,v)) 
      S.empty (fvPat pat))))
    S.empty mrules


end (* of local open*)

end (* of struct *)

