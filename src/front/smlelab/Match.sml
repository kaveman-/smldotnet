(*======================================================================*)
(* Signature matching							*)
(*======================================================================*)
structure Match :> MATCH =
struct

local 
  open SMLTy Env EnvOps SMLSch ValBind ElabState
in

structure T = SMLTerm
structure Map = Symbol.Map

(*----------------------------------------------------------------------*)
(* Enrichment for type schemes						*)
(*----------------------------------------------------------------------*)
fun matchSch (loc,longid) (strSch as TypeScheme(strTyvars, strTy),
                           sigSch as TypeScheme(sigTyvars, sigTy)) =

case SMLTyUnify.match false strTyvars sigTyvars (strTy, sigTy) of
  NONE => 
  (error (Error.error (loc, "match error: " ^ 
    Longid.toString longid),
    [("type specified", sigTy), ("type inferred", strTy)]); NONE)

| SOME S => 
  SOME (
    sigTyvars, 
    sigTy,
    map (fn tyvar => case TyVar.Map.find(S, tyvar) of 
          NONE => tyVarType tyvar
        | SOME ty => ty) strTyvars)

(*----------------------------------------------------------------------*)
(* First stage of matching for variable bindings: check that the        *)
(* identifier statuses are valid and return an exception map.           *)
(*----------------------------------------------------------------------*)
fun match1vb exmap (loc,longid) (strVB, sigVB) =
case (strVB, sigVB) of

  (* Signature specifies value and structure provides value *)
  (VarSch _, VarSch _) =>
  exmap

  (* Signature specifies excon and structure provides value *)
| (VarSch _, ExTy _) => 
  (error (Error.error (loc, 
    "signature specified excon but structure provided exception value: "
    ^ Longid.toString longid), []); exmap)

  (* Signature specifies constructor and structure provides value *)
| (VarSch _, ConSch _) => 
  (error (Error.error (loc,
    "signature specified constructor but structure provided value: "
    ^ Longid.toString longid), []); exmap)

  (* Signature specifies value and structure provides constructor *)
| (ConSch _, VarSch _) =>
  exmap

  (* Both specifiy constructor *)
| (ConSch _, ConSch _) => 
  exmap

  (* Signature specifies excon and structure provides constructor *)
| (ConSch _, ExTy ty) =>
  (error (Error.error (loc,
    "signature specified excon but structure provided data constructor: "
    ^ Longid.toString longid), []); exmap)

  (* Signature specifies value and structure provides excon *)
| (ExTy _, VarSch _) =>
  exmap

  (* Signature specifies constructor and structure provides excon *)
| (ExTy _, ConSch _) => 
  (error (Error.error (loc,
    "signature specified data constructor but structure provided excon: "
    ^ Longid.toString longid), []); exmap)

  (* Signature specifies excon and structure provides excon *)
| (ExTy(_,strexname), ExTy(_,sigexname)) =>
  TyName.Map.insert(exmap, sigexname, strexname)

| (* Signature specifies special and structure provides special *)
  (Special _, Special _) =>
  exmap

| (* Signature specifies value and structure provides special *)
  (Special _, VarSch _) =>
  exmap

(*----------------------------------------------------------------------*)
(* Second stage of matching for variable bindings: enrichment.		*)
(* For constructors and exceptions, check that the types match exactly. *)
(* For variables, return an appropriate declaration item and variable.  *)
(*----------------------------------------------------------------------*)
fun match2vb (loc, longid, longid' : SMLTerm.longid) (strVB, sigVB) =
let
  val v = List.last longid
in
case (strVB, sigVB) of
  (* Signature specifies value and structure provides value *)
  (VarSch strsch, VarSch sigsch) =>
  (case matchSch (loc,longid) (strsch, sigsch) of
    NONE => NONE
  | SOME (tyvars, ty, tys) => 
    SOME                                 (*@TODO:revise loc*)
    (T.Val(loc,tyvars,ty,T.PatVar(v,ty),(loc,T.Var(longid', tys))), v)
  )

  (* Signature specifies value and structure provides constructor *)
| (ConSch(strsch,CE), VarSch sigsch) =>
  (case matchSch (loc,longid) (strsch, sigsch) of
    NONE => NONE
  | SOME (tyvars, ty, tys) => 
    SOME                  (*@TODO:revise loc*) 
    (T.Val(loc,tyvars,ty,T.PatVar(v,ty),(loc,T.Con(v, CE, tys))), v)
  )

  (* Both specify constructor *)
| (ConSch(strsch as TypeScheme(strtyvars,strty),_), 
   ConSch(sigsch as TypeScheme(sigtyvars,sigty),_)) => 
  if SMLTy.eq (strty, 
         appSubst (ListPair.zip(sigtyvars, map tyVarType strtyvars)) sigty)
  then NONE
  else (error (Error.error (loc, 
    "type in signature doesn't match structure spec: " ^ 
    Longid.toString longid),
    [("in signature", sigty), ("in structure", strty)]); NONE)

  (* Signature specifies value and structure provides excon *)
| (ExTy(strty,excon), VarSch(TypeScheme(_,sigty))) =>
  if SMLTy.eq (strty, sigty)
  then SOME           (*@TODO:revise loc*)
  (T.Val(loc,[],sigty,T.PatVar(v,sigty),(loc,
    T.ExCon(excon, Option.map #1 (fromFunType strty)))), v)
  else
    (error (Error.error (loc, 
    "type in signature doesn't match structure spec: " ^ 
    Longid.toString longid),
    [("in signature", sigty), ("in structure", strty)]); NONE)

  (* Signature specifies excon and structure provides excon *)
| (ExTy(strty,excon), ExTy(sigty,_)) =>
  if SMLTy.eq (strty, sigty)
  then NONE
  else 
    (error (Error.error (loc, 
    "type in signature doesn't match structure spec: " ^ 
    Longid.toString longid),
    [("in signature", sigty), ("in structure", strty)]); NONE)

  (* Signature specifies value and structure provides special *)
| (special as Special _, VarSch sigsch) =>
    let val (exp,usety) =
    case special of
     Special(classty, NONE) =>
      let
        val usety = SMLTy.funType (freshMono (), classty)
        val defty = SMLTy.funType (freshMono (), classty)
        val constraint1 = TyConstraint.HasStatic(classty, NONE, defty)
        val constraint2 = TyConstraint.Sub(defty, TyConstraint.MembSub, usety)
      in
        TyConstraint.add (constraint1,loc);
        TyConstraint.add (constraint2,loc);
        (T.Invoc {defty = (classty, defty), usety = (classty, usety), 
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
        (T.Invoc {defty = (classty, defty), usety = (classty, usety), 
		  name = nameopt, object = NONE, optype = Ext.Invoke }, usety)
      end
    in
	case matchSch (loc,longid) (TypeScheme([],usety), sigsch) of
	     NONE => NONE
	   | SOME (tyvars, ty, tys) => 
		 SOME (T.Val(loc,tyvars,ty,T.PatVar(v,ty),(loc,exp)), v)
    end
(* Otherwise it's an error that's already been reported *)
| _ =>
  NONE
end

(*----------------------------------------------------------------------*)
(* First stage of signature matching: check structural stuff (if 	*)
(* signature defines an entity (var, type, substructure) then structure *)
(* should define it too). Also do instantiation of tynames to typefcns. *)
(*----------------------------------------------------------------------*)
fun match1 loc (strE : Env.Env, sigma : Env.Sig as (N,sigE)) =
let
  fun matchSE path acc (strSE : Env.StrEnv, sigSE : Env.StrEnv) =
  let
    fun matchBindings acc [] = 
        acc

      | matchBindings acc ((strid, sigE)::rest) =
        case Map.find(strSE, strid) of
          NONE => 
          (error (Error.error(loc, 
            "unmatched structure specification: " ^ 
            Longid.toString (path @ [strid])), []);
          matchBindings acc rest)
  
        | SOME strE => 
          matchBindings (matchE (path @ [strid]) acc (strE, sigE)) rest
  in
    matchBindings acc (Map.listItemsi sigSE)
  end

  and matchTE path acc (strTE : Env.TyEnv,sigTE : Env.TyEnv) =
  let
    fun matchBindings acc [] = 
        acc

      | matchBindings (acc as (psi,exmap)) ((tycon, sigtystr)::rest) =
        case Map.find(strTE, tycon) of
          NONE => 
          (error (Error.error(loc, "unmatched type specification: " ^
            Longid.toString (path @ [tycon])), []);
          matchBindings acc rest)
  
        | SOME strtystr => 
          let
            val acc = 
            case TyStr.match1 N psi (strtystr, sigtystr) of
              Result.Failure message => 
              (error (Error.error(loc, message ^ ": " ^
                Longid.toString (path @ [tycon])), []); acc)
            | Result.Success psi => 
	        ((case (TyStr.fromConcrete strtystr,TyStr.fromAbstract sigtystr) of
		      (SOME (_,ty),SOME _) =>
			  ElabCheck.checkType (loc,"an abstract type cannot be implemented by a byref type") ty
		    | _ => ());
		 (psi,exmap))
          in
            matchBindings acc rest
          end
   in
     matchBindings acc (Map.listItemsi sigTE)
   end

  and matchVE path acc (strVE : Env.ValEnv, sigVE : Env.ValEnv) =
  let
    fun matchBindings acc [] = 
        acc

      | matchBindings (acc as (psi,exmap)) ((id, sigvb)::rest) =
        case Map.find(strVE, id) of
          NONE => 
          (error (Error.error(loc, "unmatched value specification: " ^
            Longid.toString (path @ [id])), []);
          matchBindings acc rest)
  
        | SOME strvb => 
          let
            val exmap = match1vb exmap (loc, path @ [id]) (strvb, sigvb)
          in
            matchBindings (psi,exmap) rest
          end
   in
     matchBindings acc (Map.listItemsi sigVE)
   end

 and matchE path acc (strE : Env.Env, sigE : Env.Env) =
   let
     val acc = matchVE path acc (VEofE strE, VEofE sigE)
     val acc = matchTE path acc (TEofE strE, TEofE sigE)
     val acc = matchSE path acc (SEofE strE, SEofE sigE)
   in
     acc
   end
in
  matchE [] (TyName.Map.empty, TyName.Map.empty) (strE, sigE)
end

(*----------------------------------------------------------------------*)
(* Second stage of signature matching: enrichment.			*)
(* Wrt elaboration, this is just a check (does it match?) but we also   *)
(* require a term in which polymorphic variables are specialised        *)
(* appropriately.                                                       *)
(*----------------------------------------------------------------------*)
fun match2 (topstrid,loc) (strE : Env.Env, sigE : Env.Env) =
let
  fun matchSE (path, (topstrid,tail)) 
              (strSE : Env.StrEnv, sigSE : Env.StrEnv) =
  let
    fun matchBindings strval [] = 
        strval

      | matchBindings strval ((strid, sigE)::rest) =
        case EnvLookup.lookup(strSE, strid) of
         
          (* We've already reported this error *)
          NONE => matchBindings strval rest

        | SOME (strE, i) => 
          let
            val strexp = matchE (path @ [strid], (topstrid,tail @ [(strid,i)]))
              (strE, sigE)
          in
            matchBindings (Map.insert(strval, strid, strexp)) rest
          end
  in
    matchBindings Map.empty (Map.listItemsi sigSE)
  end

  and matchTE path (strTE : Env.TyEnv, sigTE : Env.TyEnv) =
  let
    fun matchBindings [] = 
        ()

      | matchBindings ((tycon, sigtystr)::rest) =
        case Map.find(strTE, tycon) of

          (* We've already reported this error *)
          NONE => 
          matchBindings rest
  
        | SOME strtystr => 
          ((case TyStr.match2 (strtystr, sigtystr) of
            SOME message => 
            error (Error.error(loc, message ^ ": " ^
              Longid.toString (path @ [tycon])), [])
          | NONE => ());
          matchBindings rest)
   in
     matchBindings (Map.listItemsi sigTE)
   end

  and matchVE (path, (topstrid, tail)) 
              (n, strVE : Env.ValEnv, sigVE : Env.ValEnv) =
  let
    fun matchBindings args [] = 
        args

      | matchBindings (ds,sval) ((id, sigvb)::rest) =          
        case EnvLookup.lookupVarVE(strVE, id) of

          (* We've already reported this error *)
          NONE => (ds,sval)
  
        | SOME (strvb, i) => 
          case match2vb (loc, path @ [id], (topstrid, tail @ [(id, i + n)])) 
               (strvb, sigvb) of
            NONE => matchBindings (ds,sval) rest
          | SOME (d,v) => matchBindings (d::ds,Map.insert(sval, id, v)) rest
   in
     matchBindings ([],Map.empty) (Map.listItemsi sigVE)
   end

 and matchE (path, longid : SMLTerm.longid) (strE : Env.Env, sigE : Env.Env) =
   let
     val strs = matchSE (path,longid) (SEofE strE, SEofE sigE)
     val (ds, vals) = matchVE (path,longid) 
     (Map.numItems (SEofE strE), VEofE strE, VEofE sigE)
     val _ = matchTE path (TEofE strE, TEofE sigE)
   in
     T.StrLet(ds, T.Struct(loc(*@TODO: better location *),vals, strs))
   end
in

  matchE ([], (topstrid, [])) (strE, sigE)

end

end (* of local open *)
end (* of struct *)
