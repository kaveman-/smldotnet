(*======================================================================*)
(* Type check a MIL term.                                               *)
(*======================================================================*)
structure TypeCheck :> TYPECHECK =
struct

local 
  open MILTerm 
in

val term = ref (NONE : Cmp option)

(* Position of a value within a value or computation *)
datatype ValPos = 
  TailAppPos 	(* Tail-application e.g. let x <= M in ?(x) *)
| AppPos 	(* Non-tail application e.g. let x <= ?(y) in M *)
| BindPos     	(* Val-binding e.g. let x <= ? in M *)
| AtomPos       (* Must be an atom *)
| NonAtomPos    (* Must not be an atom *)
| RetPos        (* Return position *)

(* Position within a computation *)
datatype CmpPos = TailPos | NonTailPos

(* Display the entire term on error? *)
val showCheckedTerm = Controls.add false "showCheckTerm"

fun dumpCheckedTerm term message = 
    if Controls.get showCheckedTerm then
   (Debug.print ("CheckedTerm: " ^ message ^ " in:\n"); 
    MILPretty.dumpCmp term)
    else ()


(* @todo: more precision, ie. check the type really is an enum *)
fun maybeEnum ty =
    case MILTy.fromTyname ty of
	NONE => false
      | SOME tn => 
	  TyName.isExternal tn 
	  andalso not (TyName.isClass tn) 
	  andalso (TyName.equality tn) = TyName.Eq

(*----------------------------------------------------------------------*)
(* Check various properties of a MIL term. Always check types. Also:	*)
(*   checkDistinctVars: 	uniqueness of bound variables		*)
(*   checkDistinctTyVars:	uniqueness of bound type variables	*)
(*   checkAtoms:		atom/value well-formedness		*)
(*   checkCC:                   have cc's been performed?		*)
(*   checkPoly:			monomorphism-ready?			*)
(*----------------------------------------------------------------------*)
fun check {closedBlocks,checkAtoms,checkDistinctTyVars,checkDistinctVars,checkCC,checkPoly} 
          {kindenv,tyenv,funenv} wholece =
let

  (* Set of term-bound type variables and ordinary variables *) 
  val boundTyVars = ref (Var.Set.empty)
  val boundVars = ref (Var.Set.empty)
  val errors = ref false

  open MILPretty NewPretty
  infixr 5 ++
  fun fail d = Debug.failDoc (text "Unrecoverable TypeCheck Error: " ++ d) 
  fun error d = (Debug.printDoc (line ++ text "TypeCheck Error: " ++ d ++ line); errors := true)

  fun mismatch (ty1,ty2) = 
    text "type mismatch; expected " ++ MILTy.pTy ty1 ++ line ++ 
    text " but got " ++ MILTy.pTy ty2

  fun errorVal v message = (error (text message ++ text " in " ++ line ++ pVal v))
  fun errorCmp e message = (error (text message ++ text " in " ++ line ++ pCmp e))

  fun failVal v message = fail (text message ++ text " in " ++ line ++ pVal v)
  fun failCmp e message = fail (text message ++ text " in " ++ line ++ pCmp e)

  type Env = 
    { tyenv : (MILTy.Type * MILTerm.FunKind option) Var.Map.map,
      kindenv : MILTy.Kind Var.Map.map,
      dbkindenv : MILTy.Kind list, 
      toplevel : bool,
      pos : CmpPos
    }

  (* Extend the type environment with non-function bindings *) 
  fun extendTyEnv ({ tyenv, kindenv, dbkindenv, toplevel, pos } : Env) typedvars =
    { tyenv = foldr (fn (((x,_),ty),m) => Var.Map.insert(m,x,(ty,NONE))) tyenv typedvars, 
      kindenv = kindenv, dbkindenv = dbkindenv, toplevel = toplevel, pos = pos }

  (* Extend the type environment with function bindings *)
  fun extendFunEnv ({ tyenv, kindenv, dbkindenv, toplevel, pos } : Env) (kind, typedvars) =
    { tyenv = foldr (fn (((x,_),ty),m) => Var.Map.insert(m,x,(ty,SOME kind))) tyenv typedvars, 
      kindenv = kindenv, dbkindenv = dbkindenv, toplevel = toplevel, pos = pos }

  (* Extend the kind environment for term-bound type variables *)
  fun extendKindEnv ({ tyenv, kindenv, dbkindenv, toplevel, pos } : Env) tyvars =
    { tyenv = tyenv, kindenv = MILTermOps.addTyVars(kindenv, tyvars), dbkindenv = dbkindenv, toplevel = toplevel, pos = pos }

  (* Extend the kind environment for type-bound type variables *)
  fun extendDBKindEnv ({ tyenv, kindenv, dbkindenv, toplevel, pos } : Env) tyvars =
    { tyenv = tyenv, kindenv = kindenv, dbkindenv = tyvars@dbkindenv, toplevel = toplevel, pos = pos }

  fun nonTailEnv ({ tyenv, kindenv, dbkindenv, toplevel, pos } : Env) =
    { tyenv = tyenv, kindenv = kindenv, dbkindenv = dbkindenv, toplevel = toplevel, pos = NonTailPos }

  val emptyEnv = 
    { tyenv = Var.Map.map (fn ty => (ty,NONE)) tyenv, kindenv = kindenv, dbkindenv = [], toplevel = true, pos = TailPos }

  fun tyenvOf (e : Env) = #tyenv e
  fun kindenvOf (e : Env) = #kindenv e
  fun dbkindenvOf (e : Env) = #dbkindenv e

  (* Check a list of types; return kind of list (all must be Eq for list to be Eq) *)
  fun checkTys env tys = 
    if List.all (fn MILTy.Eq => true | _ => false) (map (checkTy env) tys)
    then MILTy.Eq else MILTy.Any

  (* Check a type; return its kind *)
  and checkTy (env : Env) (ty : MILTy.Type) =
    case MILTy.proj ty of

      (* Check that a term-bound type variable is in the environment *)
      MILTy.Var x =>
      (case Var.Map.find(kindenvOf env, x) of
        SOME kind => kind
      | NONE => (error (text "unbound type variable " ++ text(Var.toString x) ++ text " in "  ++ MILTy.pTy ty); MILTy.Any)
      )

      (* Check that a type-bound type variable (de Brujn) is in range *)
    | MILTy.Deb i => 
      if i<0 orelse i>length (dbkindenvOf env)
      then (error (text "out-of-range de Brujn type variable " ++ text(Int.toString i) ++ text " in " ++ MILTy.pTy ty); MILTy.Any)
      else List.nth(dbkindenvOf env, i)

      (* @todo akenn: check that tn is valid *)
    | MILTy.Tyname tn =>
      (
        case TyName.equality tn of
          TyName.NotEq => MILTy.Any
        | TyName.RefEq => MILTy.Eq
        | TyName.Eq => MILTy.Eq
      )

      (* @todo akenn: check that ex is valid *)
    | MILTy.Exn(ex,tys) =>
      let val kind = checkTys env tys
      in
	MILTy.Any
      end

    | MILTy.Forall (kinds, ty) => 
      (app (checkKind env) kinds; 
      checkTy (extendDBKindEnv env kinds) ty;
      MILTy.Any)

      (* Arrays are always equality types *)
    | MILTy.Array ty => (checkTy env ty; MILTy.Eq)

      (* Vectors are equality types if the element type is *)
    | MILTy.Vector ty => checkTy env ty

      (* Refs are always equality types *)
      (* @todo akenn: check "kind" of ref (ty) *)
    | MILTy.Refty (tys,ty) => (app (ignore o checkTy env) tys; checkTy env ty; MILTy.Eq)

      (* Products are equality types if their components all are *)
    | MILTy.Prod tys => checkTys env tys

    | MILTy.Con tys => (app (ignore o checkTy env) tys; MILTy.Any)

      (* Functions are never equality types *)
    | MILTy.Arrow (tys, cty) => (checkTys env tys; checkCmpTy env cty; MILTy.Any)

    | MILTy.Mu (i, defs) => (app (ignore o checkTy (extendDBKindEnv env (map (fn _ =>MILTy.Any) defs)) o #2) defs; MILTy.Any)

    | MILTy.Sum tyss => (app (app (ignore o checkTy env)) tyss; MILTy.Any)

    | _ => (error (text "invalid type " ++ MILTy.pTy ty); MILTy.Any)

  and checkCmpTy env cty =
    let val (effect, tys) = MILTy.fromCmp cty
    in
      app (ignore o checkTy env) tys
    end

  and checkTyVar env (x, kind) =
      (if checkDistinctTyVars andalso Var.Set.member(!boundTyVars, x)
      then error (text "duplicate bound type variable " ++ text(Var.toString x))
      else boundTyVars := Var.Set.add(!boundTyVars, x); 
      checkKind env kind)

  and checkKind env (MILTy.Bound ty) = (checkTy env ty; ())
    | checkKind env _ = ()

  fun checkKinds x env ([],[]) = ()
    | checkKinds x env (ty::tys, MILTy.Bound ty'::kinds) =
      (* was leq *)
      if MILTyRep.sameRep (kindenvOf env) (ty,ty')
      then checkKinds x env (tys,kinds)
      else error (text "checkKinds: {" ++ MILTy.pTy ty ++ text "} not equivalent to bound " ++ MILTy.pTy ty')
    | checkKinds x env (ty::tys, _::kinds) = checkKinds x env (tys,kinds)
    | checkKinds x env _ =
      error (text "TypeCheck.checkKinds: tyvar arity mismatch")


    (* Check consistency of c with ty *)
    fun checkSCon (v, c, ty) =
      case c of
        Constants.BOOLEAN _ =>
        if MILTy.eq(ty, MILTys.bool) 
        then () else errorVal v "type must be bool"

      | Constants.CHAR _ =>
        if MILTy.eq(ty, MILTys.char)
        then () else errorVal v "type must be char"

      | Constants.BYTE _ =>
        if MILTy.eq(ty, MILTy.tyname TyNames.word8TyName)
        orelse MILTy.eq(ty, MILTy.tyname TyNames.int8TyName)
	orelse maybeEnum ty
        then () else errorVal v "type must be Word8.word or Int8.int or enum"

      | Constants.SHORT _ =>
        if MILTy.eq(ty, MILTy.tyname TyNames.word16TyName)
        orelse MILTy.eq(ty, MILTy.tyname TyNames.int16TyName)
	orelse maybeEnum ty
        then () else errorVal v "type must be Word16.word or Int16.int"

      | Constants.INT _ =>
        if MILTy.eq(ty, MILTy.tyname TyNames.word32TyName)
        orelse MILTy.eq(ty, MILTy.tyname TyNames.int32TyName)
	orelse maybeEnum ty
        then () else errorVal v "type must be word or int"

      | Constants.LONG _ =>
        if MILTy.eq(ty, MILTy.tyname TyNames.word64TyName)
        orelse MILTy.eq(ty, MILTy.tyname TyNames.int64TyName)
	orelse maybeEnum ty
        then () else errorVal v "type must be Word64.word or Int64.int"

      | Constants.FLOAT _ =>
        if MILTy.eq(ty, MILTy.tyname TyNames.real32TyName)
        then () else errorVal v "type must be Real32.real"

      | Constants.DOUBLE _ =>
        if MILTy.eq(ty, MILTy.tyname TyNames.real64TyName)
        then () else errorVal v "type must be real"

      | Constants.STRING _ =>
        if MILTy.eq(ty, MILTys.string)
        then () else errorVal v "type must be string"

      | Constants.NULL =>
        errorVal v "invalid constant"

  (*--------------------------------------------------------------------*)
  (* Check the type of a value term					*)
  (*--------------------------------------------------------------------*)
  fun checkVal (pos : ValPos) (env : Env) v =
  let
    fun checkAtomAgainstPos () = 
        case pos of
          NonAtomPos => if checkAtoms then errorVal v "expected non-atomic value at this position" else ()
        | _ => ()
 
    fun checkNonAtomAgainstPos () = 
        case pos of
          AtomPos => if checkAtoms then errorVal v "expected atomic value at this position" else ()
        | _ => ()
  in
    case v of
      SCon (ty, c) => 
      (checkTy env ty;        (* First check well-formedness of type *)
       checkSCon (v,c,ty);    (* Next check that constant fits type *)
       checkAtomAgainstPos(); (* Finally check that the atomic status of SCon fits the position *)
       SOME ty)

      (* Check variables; error if it's a known/local function not in an application position *)
    | Var x => 
      (case Var.Map.find(tyenvOf env, x) of
        NONE => (errorVal v "value variable not in environment"; NONE)

        (* @todo akenn: check position etc *)
      | SOME (ty, kindopt) => (checkAtomAgainstPos(); SOME ty)
    )

    (* Injection into a sum type; error if values don't match sum type *)

  | Inj(resty, i, vs, _) => 
    let
      (* First check arguments *)
      val vtyopts = map (checkVal AtomPos env) vs
    in
      (* Next check well-formedness of type *)
      checkTy env resty;       
      case MILTy.fromSum resty of
        SOME tyss =>
        if i < 0 orelse i >= length tyss then (errorVal v "invalid constructor number"; SOME resty)
        else 
        let 
          val tys = List.nth(tyss, i)
          fun checkArgs (_, []) = ()
            | checkArgs (i, (NONE, ty)::rest) = checkArgs (i+1, rest)
            | checkArgs (i, (SOME vty, ty)::rest) =
              if MILTy.sub (vty, ty) 
              then ()
              else (error (mismatch (ty,vty) ++ text " in argument " ++ text (Int.toString i) ++ text " in" ++ line ++ pVal v);
                    checkArgs (i+1, rest))
        in
          if length vs <> length tys
          then errorVal v "wrong number of arguments to constructor"
          else checkArgs (0, ListPair.zip(vtyopts, tys));
          SOME resty
        end

      | NONE => 
        (errorVal v "expected sum type"; SOME resty)
    end

    (* @TODO akenn: Should check arity of exname *)
  | ExCon(exname, vs) => 
    (map (checkVal AtomPos env) vs; checkNonAtomAgainstPos(); SOME MILTys.topExn)

  | Tuple [] =>
    (checkAtomAgainstPos (); SOME (MILTy.prod []))

  | Tuple vs => 
    let val tyopts = map (checkVal AtomPos env) vs
    in
      checkNonAtomAgainstPos(); 
      if List.all isSome tyopts then SOME (MILTy.prod (map valOf tyopts)) else NONE
    end

  | Proj(i, n, v') => 
    (
      checkNonAtomAgainstPos();
      if i < 0 orelse i >= n then errorVal v "projection out of range" else ();
      case checkVal AtomPos env v' of
        NONE => NONE
      | SOME prodty =>
        case MILTy.fromProdCon prodty of
          SOME tys => 
  	  (if n<>length tys then error (text "expected arity of product type " ++ MILTy.pTy prodty ++ text " to be " ++ text(Int.toString n) ++ text " in" ++ line ++ pVal v) else ();
           SOME (List.nth(tys, i)))
       | NONE => (error (text "expected product/constructor/exn type; got " ++ MILTy.pTy prodty ++ text " in" ++ line ++ pVal v); NONE)
    )

  | TApp(v', tys) => 
    (
      app (ignore o checkTy env) tys;
      if checkPoly andalso (case v' of Var _ => false | _ => true) then errorVal v "expected type application of variable" else ();
      case checkVal AtomPos env v' of
        NONE => NONE
      | SOME polyty =>
        case MILTy.fromForall polyty of
          SOME (a as (kinds, ty)) => 
          (checkKinds v' env (tys, kinds); SOME (MILTy.app (MILTy.abs a, tys)))
  
        | _ => (errorVal v' "expected polymorphic type"; NONE)
    )

  | TAbs(tyvars, v') =>
    (
      app (checkTyVar env) tyvars;     
      if checkPoly andalso pos<>BindPos then errorVal v "Expected type abstraction to be bound to variable" else ();
      if not (#toplevel env) then errorVal v "Found unbound type abstraction under case, try or lambda" else ();
      case checkVal pos (extendKindEnv env tyvars) v' of
        NONE => NONE
      | SOME ty => SOME (MILTy.forall (tyvars, ty))
    )

  | As(v, ty) =>
    (
      checkVal AtomPos env v;
      checkTy env ty;
      SOME ty 
    )

  | Fold(v', ty) => 
    (
      checkTy env ty;
      case checkVal AtomPos env v' of
        NONE => SOME ty
      | SOME ty1 =>
        case MILTy.fromMu ty of
          NONE => (errorVal v "expected explicit recursive type in fold"; SOME ty)
        | SOME a => 
          let val ty2 = MILTy.unfold a
          in
            if MILTy.sub (ty1,ty2) 
            then SOME ty
            else (error (mismatch (ty2, ty1) ++ text " in " ++ pVal v); SOME ty)
          end
    )

  | Unfold v => 
    (
      case checkVal pos env v of
        NONE => NONE
      | SOME ty =>
        case MILTy.fromMu ty of	  
          NONE => (error (text "expected " ++ MILTy.pTy ty ++ text " to be a recursive type in " ++ line ++ pVal v); NONE)
        | SOME a => SOME (MILTy.unfold a)
    )
  end

  and checkTAbstr env (xs,e) =  
  (
    map (checkTy env) (map #2 xs);
    checkCmp (extendTyEnv env xs) e
  )

  (*--------------------------------------------------------------------*)
  (* Type check the computation term ce, return a computation type.     *)
  (*--------------------------------------------------------------------*)
  and checkCmp (env : Env) ce =
  let 
    fun checkVar (x,srcloc) =
      if checkDistinctVars andalso not (Var.isDummy x) andalso Var.Set.member(!boundVars, x)
      then error (text "duplicate bound variable " ++ text(Var.toString x))
      else (boundVars := Var.Set.add(!boundVars, x))

    fun checkCases checkTy (v, cases, optdefault, cty) =
    let
      val tyopt = checkVal AtomPos env v
      val res = SOME cty
      fun checkCases [] = 
          (case optdefault of 
            NONE => 
            res

        | SOME e => 
          (case checkCmp env e of
            NONE => res
          | SOME cty' => 
            if MILTy.subCmp(cty',cty) then res
            else (errorCmp e "type of default case does not match other cases"; res)
        ))

      | checkCases ((i, (vs, ce'))::cases) =
        let 
          val tys = case tyopt of SOME ty => checkTy ty i | NONE => []
        in
          if length tys <> length vs 
          then (errorCmp ce "wrong number of arguments in branch"; checkCases cases)
          else 
            case checkTAbstr env (ListPair.zip(vs,tys), ce') of
              NONE => checkCases cases
            | SOME cty' =>
              if MILTy.subCmp(cty', cty) then checkCases cases
              else (errorCmp ce' "branch does not match other cases"; checkCases cases)
         end
    in
      checkCases cases
    end                          

    fun checkLetFun (tyvars, kind, def, ce) =
    let    
      (* The environment under which the functions are checked *)
      val defnenv = 
        case def of 
          Fun _ => env
        | RecFun recbinds => extendFunEnv env (kind, map (fn (f, g, (vs,ce), cty) => 
          (g, MILTy.arrow(map #2 vs, cty))) recbinds)

      val defnenv = extendKindEnv defnenv tyvars

(*
      val defnenv = 
        if MILTermOps.isLocal kind 
        then (if closedBlocks then killTyEnv defnenv else defnenv) 
        else killBlockEnv defnenv
*)

      fun checkRecBinds ([], bodyenv) = 
          checkCmp bodyenv ce

        | checkRecBinds ((f, g, tabs as (xs,e), cty)::rest, bodyenv) = 
          let val ctyopt' = checkTAbstr defnenv tabs
          in
	    checkCmpTy defnenv cty;
(*            if MILTy.sub (MILTy.arrow(tys, cty'), MILTy.arrow(tys, cty)) then ()
            else errorCmp ce ("recursive function definition does not have correct type (" ^
              MILTy.cmpToString cty ^ " against " ^
              MILTy.cmpToString cty' ^ ")"); *)
            checkRecBinds 
              (rest, extendFunEnv bodyenv (kind, [(f, 
              MILTy.forall (tyvars, MILTy.arrow(map #2 xs,cty)))]))
          end

    in
      app (checkTyVar env) tyvars;
      case def of 
        Fun(f, tabs as (xs,e)) =>
        (case checkTAbstr defnenv tabs of
          NONE => NONE
        | SOME cty => checkCmp (extendFunEnv env (kind, [(f, MILTy.forall (tyvars, MILTy.arrow(map #2 xs, cty)))])) ce
        )

      | RecFun recbinds =>
        checkRecBinds (recbinds, env)
    end
  in
  case ce of

    App(v, vs) =>
    (
      case checkVal (case #pos env of TailPos => TailAppPos | _ => AppPos) env v of
        NONE => NONE
      | SOME vty =>
        case MILTy.fromArrow vty of
          NONE => (error (text "expected function type: got " ++ MILTy.pTy vty ++ text " in" ++ pVal v); NONE)
        | SOME (tys, cty) =>
          let val vtyopts = map (checkVal AtomPos env) vs
              fun checkArgs [] = ()
                | checkArgs ((NONE, ty)::rest) = checkArgs rest
                | checkArgs ((SOME vty, ty)::rest) =
                  if MILTy.sub (vty, ty) 
                  then () 
                  else (error (mismatch (ty, vty) ++ text " in argument to function in " ++ line ++ pCmp ce); checkArgs rest)
          in
            if length vs <> length tys
            then errorCmp ce "wrong number of arguments to function" else ();
            checkArgs (ListPair.zip(vtyopts, tys));
            SOME cty
          end
    )

  | Special(j, vs, cty) =>
    (
      map (checkVal AtomPos env) vs;
      checkCmpTy env cty;
      SOME cty
    )

  | Let(e1, (xs, e2)) =>
    (
      app (checkVar o #1) xs;
      app (ignore o checkTy env o #2) xs;
      let
        val ctyopt = 
          case e1 of 
            Triv vs => 
            let val vtyopts = map (checkVal BindPos env) vs 
            in
              if List.all isSome vtyopts 
              then SOME (MILTy.cmp(Effect.none, map valOf vtyopts))
              else NONE
            end
          | _ => 
            checkCmp (nonTailEnv env) e1
      in
        case ctyopt of
          NONE => ()
        | SOME cty =>
          let
            val (eff1,tys) = MILTy.fromCmp cty
            fun checkArgs ([],[]) = ()
              | checkArgs ([],_) = error (text "too few variables bound to computation")
              | checkArgs (_,[]) = error (text "too many variables bound to computation")
              | checkArgs (((x,_),ty1)::vars, ty2::tys) =
                (if MILTy.sub (ty2, ty1) then () 
                else error (text "type of term in let (" ++ MILTy.pTy ty2 ++ text ") does not match given bound variable type for " ++ text (Var.toString x) ++
                            text ": " ++ MILTy.pTy ty1);
                checkArgs (vars,tys))
          in
            checkArgs (xs,tys)
          end;
        checkCmp (extendTyEnv env xs) e2
      end
    )

  | Triv vs =>
    let val tyopts = map (checkVal RetPos env) vs
    in
      if List.all isSome tyopts
      then SOME(MILTy.cmp(Effect.none, map valOf tyopts))
      else NONE
    end

  | Case cases =>
    checkCases 
    (fn ty =>
     case MILTy.fromSum ty of 
       NONE => failCmp ce "expected sum type in case"
     | SOME tyss =>    
       fn i => 
         if i < 0 orelse i >= length tyss
         then failCmp ce "invalid constructor number in case" 
         else List.nth(tyss, i)) cases

  | CaseSCon cases =>
    checkCases 
    (fn ty =>
     case MILTy.fromTyname ty of
       NONE => failCmp ce "expected base type in case"
     | _ => (fn jcon => [])) cases

  | TypeCase cases =>
    checkCases
    (fn ty => 
     if MILTys.isTopExn ty 
     then (fn exnty => [exnty])
     else failCmp ce "expected exception type in case") cases

  | Throw(v, cty, _) =>
    (
      checkCmpTy env cty;
      (case checkVal AtomPos env v of
        NONE => ()
      | SOME vty => 
        if MILTys.isTopExn vty then ()
        else errorVal v "expected exception type in throw"
      );
      (* Need to check that exception stored in v is reflected in effect in cty *)
      SOME (cty)
    )

  | TryLet(try, handlers, cont) =>
    let
      val tryty = checkCmp ((*killBlockEnv*) env) try
      val contty = checkTAbstr env cont
      val ctyopts = map (checkTAbstr env) handlers
    in
      (* Need to check that handlers and continuation match up here *)
      contty
    end

  | LetFun a =>
    checkLetFun a

  | LetClass(classname, classinfo, fields, methods, e) =>
    let
      val defnenv = (*killBlockEnv*) env
      fun checkMethod (n, atts, ms, tys, tyopt, SOME (f,(vars,e))) =
          let 
            val argtys = 
              if Symbol.Set.member(ms, Id.staticSym)
              then tys
              else classname::tys
          in
            ignore 
            (checkTAbstr defnenv (ListPair.zip(vars,argtys), e))
          end

        | checkMethod m = ()

    in
      app checkMethod methods;
      checkCmp env e  
    end

  | LetVal(x, v, e) =>
    let val tyopt = checkVal BindPos env v
    in
      case tyopt of 
        NONE => NONE
      | SOME ty => checkTAbstr env ([(x,ty)], e)
    end

  | Encap e =>
    (
      case checkCmp ((*killBlockEnv*) env) e of
        NONE => NONE
      | SOME cty =>
        let val (eff, tys) = MILTy.fromCmp cty
        in
          SOME (MILTy.cmp(Effect.allocs, tys))
        end
    )

  end

in
  let val result = checkCmp emptyEnv wholece
  in
    if !errors then PrintManager.println "Warning: type check errors; see log for details" else ();
    result
  end
end

fun checkAll e = check {closedBlocks=false,checkAtoms=false,checkDistinctTyVars=true,checkDistinctVars=true,checkCC=false,checkPoly=false} e
end (* of local open *)
end (* of struct *)

