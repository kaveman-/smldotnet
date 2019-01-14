(*======================================================================*)
(* Pattern match compilation.						*)
(* Adapted from Moscow ML implementation, with changes:			*)
(*   everything must be typed in MIL					*)
(*   implemented or patterns						*)
(*   implemented cast patterns						*)
(*   excon matching sequential because of sep comp AND subclassing.	*)
(*======================================================================*)
structure Pat :> PAT =
struct

local
open PatDec PatDescr PatPath TransOps

val [warnmatch, warnbind] = map (Controls.add true) ["warn.match", "warn.bind"]

structure T = MILTerm
in

(*----------------------------------------------------------------------*)
(* Compile decision tree to MIL.					*)
(*----------------------------------------------------------------------*)
fun compile EE (argvar : MILTerm.BoundVar) (funvars : MILTerm.BoundVar list) 
  (ref {tree, ...} : decision) (failLam : MILTerm.Cmp) (rhsty : MILTy.CmpType) =
    let 

      val restore = MILTermOps.setAnonVarName [Id.fromString "!Pat.compile"]
  (*..................................................................*)
  (* For a particular constructor variety, return the "tag" that      *)
  (* is used to identify the case, and a list of fresh variables      *)
  (* for the constructor arguments.				      *)
  (*..................................................................*)
      fun getSCon (PatCon.SCon scon) = (scon, [])
        | getSCon _ = Debug.fail "Pat.getSCon"

      fun envPlus env path (arity,tag) =
      if arity = 0 then (env, [])
      else 
      let val (x,_) = freshAnonVar ()
      in (PatPath.insert env (PatPath.ConArg tag::path, #1 x), [x]) end
      
      
      fun getExCon (PatCon.ExCon(exname, arity)) = 
          (exname, TyName.Map.find(EE, exname), arity)
        | getExCon _ = Debug.fail "Pat.getExCon"

(*
	fun getVec (Vec n)           = Const.INTscon n
	  | getVec _                 = Debug.fail "Match.getVec"
*)

      fun collect getcon last cases
	 (otherwise : decision as 
	  ref {tree = IfEq(obj, con, thenact, elseact), ...}) =
	  if PatPath.equal(obj, last) andalso not (shared otherwise) 
          then 
          let val (i, xs) = getcon con
          in collect getcon last ((i, (xs, thenact)) :: cases) elseact end
          else (cases, otherwise) 
	| collect _ _ cases otherwise = 
	  (cases, otherwise)

      fun revmap f xys = 
      let fun loop []            res = res
	    | loop ((x, y)::xyr) res = loop xyr ((x, f y) :: res)
      in loop xys [] end

      fun toseq env Failure       = failLam
	| toseq env (Success(rhs,pathenv)) = 
          let
            val (C, vs) = 
            Symbol.Map.foldri (fn (var,path, (C,vs)) =>
            let
              val (C', v) = PatPath.compile env (SOME var,path)
            in
              (C o C', v::vs)
            end) (Gen.identity, []) pathenv
            val f = List.nth (funvars, rhs)
          in
            C(T.App(T.Var (#1 f), vs))
          end

	| toseq env t = mkSwitch env t	

      and mkSwitch env d =
      let
        fun share env (node : PatDec.decision as ref {tree, term, funvar, ...}) =
(*
        if shared node 
        then
	  case !term of
            NONE => 
	    let 
              val body = toseq env tree
              val app = T.App(T.Var funvar, [])
            in 
              term := SOME app;
              (fn e => T.LetFun([], T.AnyFun, T.Fun(funvar, ([],body)), e),app)
            end

          | SOME lam => 
            (fn e => e, lam)

        else *) (fn e => e, toseq env tree)
      in
        case d of
          IfEq(obj, PatCon.SCon scon, thenact, elseact) =>
	  let 
            val (cases, otherwise) = collect getSCon obj [] elseact 
            val (C, t) = share env thenact
            val (C', t') = share env otherwise
            val (C, cases) =
              foldr (fn ((i,(xs,s)), (C, cases)) =>
              let 
                val (C', t) = share env s in (C o C', (i,(xs, t))::cases) 
              end)
                (C o C', []) cases
            val (C'', v) = PatPath.compile env (NONE,obj)
	  in 
	    C (C'' (T.CaseSCon(v, (scon, ([], t)) :: cases, SOME t', rhsty)))
	  end
(*
	  | IfEq(obj, con as Vec _, thenact, elseact) =>
	    let val (cases, otherwise) = collect getVec obj [] elseact 
	    in 
		Lstatichandle(Lcase(obj, (getVec con, share env thenact)
                                         :: revmap (share env) cases),
			      share env otherwise)
	    end
*)

        | IfEq(obj, con as PatCon.Con {span, tag, arity, ...}, 
            thenact, elseact) => 
	  let 
            val (env',xs) = envPlus env obj (arity,tag)
            fun collect (result as (cases, C, otherwise : decision as 
	      ref {tree = IfEq(obj', con as PatCon.Con {tag, arity, ...}, 
                thenact, elseact), ...})) =
	      if PatPath.equal(obj, obj') andalso not (shared otherwise) 
              then 
              let 
                val (env',xs) = envPlus env obj (arity,tag)
                val (C', t) = share env' thenact
              in 
                collect ((tag, (xs, t)) :: cases, C' o C, elseact)
              end
              else result

            | collect result = result

	    val (cases, C1, otherwise) = collect ([], fn e => e, elseact)
            val (C2, t) = share env' thenact
          in
            if length span - 2 = length cases
            then
            let
              val all = IntSet.addList(IntSet.empty, List.tabulate(length span,
                Gen.identity))
              val left = foldr (fn ((i,_),s) => IntSet.delete(s,i)) all cases
              val left = IntSet.delete(left, tag)
              val [i'] = IntSet.listItems left
              val (env',xs') = envPlus env obj 
                (List.nth (span,i'),i')
              val (C3, t') = share env' otherwise
              val (C4, v) = PatPath.compile env (NONE,obj)
            in
	      C1 (C2 (C3 (C4
              (T.Case(v, (i', (xs', t')) :: (tag, (xs, t)) :: cases, NONE, rhsty)))))
            end
            else
            let
              val (C3, t') = share env otherwise
              val (C4, v) = PatPath.compile env (NONE,obj)
            in
	      C1 (C2 (C3 (C4 
              (T.Case(v, (tag, (xs, t)) :: cases, SOME t', rhsty)))))
            end
          end

        | IfEq(obj, con as PatCon.ExCon _, thenact, elseact) =>           
	  let 
            val (exname, varopt, argtys) = getExCon con            
            val (env',xs) = envPlus env obj (length argtys,0)
            val (C1, t) = share env' thenact
            val (C2, v) = PatPath.compile env (NONE,obj)
            fun collect (result as (cases, C, otherwise : decision as 
	      ref {tree = IfEq(obj', con, thenact, elseact), ...})) =
	      if PatPath.equal(obj, obj') andalso not (shared otherwise) 
              then 
              let 
                val (exname, varopt, argtys) = getExCon con
                val milexty = MILTy.exn (Exn.exn [exname], argtys)
                val (convar,convarv) = freshAnonVar ()
              in
                if isSome varopt then result
                else
                let 
                  val n = length argtys
                  val (env',xs) = envPlus env obj (n,0)
                  val (C', t) = share env' thenact
                  val t = ListOps.foldri 
                   (fn (i, x, e) => T.LetVal(x, T.Proj(i, n, convarv), e)) t xs
                in 
                  collect ((milexty, ([convar], t)) :: cases, C' o C, elseact)
                end
              end
              else result

            | collect result = result

          in 
            case varopt of
              NONE =>
              let
   	        val (cases, C3, otherwise) = collect ([], fn e => e, elseact)
            	val (C4, t') = share env otherwise
                val milexty = MILTy.exn (Exn.exn [exname], argtys)
                val (convar,convarv) = freshAnonVar ()
                val n = length xs
                val t = ListOps.foldri 
                 (fn (i, x, e) => T.LetVal(x, T.Proj(i, n, convarv), e)) t xs
              in
            	C1 (C2 (C3 (C4
              	(T.TypeCase(v, (milexty, ([convar], t)) :: rev cases, SOME t', rhsty)))))
              end

            | SOME genvar =>
              let
                val (C3, t') = share env elseact
                val (x,xv) = freshAnonVar ()                
                val (b,bv) = freshAnonVar ()
                val milexty = MILTy.exn (Exn.exn [exname], MILTys.int::argtys)
                val (convar,convarv) = freshAnonVar ()
                val t = 
                  T.Let(T.Special((Ext.Prim (Id.fromString "eq"), NONE, NONE),
                    [T.Var genvar, xv], MILTy.noeffect [MILTys.bool]),
                    ([(b, MILTys.bool)], MILTermOps.cond(bv, t, t', rhsty)))                
                val n = length xs + 1
                val t = ListOps.foldri 
                 (fn (i, x, e) => T.LetVal(x, T.Proj(i, n, convarv), e)) t (x::xs)
              in
	        C1 (C2 (C3 
                (T.TypeCase(v, [(milexty, ([convar], t))], SOME t', rhsty))))
              end
          end

        | IfEq(obj, con as PatCon.Cast castty, thenact, elseact) => 
	  let 
            val (C1, t1) = share env thenact
            val (C2, t2) = share env elseact
            val (C3, v) = PatPath.compile env (NONE,obj)
            val (b,bv) = freshAnonVar ()
          in
            C1 (C2 (C3
              (T.Let(T.Special((Ext.InstanceOf, SOME castty, NONE),
                [v], MILTy.noeffect [MILTys.bool]), ([(b, MILTys.bool)],
                MILTermOps.cond (bv, t1, t2, rhsty))))))
          end

      end
    in restore();
       toseq (emptyEnv (#1 argvar)) tree 
    end

val decmemo = ref [mkDecision Failure]

(*@BUG: why don't we pass in freshBoundVar too? *)
fun preTrans (transExp, transType, freshAnonVar, freshBoundVar:(Syntax.Location option * Syntax.symbol) -> 
                       (MILTerm.BoundVar * MILTerm.Val),  VE, EE, lhsty,
  match : SMLTerm.Match as (rhsty,rules as (loc,_,_)::_)) =
let

  val restore = MILTermOps.setAnonVarName [Id.fromString "!Pat.preTrans"]

  (* Translate or-patterns. Result is a list of (pattern, rule number) pairs *)
  val mrules = PatOrs.translate (map #2 rules)

  fun transEqn ((loc, pat, exp as (locexp,_)), (defs, cty)) =
  let
    val fvs = SMLTermOps.fvPat pat
    val typedvars = map (fn (x,ty) => (#1 (freshBoundVar (NONE,x)), transType ty)) 
      (Symbol.Map.listItemsi fvs)
    val VE' : TransOps.ValEnv = foldr (fn (((x,longid),ty), VE) =>
      Symbol.Map.insert(VE, List.last longid, ((x,ty), []))) VE typedvars
    val (e,cty') = transExp VE' exp

    (*@NOTE: sad attempt to preserve variable names *)
    val (defvar,_) = case pat of SMLTerm.PatVar (x,_) => freshBoundVar(NONE,x)
                              | _ => freshAnonVar (SOME locexp)
    val def = (defvar, (typedvars, e))
  in
    (def::defs, MILTy.unionCmpTypes (cty,cty'))
  end

  (* Compile rhs of each rule to produce a non-recursive function def *)
  val (defs, cty) = foldr transEqn ([], MILTy.noeffect []) rules

  (* Create a decision DAG from the patterns *)
  val decdag = PatDecComp.make transType lhsty mrules
  
  val _ = (incrnode decdag; decmemo := decdag :: !decmemo)
in
  restore();
  (decdag, defs, cty)
end

(*----------------------------------------------------------------------*)
(* Do it!								*)
(*----------------------------------------------------------------------*)
fun transFn { sourcemap, entity, transExp, transType, 
	      freshAnonVar, freshBoundVar,
	      VE, EE, smlty = lhsty, 
	      match : SMLTerm.Match as (rhsty,rules as (loc,_,_)::_)}=
let
  val restore = MILTermOps.setAnonVarName [Id.fromString "!Pat.transFn"]
  val (decdag, defs, cty) = preTrans (transExp, transType, freshAnonVar, freshBoundVar, VE, EE, lhsty, match)

  (* Check for redundant/nonexhaustive matches *)
  val (rulesused, nonexhaustive) = PatDec.tally decdag
  val rulesnotused = 
    IntSet.difference (IntSet.addList(IntSet.empty, 
      List.tabulate (length rules, Gen.identity)), rulesused)

  val (failfun,failfunv) = freshAnonVar NONE
  fun surround e [] = e
    | surround e (def::rest) = 
      T.LetFun([], T.LocalFun, T.Fun def, surround e rest)

  val (argvar,_) = freshAnonVar (SOME loc) (* was NONE *)
  val e = compile EE argvar (map #1 defs) decdag (T.App(failfunv, [])) (MILTy.cmp(Effect.any, [transType rhsty]))
  val defs = 
  if nonexhaustive
  then
    let 
      val (x,xv) = freshBoundVar (NONE,Id.fromString "Match")
      val milty = transType rhsty
      val cty = MILTy.cmp(Effect.throws (Exn.exn[TyNames.matchTyName]), [milty])
    in 
      (failfun, 
        ([], T.LetVal(x, T.ExCon(MILTy.exn(Exn.exn[TyNames.matchTyName],[]), []),
          T.Throw(xv, cty, throwMessage(sourcemap,entity,loc)))))::defs
    end
  else defs

  val e' = surround e defs
in
  if nonexhaustive andalso Controls.get warnmatch
  then TransOps.addError (Error.warning (loc, " match not exhaustive")) 
  else ();

  IntSet.app (fn i => TransOps.addError 
    (Error.warning (#1 (List.nth (rules, i)), 
      " match redundant"))) 
    rulesnotused;
  restore();
  (([(argvar, transType lhsty)], e'), cty)
end


fun transHandle 
{ 
  sourcemap, entity, transExp, transType, 
  freshAnonVar, freshBoundVar,
  VE, EE, 
  exp as (exploc,_),
  match = match as (rhsty,rules as (loc,_,_)::_)
}  =
let

  val restore = MILTermOps.setAnonVarName [Id.fromString "!Pat.transHandle"]    
  val (ce,cty) = transExp VE exp
  val (decdag, defs, cty') = 
    preTrans (transExp, transType, freshAnonVar, freshBoundVar, VE, EE, SMLPrimTy.exnType, match)

  (* Check for redundant/nonexhaustive matches *)
  val (rulesused, nonexhaustive) = PatDec.tally decdag
  val rulesnotused = 
    IntSet.difference (IntSet.addList(IntSet.empty, 
      List.tabulate (length rules, Gen.identity)), rulesused)

  val (failfun,failfunv) = freshAnonVar NONE
  fun surround e [] = e
    | surround e (def::rest) = 
      T.LetFun([], T.LocalFun, T.Fun def, surround e rest)

  val (argvar,argvarterm) = freshAnonVar (SOME loc) (* was NONE *)
  val (resvar,resvarterm) = freshAnonVar (SOME exploc)
  val resty = transType rhsty
  val e = compile EE argvar (map #1 defs) decdag (T.App(failfunv, [])) (MILTy.cmp(Effect.any, [resty]))
  val defs = 
  if nonexhaustive 
  then
    (failfun, 
      ([], T.Throw(argvarterm, MILTy.cmp(Effect.throwsAny, [resty]), "")))::defs
  else defs

  val e' = surround e defs
in
  IntSet.app (fn i => TransOps.addError 
    (Error.warning (#1 (List.nth  (rules, i)), 
      " match redundant"))) 
    rulesnotused;
  restore();
  (T.TryLet(ce, [([(argvar, MILTys.topExn)], e')], ([(resvar, resty)],
    T.Triv [resvarterm])), MILTy.unionCmpTypes(cty,cty'))
end

fun transLetPat
{ 
  transExp, transType, 
  freshAnonVar, freshBoundVar,
  VE, EE, var, smlty, fail = (fail,_),
  pat, loc
} =
let
  val restore = MILTermOps.setAnonVarName [Id.fromString "!Pat.transLetPat"]    
  (* Translate or-patterns. Result is a list of (pattern, rule number) pairs *)
  val mrules = PatOrs.translate [pat]

  (* Create a decision DAG from the patterns *)
  val decdag = PatDecComp.make transType smlty mrules
  
  val _ = incrnode decdag

  (* Check for redundant/nonexhaustive matches *)
  val (rulesused, nonexhaustive) = PatDec.tally decdag

  (* The success function *)
  val (successvar,_) = freshAnonVar (SOME loc) (* was NONE *)
  val (failvar,failvarterm) = freshAnonVar NONE

  val milty = transType smlty
  val e = compile EE var [successvar] decdag (T.App(failvarterm, [])) (MILTy.cmp(Effect.any, [milty]))

  val fvs = SMLTermOps.fvPat pat

  fun fst(a,_)=a

  val typedvars = map (fn (v,ty) => (fst(freshBoundVar (NONE,v)), transType ty))
    (Symbol.Map.listItemsi fvs)

  val (e,cty) = 
  if nonexhaustive 
  then
    (T.LetFun([], T.LocalFun, T.Fun (failvar, ([],fail)), e), MILTy.cmp (Effect.throwsAny (* TyName.bindExName *), [milty]))
  else (e, MILTy.cmp (Effect.none, [milty]))

  val e = T.LetFun([], T.LocalFun, 
    T.Fun (successvar, (typedvars, T.Triv (map (T.Var o #1 o #1) typedvars))), e)
in
  if nonexhaustive andalso Controls.get warnbind
  then TransOps.addError (Error.warning (loc, " binding not exhaustive")) 
  else ();
  restore();
  (e, cty)
end (* of let *)
end (* of local *)
end (* of struct *)