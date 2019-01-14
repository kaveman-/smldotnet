 (*======================================================================*)
(* Pattern match compilation; make decision tree from match.            *)
(*======================================================================*)
structure PatDecComp :> PATDECCOMP =
struct

open PatDec PatDescr PatPath

(*----------------------------------------------------------------------*)
(* make (transtype,EE) ty allpats                                       *)
(*   transtype : SMLTy.Type -> MILTy.Type       translation for types   *)
(*   ty : SMLTy.Type                            type of entire pattern  *)
(*   allpats : (SMLTerm.Pat*int) list           patterns with eqn nums  *)
(* Result: a decision tree.                                             *)
(*----------------------------------------------------------------------*)
fun make transtype ty (allpats as (pat1,_) :: _) = 
    let 
      val failure = mkDecision Failure
      val table = PatDec.Hash.mkTable (37, Match)
      val objs1 = [[]]
      val tys1 = [ty]

      val topCon = PatCon.Tup 1 (* Hack to handle top-level pat *)
      val topctx = [(topCon, [])] : context

      val allmrules = map (fn (pat,i) => ([pat], i)) allpats

      fun fail _              []                          = failure
        | fail (Pos(_, dscs)) ((pats1, rhs1) :: rulerest) =
          succeed Symbol.Map.empty 
            topctx [(tys1, pats1, objs1, dscs)] rhs1 rulerest
        | fail _ _ = Debug.fail "PatDag.fail"

      and succeed env ctx [] rhs rules = mkDecision (Success(rhs, env))
        | succeed env ctx (work1::workrest) rhs rules = 
          case work1 of 
            ([], [], [], []) => succeed env ctx workrest rhs rules
          | (ty1::tyrest, pat1::patrest, obj1::objrest, dsc1::dscrest) =>
            match env ty1 pat1 obj1 dsc1 ctx 
            ((tyrest, patrest, objrest, dscrest) :: workrest) rhs rules
          | _ => Debug.fail "PatDag.succeed"

      and mktest env pcon obj dsc ctx work rhs rules conequal =
        case staticmatch pcon dsc of
          Yes   => 
          conequal dsc

        | No    => 
          fail (builddsc ctx dsc work) rules

        | Maybe => 
          unique table (IfEq(obj, pcon, 
            conequal (Pos(pcon, bots (PatCon.arity pcon))),
            fail (builddsc ctx (addneg dsc pcon) work) rules))

      and match env ty pat obj dsc ctx work rhs rules =
        case pat of
          SMLTerm.PatSCon (scon, loc) => 
          (case TransSCon.trans (scon, transtype ty) of
            NONE =>
            (TransOps.addError (Error.error (loc, SMLTy.toString ty ^ 
              " constant in pattern is out-of-range")); failure)

          | SOME c =>
            let
              fun conequal newdsc = 
                succeed env (apply ctx newdsc) work rhs rules
            in 
              mktest env (PatCon.SCon c) obj dsc ctx work rhs rules conequal 
            end
          )

        | SMLTerm.PatLiteral c => 
            let
              fun conequal newdsc = 
                succeed env (apply ctx newdsc) work rhs rules
            in 
              mktest env (PatCon.SCon c) obj dsc ctx work rhs rules conequal 
            end
(*@TODO: vector patterns
        | SMLTerm.PatVec pats =>
            let val arity = List.length pats
                val pcon = PatCon.Vec arity
                fun getsargs (Neg _)           = bots arity
                  | getsargs (Pos(con, sargs)) = sargs
                fun conequal newdsc =
                    case pats of
                        [] => succeed env (apply ctx newdsc) work rhs rules
                      | _  => 
                        succeed env ((pcon, []) :: ctx) 
                          ((pats, splitPath arity obj, getsargs dsc) :: work)
                          rhs rules
            in 
              mktest pcon (Lprim(Prim.Pvectlength, [obj])) dsc ctx work rhs 
                rules conequal
            end
*)
        | SMLTerm.PatWild =>
          succeed env (apply ctx dsc) work rhs rules

        | SMLTerm.PatVar(id,ty) =>
          succeed (Symbol.Map.insert(env, id, obj)) 
            (apply ctx dsc) work rhs rules          

        | SMLTerm.PatLayer((id,ty),pat) =>
          match (Symbol.Map.insert(env, id, obj))
            ty pat obj dsc ctx work rhs rules

        | SMLTerm.PatCon (c, (tyvars, tyname, CE), tys, patopt) =>
          let
            val (i, tyopt) = 
                case SMLTy.findCon (tyname, CE, c) of 
                  SOME res => res 
                | NONE => Debug.fail "PatDecComp:SMLTerm.PatCon"
            val tyopt = Option.map 
              (SMLTy.appSubst (ListPair.zip(tyvars, tys))) tyopt
            fun makeArity NONE = 0 
              | makeArity (SOME _) = 1
            val span = map makeArity (map #2 (Id.fixMap CE))
            val pcon = 
              PatCon.Con { tag = i, span = span, arity = makeArity tyopt }
            val isenum = InterOp.isEnumType (SMLTy.consType(tys, tyname))
            val obj = case isenum of
                           SOME ty => PatPath.NopCast (transtype ty)::obj
                         | NONE => PatPath.Unfold::obj
          in
            case (patopt,tyopt) of
              (NONE, NONE) =>              
              let 
                fun conequal newdsc = 
                  succeed env (apply ctx newdsc) work rhs rules
              in 
                mktest env pcon obj dsc ctx work rhs rules conequal 
              end

            | (SOME pat, SOME ty) =>
              let 
                val oarg = 
                  if length span = 1 then obj else PatPath.ConArg i::obj
                fun getsargs (Neg _)           = [Bot]
                  | getsargs (Pos(con, sargs)) = sargs
                fun conequal newdsc =
                  succeed env ((pcon, []) :: ctx) 
                    (([ty], [pat], [oarg], getsargs dsc) :: work) rhs rules
              in 
                mktest env pcon obj dsc ctx work rhs rules conequal 
              end
          end

        | SMLTerm.PatExCon (exname, typatopt) =>
          let
            val pcon = PatCon.ExCon (exname, 
              case typatopt of NONE => [] | SOME (ty,_) => [transtype ty])
          in
            case typatopt of
              NONE =>
              let
                fun conequal newdsc = 
                    succeed env (apply ctx newdsc) work rhs rules
              in 
                mktest env pcon obj dsc ctx work rhs rules conequal 
              end

            | SOME (ty, pat) =>
              let 
                (* Number shouldn't matter *)
                val oarg = PatPath.ConArg 0::obj
                fun getsargs (Neg _)           = [Bot]
                  | getsargs (Pos(con, sargs)) = sargs
                fun conequal newdsc =
                  succeed env ((pcon, []) :: ctx) 
                    (([ty], [pat], [oarg], getsargs dsc) :: work) rhs rules
              in 
                mktest env pcon obj dsc ctx work rhs rules conequal 
              end
          end

          (* The irrefutable pattern () or {} *)
        | SMLTerm.PatRecord(false, []) =>
          succeed env (apply ctx dsc) work rhs rules

        | SMLTerm.PatRecord(_, pats) =>
          let 
            val SOME rowty = SMLTy.fromRecType ty
            val arity = length rowty
            val sargs = 
              case dsc of 
                Neg _         => bots arity
              | Pos(_, sargs) => sargs
            val (pats, paths) = ListPair.unzip(ListOps.mapi (fn (i,(lab,ty)) =>
              case List.find (fn (lab',_) => Symbol.equal(lab,lab')) pats of
                NONE => (SMLTerm.PatWild, Proj (i,arity) :: obj)
              | SOME (_,pat) => (pat, Proj (i,arity) :: obj)) rowty)
          in 
            succeed env ((PatCon.Tup arity, []) :: ctx)
              ((map #2 rowty, pats, paths, sargs) :: work) 
              rhs rules
          end

        | SMLTerm.PatRef pat =>
          let
            val SOME (ty,_) = SMLTy.fromRefType ty
          in
            match env ty pat (PatPath.Deref (transtype ty) :: obj)
              dsc ctx work rhs rules
          end

        | SMLTerm.PatCast (id, ty) =>
          let
            val milty = transtype ty
            val pcon = PatCon.Cast milty
            val oarg = PatPath.Cast milty::obj
            val env' = Symbol.Map.insert(env, id, PatPath.Cast milty::obj)
            fun conequal newdsc = 
                succeed env' (apply ctx newdsc) work rhs rules
          in 
            mktest env pcon obj dsc ctx work rhs rules conequal 
          end
in 
  fail (Pos(topCon, bots 1)) allmrules
end

end