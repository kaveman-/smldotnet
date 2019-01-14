(*======================================================================*)
(* Translate a valuable pattern into a series of projections and        *)
(* unfoldings.                                                          *)
(*======================================================================*)
structure ValPat :> VALPAT =
struct

local 
  open SMLTerm SMLTy TransOps 
  structure T = MILTerm
in

fun trans 
  { freshAnonVar: Syntax.Location option ->
                  (MILTerm.BoundVar * MILTerm.Val),
    freshBoundVar: (Syntax.Location option * Syntax.symbol) -> 
                       (MILTerm.BoundVar * MILTerm.Val),
    TVE : MILTy.Type TyVar.Map.map, 
    TNE : MILTy.Type TyName.Map.map,
    tyvars : TyVar.TyVar list,
    pat : SMLTerm.Pat, 
    var = x : MILTerm.BoundVar, 
    smlty : SMLTy.Type 
  } =
let
  val restore = MILTermOps.setAnonVarName [Id.fromString "!ValPat.trans"]    

  val (TVE', miltyvars) = freshTyVars (TVE, tyvars)

  fun getVar pat = case getPatVar pat of SOME v => freshBoundVar (NONE,v) | NONE => freshAnonVar NONE

  fun transType smlty = 
    MILTy.forall(miltyvars, TransType.transType TVE' TNE smlty)

  fun trans' (pat, x, smlty) =
  let
    fun transRec patrow (binds, VE : TransOps.ValEnv) =
    case patrow of
      [] => 
      (binds, VE)

    | (lab, pat)::patrow =>
      let 
        val (fldty, i, n) = SMLTy.fieldType (smlty, lab)
        val (x',xv') = getVar pat
        val (binds', VE') = trans' (pat, x', fldty)
      in
        transRec patrow (binds @ (x', 
          MILTermOps.tabs(miltyvars, 
            T.Proj(i,n, 
            MILTermOps.tapp(T.Var (#1 x), map (MILTy.tyvar o #1) miltyvars)))) 
            :: binds',
          TransOps.merge (VE, VE'))
      end
  in
    case pat of
      PatWild => 
      ([], Symbol.Map.empty)

    | PatVar(v,smlty) =>
      ([], Symbol.Map.insert 
        (Symbol.Map.empty, v, (((#1 x), transType smlty), [])))

    | PatCon(con, defs as (tvs, tyname, CE), tys, patopt) =>
      let 
        val (i, tyopt) = case SMLTy.findCon(tyname, CE, con) of 
			    SOME res => res
                          | NONE => Debug.fail "ValPat.PatCon" 
        val tyopt = Option.map (SMLTy.appSubst (ListPair.zip(tvs, tys))) tyopt
      in
        case (patopt, tyopt) of
          (NONE, NONE) =>
          ([], Symbol.Map.empty)

(*        | (SOME pat, SOME smlty) =>
          let
            val (x',xv') = getVar(pat)
            val (binds, VE) = trans' (pat, x', smlty)
          in
            ((x', MILTermOps.tabs(miltyvars, T.Unfold (MILTermOps.tapp
              (T.Var (#1 x), map (MILTy.tyvar o #1) miltyvars))))::binds, VE)
          end
*)
        | (SOME pat, SOME smlty) =>
          let
	    val ty = SMLTy.consType(tys,tyname)
            val (x',xv') = getVar pat
            val (binds, VE) = trans' (pat, x', smlty)
          in
	      case InterOp.isEnumType ty of
		  SOME ty' =>  
		      ((x',
			MILTermOps.tabs(miltyvars, T.As(MILTermOps.tapp
								 (T.Var (#1 x), map (MILTy.tyvar o #1) miltyvars),
					                        TransType.transType TVE' TNE ty')))
			::binds, 
			VE)
	      | NONE => 
		      ((x', MILTermOps.tabs(miltyvars, T.Unfold (MILTermOps.tapp
								 (T.Var (#1 x), map (MILTy.tyvar o #1) miltyvars))))::binds, VE)
          end

          
        | _ =>
          Debug.fail "ValPat.trans: arity mismatch"
      end

    | PatRecord(openrec, patrow) =>
      transRec patrow ([], Symbol.Map.empty)

    | PatLayer((v,smlty'), pat) => 
      let
        val (binds, VE) = trans' (pat, x, smlty)
      in
        (binds, Symbol.Map.insert (VE, v, (((#1 x), transType smlty'), [])))
      end

    | _ =>
      Debug.fail "ValPat.trans: refutable pattern"

  end

  val (binds, VE) = trans' (pat, x, smlty)
in
  restore();
  { bindings = binds, VE = VE, TVE = TVE', tyvars = miltyvars }
end

end (* of local open *)
end (* of struct *)
