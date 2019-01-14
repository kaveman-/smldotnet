(*======================================================================*)
(* Auxiliary stuff for elaboration					*)
(*======================================================================*)
structure ElabOps :> ELABOPS = 
struct

local
  open Syntax SMLTy SMLPrimTy EnvOps ElabState
in

structure T = SMLTerm
structure Map = Symbol.Map

fun vidToLongid (Short id) = [id]
  | vidToLongid (Long longid) = longid
  | vidToLongid (OpShort id) = [id]

(*----------------------------------------------------------------------*)
(* Merge two pattern environments (bindings of pattern variables to 	*)
(* their types and paths) raising an error if duplicates are found,	*)
(* enforcing the first "syntactic" restriction in Section 2.9 of Defn.	*)  
(*----------------------------------------------------------------------*)
fun patmerge loc (m1, m2) =
  let fun f [] = m1
        | f ((x,v)::m') = 
          let val m = f m'
          in
            case Map.find (m,x) of
              NONE => Map.insert(m,x,v)
          | _ => 
            (error (Error.error(loc, "duplicate variable in pattern: " 
              ^ Id.toString x), []); m)
          end
  in 
    f (Map.listItemsi m2) 
  end

(*----------------------------------------------------------------------*)
(* A monotyped unqualified variable					*)
(*----------------------------------------------------------------------*)
fun monovar (loc,v) = (loc,T.Var((v,[]),[]))

(*----------------------------------------------------------------------*)
(* Typed term for a case construct					*)
(*----------------------------------------------------------------------*)
fun caseTerm (loc, e, ty, match) = (loc,T.App((loc,T.Fn(ty, match)), e))

(*----------------------------------------------------------------------*)
(* Typed term for a conditional.					*)
(*----------------------------------------------------------------------*)
fun condTerm (loc, e1, (e2,loc2), (e3,loc3), ty) =
  caseTerm(loc, e1, boolType,
    (ty, [(loc2, T.PatCon(Id.trueSym, TopEnv.boolCE, [], NONE), e2), 
          (loc3, T.PatCon(Id.falseSym, TopEnv.boolCE, [], NONE), e3)]))

(*----------------------------------------------------------------------*)
(* Typed term for a tuple.						*)
(*----------------------------------------------------------------------*)
fun tupleTerm (loc,etys) =
let fun make [] n = []
      | make ((e,ty)::etys) n = (Id.fromString (Int.toString n), e,ty) :: make etys (n+1)
in
  (loc,T.Record (make etys 1))
end

(*----------------------------------------------------------------------*)
(* List pattern derived form						*)
(*----------------------------------------------------------------------*)
fun patList loc [] = 
    (loc, PatVar(Short Id.nilSym))

  | patList loc ((pat as (loc',_))::pats) = 
    (loc, PatCon([Id.consSym], 
      (loc, PatRecord(false, 
      [(Id.fromString "1", pat), (Id.fromString "2", patList loc' pats)]))))

(*----------------------------------------------------------------------*)
(* While derived form							*)
(*----------------------------------------------------------------------*)
fun makeWhileTerm (loc, e1, e2) =
  let
    val v = freshVar ()
  in
   (loc,
    T.Let([T.ValRec(
    [], 
    [(v, 
      (loc,T.Fn(unitType, 
           (unitType, [(loc, T.PatWild, 
 			condTerm(loc,
			  e1, 
			  ((loc,T.Let([T.Val(loc, [], unitType, T.PatWild, e2)], 
				 ((loc,T.App(monovar(loc,v), (loc,T.Record[])))))), loc), 
			  ((loc,T.Record[]), loc),
			  unitType)
            )]))),
      SMLTy.funType (unitType, unitType
     ))])],
    (loc,T.App(monovar (loc,v), (loc,T.Record [])))))
  end

  

(*----------------------------------------------------------------------*)
(* Construct fn x1 => ... => fn xn => e					*)
(*----------------------------------------------------------------------*)
fun abs [] e = e
  | abs ((vloc,v)::vs) (e as (loc,_))  = 
    (loc, Fn [((vloc, PatVar(Short v)), abs vs e)])

(*----------------------------------------------------------------------*)
(* Tuple pattern derived form						*)
(*----------------------------------------------------------------------*)
fun patTuple loc [pat] = pat
  | patTuple loc pats =
let fun make [] n = []
      | make (pat::pats) n = 
        (Id.fromString (Int.toString n), pat) :: make pats (n+1)
in
  (loc, PatRecord(false, make pats 1))
end

(*----------------------------------------------------------------------*)
(* Tuple expression derived form					*)
(*----------------------------------------------------------------------*)
fun expTuple loc [exp] = exp
  | expTuple loc exps =
let fun make [] n = []
      | make (exp::exps) n = 
        (Id.fromString (Int.toString n), exp) :: make exps (n+1)
in
  (loc, Record(make exps 1))
end

(*----------------------------------------------------------------------*)
(* Generate code corresponding to a source-level open construct		*)
(*----------------------------------------------------------------------*)
fun makeOpen (loc, E, (topstrid, path) : T.longid) =
let
  val (i1,d1) = 
    foldl (fn 
      ((strid, E), (i, d)) => (i+1, 
      T.Structure(strid, 
        T.Strid(topstrid, path @ [(strid,i)]))::d))
    (0, []) 
    (Id.fixMap (SEofE E))

  val (i2,d2) = 
    foldl (fn 
      ((id, ValBind.VarSch(SMLSch.TypeScheme(tyvars,ty))), (i, d)) => 
      (case map TyVar.sort tyvars of
        [TyVar.Overloaded tynameset] => 
         (i+1,
         T.Val(loc, [], ty, T.PatVar (id, ty), 
           (loc,T.OverloadedVar((topstrid, path @ [(id,i)]), tynameset, [])))::d)

      | _ =>
         (i+1,
         T.Val(loc, tyvars, ty, 
         T.PatVar (id, ty), (loc,T.Var((topstrid, path @ [(id,i)]), 
            map tyVarType tyvars)))::d)
      )
    | ((_, _), (i, d)) => (i, d)) 
   (i1, d1)
   (Id.fixMap (VEofE E))
in
  d2
end


end (* of local open *)

end (* of struct *)