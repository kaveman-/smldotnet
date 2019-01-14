(*======================================================================*)
(* Primitive structures (at present, just Prim)                         *)
(*======================================================================*)
structure SepPrim :> SEPPRIM =
struct

local 
  open MILTerm TransOps Effect 
in

(*----------------------------------------------------------------------*)
(* Make a typed abstractions for a particular type.                     *)
(*                                                                      *)
(* The function f takes lists of value arguments and result types and   *)
(* returns a computation term.                                          *) 
(*----------------------------------------------------------------------*)
fun makeFun f milty =
let
  val SOME ([argty], cty) = MILTy.fromArrow milty
  val (_, restys as [resty]) = MILTy.fromCmp cty
  val restys = 
    case MILTy.fromProd resty of
      SOME [] => []
    | _ => restys

  val (x,xv) = freshAnonVar ()
  val term = 
    case MILTy.fromProd argty of
      SOME comptys =>
      let
        val n = length comptys
        fun makeTerm (0, args) = 
            f (args, restys)

          | makeTerm (i, args) = 
            let
              val (x',xv') = freshAnonVar ()
            in
              LetVal(x', Proj(i-1, n, xv), makeTerm(i-1, xv' :: args))
            end
      in
        makeTerm (n, [])
      end

    | NONE => 
      f ([xv], restys)

  val term = 
    case MILTy.fromProd resty of
      SOME [] => Let(term, ([], Triv [Tuple []])) 
    | _ => term
in
  ([(x, argty)], term)
end

fun makeSpecialFun (j, eff) = 
  makeFun (fn (vs, restys) => Special((j, NONE, NONE), vs, MILTy.cmp(eff,restys)))

fun makeSpecialPrim p = 
    let
      val optype = Ext.Prim (Id.fromString p)
    in
       makeSpecialFun (optype, ExtOps.effectOf optype)
    end
  

val RTOps = 
  map (fn x =>(x,makeSpecialPrim x))
[ "gt", "lt", "ge", "le", "eq", "ne", "gt_un", "lt_un",
  "add", "And", "arraylength", "arrayload", "arraystore", "arraycopy",
  "cmp", "cmpl", "cmpg", "div", "mul", "neg", "newarray", "or", "rem",
  "shl", "shr", "sub", "ushr", "xor", "getLocMessage", "=", "ref", ":=", "!",
  (* CLR only *)
  "div_un","rem_un",
  "add_ovf","mul_ovf","sub_ovf",
  "add_ovf_un","mul_ovf_un","sub_ovf_un"
  ]

fun purify milty =
case MILTy.fromArrow milty of
  SOME (argtys, cty) =>
  let
    val (eff,restys) = MILTy.fromCmp cty
  in
    MILTy.arrow(argtys, MILTy.cmp(Effect.none, restys))
  end
| _ => milty

val coercions = 
  map (fn x => (x, makeFun (fn ([a], [ty]) => 
    Triv [As(a, purify ty)])))
[
  "toVector", "fromVector", "unsafeValOf", 
  "fromWord", "toWord",
  "fromWord64", "toWord64",   
  "fromWord16", "toWord16",
  "fromWord8", "toWord8",
  "fromChar", "toChar",
  "fromInt8", "toInt8",
  "fromInt16", "toInt16",
  "purify"
]

(* CLR only *)

(* signed conversions without overflow *)

val conv = map (fn x => (x, makeSpecialPrim "conv")) [
   "I42I1", "I42U1", "I42I2", "I42U2", "I42I4", "I42U4", "I42I8", "I42U8", "I42I", "I42U", "I42R4", "I42R8", 
   "I82I1", "I82U1", "I82I2", "I82U2", "I82I4", "I82U4", "I82I8", "I82U8", "I82I", "I82U", "I82R4", "I82R8", 
   "I2I1", "I2U1", "I2I2", "I2U2", "I2I4", "I2U4", "I2I8", "I2U8", "I2I", "I2U", "I2R4", "I2R8", 
   "R42R8", "R82R4" (* Use +ve and -ve instead of overflowing *)]


(* signed conversions with overflow *)
 
val conv_ovf = map (fn x => (x, makeSpecialPrim "conv_ovf")) [
   "I42I1_ovf", "I42U1_ovf", "I42I2_ovf", "I42U2_ovf", "I42I4_ovf", "I42U4_ovf", "I42I8_ovf", "I42U8_ovf", "I42I_ovf", "I42U_ovf", "I42R4_ovf", "I42R8_ovf", "I42R_ovf", 
   "I82I1_ovf", "I82U1_ovf", "I82I2_ovf", "I82U2_ovf", "I82I4_ovf", "I82U4_ovf", "I82I8_ovf", "I82U8_ovf", "I82I_ovf", "I82U_ovf", "I82R4_ovf", "I82R8_ovf", "I82R_ovf", 
   "I2I1_ovf", "I2U1_ovf", "I2I2_ovf", "I2U2_ovf", "I2I4_ovf", "I2U4_ovf", "I2I8_ovf", "I2U8_ovf", "I2I_ovf", "I2U_ovf", "I2R4_ovf", "I2R8_ovf", "I2R_ovf",
   "R42I4_ovf", "R82I4_ovf", "R42I8_ovf", "R82I8_ovf"]
 
(* unsigned conversions with overflow *)
 
val conv_ovf_un = map (fn x => (x, makeSpecialPrim "conv_ovf_un")) [
   "I42I1_ovf_un", "I42U1_ovf_un", "I42I2_ovf_un", "I42U2_ovf_un", "I42I4_ovf_un", "I42U4_ovf_un", "I42I8_ovf_un", "I42U8_ovf_un", "I42I_ovf_un", "I42U_ovf_un", "I42R4_ovf_un", "I42R8_ovf_un", "I42R_ovf_un", 
   "I82I1_ovf_un", "I82U1_ovf_un", "I82I2_ovf_un", "I82U2_ovf_un", "I82I4_ovf_un", "I82U4_ovf_un", "I82I8_ovf_un", "I82U8_ovf_un", "I82I_ovf_un", "I82U_ovf_un", "I82R4_ovf_un", "I82R8_ovf_un", "I82R_ovf_un", 
   "I2I1_ovf_un", "I2U1_ovf_un", "I2I2_ovf_un", "I2U2_ovf_un", "I2I4_ovf_un", "I2U4_ovf_un", "I2I8_ovf_un", "I2U8_ovf_un", "I2I_ovf_un", "I2U_ovf_un", "I2R4_ovf_un", "I2R8_ovf_un", "I2R_ovf_un"
]

(* CLR stuff *)
val addressOps = 
let val & = Ext.Address 0  
in
  [
   ("&", makeSpecialFun(&,ExtOps.effectOf &)) (* take the address of a ref (any ref) *)
   ]
end

val allOps = 
  foldl 
  (fn ((x, v), m) => Symbol.Map.insert(m, Id.fromString x, v))
  Symbol.Map.empty
  (addressOps @ RTOps @ coercions @
   conv @ conv_ovf @ conv_ovf_un)

fun makePrimTerm (TNE,E) = 
let
  val VE = EnvOps.VEofE E

  fun lookup (id, TVE) smlty = 
  let
    val milty = TransType.transType TVE TNE smlty
    val makeTAbs =   
      case Symbol.Map.find(allOps, id) of
        NONE => 
        Debug.fail ("SepPrim.makePrimTerm: no such operation: " ^ 
          Id.toString id)

      | SOME a => a

    val tabs = makeTAbs milty
    val (f,fv) = freshAnonVar ()
  in
    (f, tabs)
  end
 
  fun trans ([], args) = 
      Triv [Tuple (rev args)]

    | trans ((id, ValBind.VarSch sch)::rest, args) = 
      (case sch of
        SMLSch.TypeScheme([], smlty) =>
        let
          val (f, tabs) = lookup (id, TyVar.Map.empty) smlty
        in
          LetFun([], AnyFun, Fun(f, tabs), trans (rest, Var (#1 f) :: args))
        end                  

      | SMLSch.TypeScheme(smltyvars, smlty) =>
        case map TyVar.sort smltyvars of
          [TyVar.Overloaded tynames] =>
          let
            val tyvar = hd smltyvars
            val smltys = map (fn tyname => 
              (SMLTy.appSubst [(tyvar, SMLTy.consType([], tyname))] smlty))
              (TyName.Set.listItems tynames)

            val pairs = map (lookup (id, TyVar.Map.empty)) smltys
            val (p,pv) = freshAnonVar ()

            fun trans' ([], args') =
                LetVal(p, Tuple (rev args'), trans (rest, pv :: args))

              | trans' ((f, tabs)::rest', args') =
                LetFun([], AnyFun, Fun(f, tabs), trans'(rest', Var (#1 f) :: args'))
                
          in
            trans' (pairs, [])
          end

        | _ =>
          let
            val (TVE,tyvars) = TransOps.freshTyVars (TyVar.Map.empty,smltyvars)
            val (f, tabs) = lookup (id, TVE) smlty
          in
            LetFun(tyvars, AnyFun, Fun(f, tabs), trans (rest, Var (#1 f) :: args))
          end
      )

    | trans (_::rest, args) = trans(rest, args)
in
  trans (Id.fixMap VE, [])
end

fun makePrimEntry (TNE,primE) = 
let
  val _ = TransOps.initialize Var.initial
  val ty = TransType.transE TNE primE
  val term = makePrimTerm (TNE,primE)
in
  UnitTypes.Str
  {
    E = primE,

    (* The MIL variable supply *)
    supply = TransOps.getSupply (),

    tyvarsupply = TransOps.getSupply (),

    (* The MIL term *)
    term = ([],term),

    (* MIL type defs for this module *)
    tynameTys = TyName.Map.empty
  } 
end

end (* of local open *)

end