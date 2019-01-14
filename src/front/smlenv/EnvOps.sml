(*======================================================================*)
(* Operations on environments used in type inference			*)
(*======================================================================*)
structure EnvOps :> ENVOPS =
struct

val showTyNames = Controls.add false "env.showTyNames"
val showExpandedEnvs = Controls.add false "env.showExpandedEnvs"

local open SMLTy Env ValBind in

structure Map = Symbol.Map

fun freeVE VE = 
  Map.foldr (fn (b, tyvars) => 
    TyVar.Set.union (ValBindOps.tyvars b, tyvars)) TyVar.Set.empty VE

fun merge (m1, m2) = 
  Map.foldri (fn (x,v,m) => Map.insert (m,x,v)) m1 m2

fun renameVE r = Map.map (ValBindOps.rename r)

fun renameTE r = Map.map (TyStr.rename r)

fun renameE r (Env (SE,TE,VE)) =
 Env (Map.map (renameE r) SE, renameTE r TE, renameVE r VE)

fun appRealisationVE psi = Map.map (ValBindOps.appRealisation psi)

fun appRealisationTE psi = Map.map (TyStr.appRealisation psi)

fun appRealisationE psi (Env (SE,TE,VE)) =
 Env (Map.map (appRealisationE psi) SE, appRealisationTE psi TE, 
   appRealisationVE psi VE)

(*----------------------------------------------------------------------*)
(* Type names in environments						*)
(*----------------------------------------------------------------------*)
fun tynamesVE VE = 
  Map.foldr (fn (vb,T) => TyName.Set.union(ValBindOps.tynames vb, T)) 
  TyName.Set.empty VE
fun tynamesTE TE =
  Map.foldr (fn (tystr,T) => TyName.Set.union(TyStr.tynames tystr, T))
  TyName.Set.empty TE
fun tynamesSE SE =
  Map.foldr (fn (E,T) => TyName.Set.union(tynamesE E, T))
  TyName.Set.empty SE
and tynamesE (Env(SE,TE,VE)) = 
  TyName.Set.union(tynamesSE SE, TyName.Set.union(tynamesTE TE, tynamesVE VE))

(*----------------------------------------------------------------------*)
(* Selection of environment components					*)
(*----------------------------------------------------------------------*)
fun VEofE (Env(SE,TE,VE)) = VE
fun TEofE (Env(SE,TE,VE)) = TE
fun SEofE (Env(SE,TE,VE)) = SE
fun GofB (T,F,G,E,path) = G
fun EofB (T,F,G,E,path) = E
fun TofB (T,F,G,E,path) = T
fun FofB (T,F,G,E,path) = F
fun pathofB (T,F,G,E,path) = path
fun pathofC (T,U,E,lam,path,class) = path
fun lamofC (T,U,E,lam,path,class) = lam
fun classofC (T,U,E,lam,path,class) = class
fun CofB B = (TofB B, TyVar.Set.empty, EofB B, false, pathofB B, NONE)
fun EofC (T,U,E,lam,path,class) = E
fun UofC (T,U,E,lam,path,class) = U

fun tyconsE E =
let
  fun tyconsE' longid (Env(SE,TE,VE)) =
  map (fn (id,_) => longid @ [id]) (Map.listItemsi TE) @
  List.concat 
  (map (fn (id,E) => tyconsE' (longid @ [id]) E) (Map.listItemsi SE))
in
  tyconsE' [] E
end

(*----------------------------------------------------------------------*)
(* Extension of environment components					*)
(*----------------------------------------------------------------------*)
fun EplusSE (Env(SE,TE,VE)) SE' = Env(merge(SE,SE'),TE,VE)
fun EplusTE (Env(SE,TE,VE)) TE' = Env(SE,merge(TE,TE'),VE)
fun EplusVE (Env(SE,TE,VE)) VE' = Env(SE,TE,merge(VE,VE'))
fun VEplusVE VE1 VE2 = merge (VE1,VE2)
fun TEplusTE TE1 TE2 = merge (TE1,TE2)
fun SEplusSE SE1 SE2 = merge (SE1,SE2)
fun EplusE (Env(SE1,TE1,VE1)) (Env(SE2,TE2,VE2)) =
  Env(merge(SE1,SE2), merge(TE1,TE2), merge(VE1,VE2))
fun BplusE (T,F,G,E,path) E' = (T,F,G,EplusE E E',path)
fun BplusG (T,F,G,E,path) G' = (T,F,merge(G,G'),E,path)
fun BplusF (T,F,G,E,path) F' = (T,merge(F,F'),G,E,path)
fun CplusE (T,U,E,lam,path,class) E' = (T,U,EplusE E E',lam,path,class)
fun CplusVE (T,U,E,lam,path,class) VE = (T,U,EplusVE E VE,lam,path,class)
fun CplusTE (T,U,E,lam,path,class) TE = (T,U,EplusTE E TE,lam,path,class)
fun CplusU (T,U,E,lam,path,class) U' = 
  (T,TyVar.Set.union(U,U'),E,lam,path,class)
fun lamC (T,U,E,lam,path,class) = (T,U,E,true,path,class)
fun CwithClass (T,U,E,lam,path,_) classOpt = (T,U,E,lam,path,classOpt)
fun BplusStr (T,F,G,E,path) strid = (T,F,G,E,path @ [strid])
fun BnoPath (T,F,G,E,path) = (T,F,G,E,[])

(*----------------------------------------------------------------------*)
(* Inclusion of environment components					*)
(*----------------------------------------------------------------------*)
fun SEinE SE = Env(SE, Map.empty, Map.empty)
fun TEinE TE = Env(Map.empty, TE, Map.empty)
fun VEinE VE = Env(Map.empty, Map.empty, VE)
fun VETEinE (VE,TE) = Env(Map.empty, TE, VE)
fun EinB E = (TyName.Set.empty, Map.empty, Map.empty, E, [])

(*----------------------------------------------------------------------*)
(* Empty environments							*)
(*----------------------------------------------------------------------*)
val emptyE = Env(Map.empty, Map.empty, Map.empty)
val emptyB = (TyName.Set.empty, Map.empty, Map.empty, emptyE, [])

(*----------------------------------------------------------------------*)
(* Pretty-printing							*)
(*----------------------------------------------------------------------*)
fun VEtoString depth VE =
  Pretty.bigVec depth 
  (fn (id, vbind) => Id.toString id ^ " : " ^ ValBindOps.toString vbind)
  (Map.listItemsi VE)

and TEtoString depth TE =
  Pretty.bigVec depth
  (fn (id, tystr) => Id.toString id ^ " = " ^ TyStr.toString tystr)
  (Map.listItemsi TE)

and SEtoString depth SE =
  Pretty.bigVec depth
  (fn (id, E) => Id.toString id ^ " = " ^ EtoString (depth+1) E)
  (Map.listItemsi SE)

and EtoString depth (Env(SE,TE,VE)) =
  Pretty.newline depth ^ "(" ^ Pretty.newline (depth+1) ^
  "SE = " ^ SEtoString (depth+1) SE ^ "," ^ Pretty.newline (depth+1) ^
  "TE = " ^ TEtoString (depth+1) TE ^ "," ^ Pretty.newline (depth+1) ^
  "VE = " ^ VEtoString (depth+1) VE ^ ")" ^ Pretty.newline (depth)

val EtoString = EtoString 0

fun tnFun tnmap tyname = 
  getOpt(TyName.Map.find (tnmap,tyname), TyNames.toString tyname)

fun VEasVals (depth,tnmap) VE =
  String.concat (map
  (fn (id, vbind) => "val " ^ 
    Id.toString id ^ " : " ^ ValBindOps.toStringWith (depth+1,tnFun tnmap) vbind
    ^ Pretty.newline depth) (Map.listItemsi VE))
and TEasTypes (depth,tnmap,expand) TE =
  String.concat (map 
  (fn (id, tystr) => 
  case TyStr.fromDatatype tystr of
    SOME (tyvars, tyname, contys) =>
    "datatype " ^ SMLTy.tyvarsToString tyvars ^ Id.toString id ^ 
    (if Controls.get showTyNames then "(is " ^ TyNames.toString tyname ^ ")" else "") ^ " = " ^ 
    Pretty.newline depth ^ "  " ^ Pretty.simpleVec "| "
    (fn (id, tyopt) => Id.toString id ^ 
      (case tyopt of NONE => "" | SOME ty =>
      " of " ^ SMLTy.toStringWith (tnFun tnmap) ty) ^ Pretty.newline depth) 
      (Map.listItemsi contys)
  | NONE =>
    case TyStr.fromClasstype tystr of
      SOME c =>
      "_classtype " ^ 
      TyStr.toStringWith (depth,(tnFun tnmap)) tystr ^ Pretty.newline depth
    | NONE =>
      case TyStr.fromAbstract tystr of
        SOME (tyvars, tyname) =>
        (if TyName.equality tyname = TyName.Eq
        then "eqtype " 
        else if TyName.isClass tyname
        then "_classtype " else "type ") ^ 
        SMLTy.tyvarsToString tyvars ^ Id.toString id ^ 
        (if Controls.get showTyNames then "(is " ^ TyNames.toString tyname ^ ")" else "") ^ Pretty.newline depth
      | NONE =>
        case TyStr.fromConcrete tystr of
          SOME (tyvars, ty) =>
          "type " ^ SMLTy.tyvarsToString tyvars ^ Id.toString id 
          ^ " = " ^ 
	  (case (expand,fromExternalClass ty) of 
	     (true,SOME tyname) => classAsSig (depth,tnmap) (ty,tyname)
	   | _ => SMLTy.toStringWith (tnFun tnmap) ty)
	  ^ Pretty.newline depth
       | NONE => "")
  (Map.listItemsi TE))

and fromExternalClass ty  = case SMLTy.fromConsType ty of
  SOME ([], tyname) => 
  if TyName.isExternal tyname then SOME tyname else NONE
| _ => NONE

(*@TODO: part of this this should be in TransInterop *)
and classAsSig (depth,tnmap) (ty,tyname)= 
	     (case TransInter.lookupExtClass (TyName.longid tyname) of 
		  NONE => ""
		| SOME{longid,flags,super,interfaces,fields,methods} =>
		      (* dressing up mutton as lamb ;-> *)
		      let val mlfields = 
			  List.foldr (fn(fieldinfo as{name,flags,...},fields)=>
				      if Symbol.Set.member(flags,Id.staticSym) 
					  then fields
				      else {name = name,flags = flags,ty = #1 (InterOp.fieldTy ty (fieldinfo))}::fields)
			  []
			  (if isSome (InterOp.isEnumType ty) then [] else fields)
			  val mlmethods = 
			      List.foldr (fn(methodinfo as {name,flags,...},methods)=>
					  if Symbol.Set.member(flags,Id.staticSym)  
					     orelse Symbol.equal(name, Id.fromString "<init>")
					     then methods 
					  else {name=name,flags=flags,ty=InterOp.methTy (methodinfo)}::methods)
			      mlfields 
			      methods					   
			  val fakeTyStr = TyStr.makeClasstype
			      (SMLTy.MLClass({tyname=tyname,flags=flags,
					      super= case super of 
					      NONE => SMLTy.baseType(TyNames.objectTyName) 
					    | SOME ty => ty,
						  interfaces=interfaces,
						  initargty=NONE,
						  methods=mlmethods}))
		      in
			  TyStr.toStringWith (depth,(tnFun tnmap))  fakeTyStr
		      end)
  
fun mapToString(depth,tnmap,desc,sep,tyToString,env) =
  String.concat (List.foldr 
		 (fn ((id, ty),lines) => 
		  (desc ^ Id.toString id ^ sep ^ tyToString (depth+1,tnmap) ty) ^ 
		   Pretty.newline depth 
		   :: lines)
		[]
	        (Map.listItemsi env))

fun dots(expand,f) = if expand orelse Controls.get showExpandedEnvs then f else  fn _ => fn _ => " ... " 

fun SEasSig (depth,tnmap,expand) SE =
  mapToString(depth,tnmap,"structure "," : ",dots(expand,EasSig),SE)
and FasSig (depth,tnmap,expand) F =
  mapToString(depth,tnmap,"functor "," : ",dots(expand,phiAsFunSig),F)
and GasSig (depth,tnmap,expand) G =
  mapToString(depth,tnmap,"signature "," = ",dots(expand,sigmaAsSig),G)
and EasSig (depth,tnmap) (Env(SE,TE,VE)) =
 (* Pretty.newline depth ^  *)
  "sig" ^ 
  (if Map.isEmpty SE andalso Map.isEmpty TE andalso Map.isEmpty VE 
       then " end"
   else
       Pretty.newline (depth+1) ^
       SEasSig (depth+1,tnmap,false) SE ^ 
       TEasTypes (depth+1,tnmap,false) TE ^
       VEasVals (depth+1,tnmap) VE ^ 
       Pretty.newline (depth) ^ "end")
       
and sigmaAsSig (depth,tnmap) ((T,E):Env.Sig) = EasSig (depth,tnmap) E
and phiAsFunSig (depth,tnmap) (((T,(E,G)),_,_):Env.FunInfo) = 
    "functor(_:" ^ EasSig (depth,tnmap) E ^")" ^
    sigmaAsSig (depth,tnmap) G
and BasSig (depth,tnmap) ((T,F,G,Env(SE,TE,VE),_):Env.Basis)=
  Pretty.newline (depth) ^
  TEasTypes (depth,tnmap,true) TE ^
  VEasVals (depth,tnmap) VE ^
  SEasSig (depth,tnmap,true) SE ^ 
  FasSig (depth,tnmap,true) F ^ 
  GasSig (depth,tnmap,true) G  
    
val EasSig = EasSig (0,TyName.Map.empty)
val SigmaAsSig = sigmaAsSig (0,TyName.Map.empty) 
val FunInfoAsFunSig = phiAsFunSig (0,TyName.Map.empty) 
val BasSig = BasSig(0,TyName.Map.empty)
val TEasTypes = TEasTypes (0,TyName.Map.empty,true)

(*----------------------------------------------------------------------*)
(* Equality of environments						*)
(*----------------------------------------------------------------------*)
fun eqVE (VE1, VE2) = 
  Eq.list 
  (fn ((id1,vb1), (id2,vb2)) => Symbol.equal(id1,id2) 
    andalso ValBindOps.eq(vb1,vb2))
  (Map.listItemsi VE1, Map.listItemsi VE2)

and eqTE (TE1, TE2) =
  Eq.list 
  (fn ((id1,tstr1), (id2,tstr2)) => Symbol.equal(id1,id2) 
    andalso TyStr.eq(tstr1,tstr2))
  (Map.listItemsi TE1, Map.listItemsi TE2)

and eqSE (SE1, SE2) =
  Eq.list
  (fn ((id1,E1), (id2,E2)) => Symbol.equal(id1,id2) 
    andalso eq(E1,E2))
  (Map.listItemsi SE1, Map.listItemsi SE2)

and eq (Env(SE1,TE1,VE1), Env(SE2,TE2,VE2)) =
  eqSE (SE1,SE2) andalso eqTE (TE1,TE2) andalso eqVE (VE1,VE2)

local
open Pickle
in

val tyEnvPickler = IdPickle.idMap TyStr.pickler
val valEnvPickler = IdPickle.idMap ValBindOps.pickler
val (envPickler,strEnvPickler) = fix2 (fn (envPickler, strEnvPickler) =>
  (wrap (Env, fn Env x => x)
    (triple (strEnvPickler, tyEnvPickler, valEnvPickler)),
   IdPickle.idMap envPickler))

val tnsPickler = 
  wrap (fn xs => TyName.Set.addList(TyName.Set.empty, xs),
        TyName.Set.listItems) (list TyName.pickler)

val sigPickler = pair (tnsPickler, envPickler)

val funsigPickler = pair (tnsPickler, pair (envPickler, sigPickler))

val funPickler = triple (funsigPickler, EntityOps.fileRefPickler,
  list IdPickle.longid)

val topEnvPickler = 
  triple (IdPickle.idMap funPickler, IdPickle.idMap sigPickler, envPickler)

end

end

end
