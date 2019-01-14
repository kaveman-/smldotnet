(*======================================================================*)
(* Some derived types							*)
(*======================================================================*)
structure MILTys =
struct

val unit     = MILTy.prod []
val bool     = MILTy.mu (0, [(TyNames.boolTyName,MILTy.sum [[],[]])])
val int      = MILTy.tyname TyNames.int32TyName
val char     = MILTy.tyname TyNames.charTyName
val topExn   = MILTy.tyname TyNames.exnTyName
val string   = MILTy.tyname TyNames.stringTyName
val sumTagEnum = MILTy.tyname TyNames.sumTagEnum

datatype RefKind = Heap 
                 | Address 
                 | Static of TyName.TyName * Symbol.symbol
                 | Field of TyName.TyName * Symbol.symbol                 
 
fun fromRefKind ty = 
    case MILTy.fromTyname ty of
      SOME tn =>
	   if TyName.eq(tn,TyNames.heapTyName) 
	   then SOME Heap
	   else if TyName.eq(tn,TyNames.addressTyName) 
		then SOME Address
		else NONE
    | NONE =>
	(case MILTy.fromApp ty of
	     SOME (ty,[class,field]) =>
		 (case MILTy.fromTyname(ty) of
		      SOME tn =>
			  (case (MILTy.fromTyname class,MILTy.fromTyname field) of
			       (SOME classtn,SOME fieldtn) => 
				   (case TyName.longid fieldtn of 
					[sym] => 
					    if TyName.eq(tn,TyNames.fieldTyName)
						then  SOME (Field (classtn,sym))
					    else if TyName.eq(tn,TyNames.staticTyName)
						     then SOME (Static(classtn,sym))
						 else NONE
				      | _ => NONE)
			     | _ => NONE)
		    | NONE => NONE)
           | NONE => NONE)

fun refKind Heap = MILTy.inj(MILTy.Tyname(TyNames.heapTyName))
  | refKind Address = MILTy.inj(MILTy.Tyname(TyNames.addressTyName))
  | refKind (Field(tn,sym)) = 
	 MILTy.app(MILTy.tyname TyNames.fieldTyName,[MILTy.tyname (TyName.external (RuntimeNames.syslib, [sym],0))])
  | refKind (Static(tn,sym)) = 
	 MILTy.app(MILTy.tyname TyNames.staticTyName,[MILTy.tyname (TyName.external (RuntimeNames.syslib, [sym],0))])

fun isTopExn ty =
  case MILTy.fromTyname ty of
    SOME tn =>
    TyName.eq(tn, TyNames.exnTyName)

  | _ => false

fun exnTys exnty =
  case MILTy.fromExn exnty of
    SOME (_,tys) => tys
  | NONE => []

end