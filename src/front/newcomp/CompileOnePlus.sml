(*======================================================================*)
(* Special compilation for 1+ types.					*)
(*======================================================================*)
structure CompileOnePlus :> COMPILEONEPLUS =
struct

local 
  open CompileEnv TyRep CompileFixedOps CompileOps
  open RTInstrs
  infixr 5 ++ +$ $+
in

val showNones = Controls.add false "showNones"

structure Map =   
  MapFn(
  struct 
    type ord_key = MILTy.Type 
    val compare = MILTyRep.compare Var.Map.empty
  end); 




val nones =
  ref (Map.empty : (int * MILTy.Type *  VMTy.Type * VMTy.Type list) Map.map)

val count = 
  ref 0


(*@TODO: currently no 1+ types actually require dummy nones, 
         all use null to represent inl <>
*)
(*@TODO: revise *)

(* use this for debugging to detect loops 
local fun strip c hyp ty = 
   (
    if Controls.get showClassGen then
	  PrintManager.print 
          ("(strip "^(Int.toString c)^")")
    else ();
    case MILTy.proj ty of 
	MILTy.Mu a => 
	    strip (c+1) (ty::hyp) (MILTy.unfold a)
      | t as (MILTy.Sum [[],[ty']]| MILTy.Sum [[ty'],[]]) =>
	    if List.exists (fn hty => MILTy.eq(hty,ty')) hyp then
		 t
	    else strip (c+1) (ty::hyp) ty'
      | t  => t)
in val strip = fn ty => strip 0 [] ty
end
*)

local fun strip hyp ty = 
   case MILTy.proj ty of 
	MILTy.Mu a => 
	    strip (ty::hyp) (MILTy.unfold a)
      | t as MILTy.Sum [[],[ty']] =>
	    if List.exists (fn hty => MILTy.eq(hty,ty')) hyp 
	    then t
	    else strip (ty::hyp) ty'
      | t as MILTy.Sum [[ty'],[]] =>
	    if List.exists (fn hty => MILTy.eq(hty,ty')) hyp 
	    then t
	    else strip (ty::hyp) ty'

      | t  => t
in val strip = fn ty => strip [] ty
end

(*@TODO: revise *)
fun lookupNone env (ty,repty,rep) =
  case Map.find(!nones, ty) of
    NONE =>
    let
      val i = !count
      val argreps =        
        case strip ty of
	  MILTy.Sum ([[], [ty]]) => []
        | MILTy.Sum ([[ty], []]) => []
        | MILTy.Con [] => [RTOps.int]
	| MILTy.Sum _ => [RTOps.int]
        | MILTy.Con tys =>
          map (tyToRep env) tys
	| MILTy.Prod tys =>
          map (tyToRep env) tys

        | MILTy.Refty (tys,refkind)  =>
	  (case MILTys.fromRefKind refkind of
	       SOME MILTys.Heap => map (tyToRep env) tys
	     | SOME MILTys.Address =>
		   (* produces unverifiable code *)
		   [VMTy.Address (tyToRep env (hd tys))]
	     | SOME (MILTys.Field(classtn,field)) => 
		   if TyName.isClass classtn
		   then [tyToRep env (MILTy.tyname classtn)]
		   else	(* produces unverifiable code *) 
		        [VMTy.Address (tyToRep env (MILTy.tyname classtn))]
	     | SOME (MILTys.Static(classtn,field)) => 
			[RTOps.int]
	     | NONE => (* should be unreachable *)
			[RTOps.object])
	| (tyc as MILTy.Tyname tn)=> 
	       if TyName.eq(tn,TyNames.objectTyName)
		   then []
	       else if TyName.eq(tn,TyNames.stringTyName)
		   then []
	       else if MILTyRep.isUninstantiableClass (MILTy.inj tyc) 
		   then [tyToRep env (MILTy.inj tyc)]
	       else [tyToRep env (MILTy.inj tyc)] (*???*)
        | _ => []     
    in
      count := i + 1;
      nones := Map.insert(!nones, ty, (i,repty,rep,argreps)); i
    end
  | SOME(i,_,_,_) => i

(*----------------------------------------------------------------------*)
(* Given a type ty, return the instructions and value necessary to	*)
(* generate inl <> : 1+ty                                               *)
(*----------------------------------------------------------------------*)
fun none env ty = 
let
  val ty = MILTy.forceBounds (#kindenv env) ty
in
  if MILTyRep.noneIsNull (#kindenv env) ty
  then ($ ldnull, 1)
  else
    let
      val repty = 
	if MILTyRep.someIsNop (#kindenv env) ty 
        then ty
        else MILTy.prod [ty]
      val rep = tyToRep env repty 
      val i = lookupNone env (ty,repty,rep)
    in
      ($(ldsfld 
       { name = RepNames.noneVal i, 
         classty = globs (),
         fldty = rep }), 1)
    end
end

(*----------------------------------------------------------------------*)
(* Given a type ty and instrs for v, return the instructions and value	*)
(* necessary to generate inr <v> : 1+ty                                 *)
(*----------------------------------------------------------------------*)

fun some env (vinstrs,stack) ty =
let
  val ty = MILTy.forceBounds (#kindenv env) ty
in
  if MILTyRep.someIsNop (#kindenv env) ty
  then (vinstrs, stack)
  else 
  let
    val rep = tyToRep env (MILTy.prod [ty])
    val argrep = tyToRep env ty
  in
    StaticNewObj.init (vinstrs, stack) { classty = rep, argtys = [argrep] }
  end
end

(*----------------------------------------------------------------------*)
(* Given a type ty and a value for inr <v>, return the instructions and *)
(* value corresponding to v that are required for the projection.       *)
(*----------------------------------------------------------------------*)
fun proj env ty =
let
  val ty = MILTy.forceBounds (#kindenv env) ty
in
  if MILTyRep.someIsNop (#kindenv env) ty
  then (Empty, 1)
  else
  let
    val rep = tyToRep env (MILTy.prod [ty])
    val fldrep = tyToRep env ty
  in
    ($(ldfld { name = RepNames.argLabel 0, fldty = fldrep, classty = rep }), 1)
  end
end

(*......................................................................*)
(* Null value of given type					1/7/98	*)
(*......................................................................*)
local
  val basemap =
  foldl (fn ((tn,s),m) => TyName.Map.insert(m,tn,s)) TyName.Map.empty
  [(TyNames.boolTyName, Constants.BOOLEAN (RTInt.fromInt 0)),
   (TyNames.int32TyName, Constants.INT (RTInt.fromInt 0)),
   (TyNames.int8TyName, Constants.BYTE (RTInt.fromInt 0)),
   (TyNames.int16TyName, Constants.SHORT (RTInt.fromInt 0)),
   (TyNames.int64TyName, Constants.LONG (RTLong.fromInt 0)),
   (TyNames.charTyName, Constants.CHAR (RTInt.fromInt 0)),
   (TyNames.word32TyName, Constants.INT (RTInt.fromInt 0)),
   (TyNames.word8TyName, Constants.BYTE (RTInt.fromInt 0)),
   (TyNames.word64TyName, Constants.LONG (RTLong.fromInt 0)),
   (TyNames.real64TyName, Constants.DOUBLE (RTDouble.fromReal 0.0)),
   (TyNames.real32TyName, Constants.FLOAT (RTFloat.fromReal 0.0))]
in
fun nullFor rep =
case rep of
  VMTy.ValueClass b =>
  ldc (valOf (TyName.Map.find(basemap, b)))

| _ =>
  ldnull
end

(*----------------------------------------------------------------------*)
(* Generate the instructions necessary to create a single dummy none.	*)
(*----------------------------------------------------------------------*)
fun makeNoneInstrs (i,_, rep, argreps) = 
  let fun default () = 
      let
	  val arginstrs = map nullFor argreps
      in
	  init (fromList arginstrs, length argreps) { classty = rep, argtys = argreps }
      end
  in
  case rep of
    VMTy.Array ty =>
    (fromList [ldc (Constants.INT (RTInt.fromInt 0)), newarr ty], 1)
  | VMTy.Class tn =>
    if TyName.eq(tn,TyNames.stringTyName)
	then (fromList [ldc (Constants.STRING (UString.fromString "")), 
	       call {name=Id.fromString "Copy",classty=RTOps.string,argtys=[RTOps.string],resty=SOME RTOps.string}],
	      1)
    else default()
  | _ => default()
  end
		 

(*----------------------------------------------------------------------*)
(* Generate the instructions necessary to create all dummy none values.	*)
(*----------------------------------------------------------------------*)
fun makeNones () =
let
  fun loop ([], result) = result
    | loop ((x as (i,_, rep, _))::xs, (instrs, stack)) =
      let
        val (instrs', stack') = makeNoneInstrs x
      in
        loop (xs, (instrs ++ instrs' +$ stsfld { name = RepNames.noneVal i,
          classty = TyRep.globs (), fldty = rep }, Int.max(stack, stack')))
      end
in
(*
  (if Controls.get showNones then
      Debug.print 
        ("\nNones: \n" ^ 
	 Pretty.simpleVec "\n" 
	 (fn (ty,(i,repty,rep,argreps)) => 
	  MILTy.toString ty ^ " repty: " ^
	  MILTy.toString repty ^ " tag:" ^
	  (RepNames.noneVal i) ^ " rep:" ^
	  (VMTy.toString rep) ^ " argreps: " ^
	  (Pretty.simpleVec "," VMTy.toString argreps))
	 (Map.listItemsi (!nones)))
  else ());
*)
  loop (Map.listItems (!nones), (Empty, 0))
end

(*----------------------------------------------------------------------*)
(* Make a field defn for a single dummy none.                           *)
(*----------------------------------------------------------------------*)
fun makeNoneField (i,_, rep, _) =
      {
        name = RepNames.noneVal i, 
        flags = assemblystatic,
        ty = rep,
        value = NONE
      }

(*----------------------------------------------------------------------*)
(* List the global fields used for all dummy none values.		*)
(*----------------------------------------------------------------------*)
fun makeNoneFields () = 
  map makeNoneField (Map.listItems (!nones))

fun init () = (nones := Map.empty; count := 0)
      
end (* of local open *)

end (* of struct *)





