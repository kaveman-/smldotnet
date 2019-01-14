(*======================================================================*)
(* Convert MIL types to target reps, generating classes as we go.	*)
(*======================================================================*)
structure TyRep :> TYREP =
struct

val genGenerics =  Controls.add false "codegen.genGenerics"
val showClassGen = Controls.add false "codegen.showClassGen"
val genValueTypes = Controls.add false "codegen.structTuples"

(* Map from equivalence classes of types *)
structure Map =  (*@HACK: MILTy.Map *)
  MapFn(
  struct 
    type ord_key = MILTy.Type 
    val compare = MILTyRep.compare Var.Map.empty
  end);


val showReps = Controls.add false "codegen.showReps"
val showSharing = Controls.add false "codegen.showSharing"

exception NotFound
val reps = ref (Map.empty : VMTy.Type Map.map)

(* Supply of class names for namespaces associated with ML types *)
val classSupply = ref (StringMap.empty : int StringMap.map)

(* Classes already used for ML exceptions; backoff to classSupply if
   we get clashes *)
val exnClassSupply = ref Longid.Set.empty

(* clean up *)

val save = ref (fn x:RTResult.Class => (raise Fail "TyRep.save called too early") : unit)

val exportedClasses = ref (TyName.Set.empty)

fun isExportedClass classty = 
    case MILTy.fromTyname classty of 
	NONE => false
      | SOME tn => TyName.Set.member(!exportedClasses,tn)

val primTypes = TyName.Set.addList(TyName.Set.empty,
  [TyNames.int32TyName, TyNames.stringTyName, TyNames.real32TyName, TyNames.real64TyName,
   TyNames.boolTyName, TyNames.charTyName])

fun isPrimType ty =
  case MILTy.fromTyname ty of
    NONE => false
  | SOME tn => TyName.Set.member(primTypes, tn)

fun start (classnames, savecopy) =
(
  classSupply := StringMap.empty;  
  exnClassSupply := Longid.Set.empty;
  SumRep.init();
  StaticNewObj.start();
  save := savecopy;
  reps := 
    foldl (fn ((tn,(longid,depth)),m) => 
	   Map.insert(m, 
		      MILTy.tyname tn, 
		      (* produces "nested" classes in nested classes *)
                      VMTy.Class (TyName.external (Id.fromString "", longid, depth))))
          Map.empty classnames;
  exportedClasses := foldl (fn ((tn,_),s) => TyName.Set.add(s,tn)) TyName.Set.empty classnames
)

fun internalClass (valueType, name) =
let  
  val longid = [RepNames.internalNamespace, name]
in
  (if valueType then VMTy.ValueClass else VMTy.Class) (TyName.external (Id.fromString "", longid,0))
end

local
  val c = ref 0
  fun tyToRep' ty =  
  let
    val tr = tyToRep'
    fun add rep = (reps := Map.insert(!reps, ty, rep); rep)

    fun new (valueType, prefix) =
    let
      val n = case StringMap.find(!classSupply, prefix) of
        NONE => 
        (classSupply := StringMap.insert(!classSupply, prefix, 1);
         0)
      | SOME n => 
        (classSupply := StringMap.insert(!classSupply, prefix, n+1);
         n)
      val rep = internalClass (valueType, Id.fromString (prefix ^ "_" ^ Pretty.indexToString n))
    in
      if Controls.get showClassGen then
	  PrintManager.print 
          ("(class "^(Int.toString (!c))^":"^(VMTy.toString rep)^")")
      else ();
      add rep
    end
  in  
  case Map.find(!reps, ty) of
    SOME rep => rep
  | NONE =>
    case MILTy.proj ty of
      MILTy.Forall (a as (tyvars,_)) => 
      let
        val tys = map (fn MILTy.Bound ty => ty) tyvars
      in
        add (tr (MILTy.app (MILTy.abs a, tys)))
      end

    | MILTy.Tyname tn =>
      if TyName.isExternal tn
      then
        if TyName.isClass tn
        then add (VMTy.Class tn)
        else add (VMTy.ValueClass tn)
      else new (false, RepNames.classPrefix)

    | MILTy.Sum ([[], [ty]]) =>
      if MILTyRep.someIsNop Var.Map.empty ty
      then add (tr ty)
      else (*@BUG: add? *) tr (MILTy.prod [ty])
    | MILTy.Sum ([[ty], []]) =>
      if MILTyRep.someIsNop Var.Map.empty ty
      then add (tr ty)
      else (*@BUG: add? *) tr (MILTy.prod [ty])

(*@NOTE: bools should be represented as booleans *)
    | MILTy.Sum ([[],[]]) =>
      add (VMTy.ValueClass TyNames.boolTyName)

    | MILTy.Sum tyss =>
      if List.all List.null tyss
      then add (VMTy.ValueClass TyNames.int32TyName)
      else tr (MILTy.con [])

    | MILTy.Prod [] =>  
      add RTOps.object

    | MILTy.Array ty =>
      add (VMTy.Array (tr ty))
    | MILTy.Vector ty =>
      add (VMTy.Array (tr ty))

    | MILTy.Mu (a as (_,defs)) =>
      if 
	 (*@BUG: CRUSSO the test for not(Controls.get genGenerics) is suspect,
	    but omitting it triggers the else branch which somehow
            generates too many representation classes, most likely by 
            failing to identify the representation of a mu type with the 
            representation of its unfolding. The bug is subtle
	    because the .il still runs, but fails to peverify.
            To repro, comment out the genGenerics test to trigger
	    the then branch and compile and verify an example with 
	    recursive datatypes.
         *)
	 not (Controls.get genGenerics) orelse    
         List.all (fn (_,ty) => IntSet.isEmpty (MILTy.dtyvars ty)) defs
      then 
            (add (tr (MILTy.unfold a)))
      else
      let (*@BUG: CRUSSO this doesn't work, but is never exercised on non-generic runtime *)
        val rep = add (new (false, RepNames.recPrefix))
        val ua = MILTy.unfold a
        val unfoldrep = (tr (MILTy.unfold a))
      in
        (!save) (CompileProdSum.makeProdClass (rep, [unfoldrep]));
        rep
      end

    | MILTy.Prod tys =>
      let
        val valueType = Controls.get genValueTypes andalso 
                        length tys >= 2 andalso length tys <= 3 andalso List.all isPrimType tys
        val rep = new (valueType, RepNames.tuplePrefix ^ Int.toString (length tys))
        val reps = map tr tys
      in
        (!save) (CompileProdSum.makeProdClass (rep,reps));
        rep
      end
    | MILTy.Refty (tys,refkind) =>
      (case MILTys.fromRefKind refkind of
	   SOME (MILTys.Heap) =>
	      (* continue as in MILTy.Prod tys *)
		   let
		       val rep = new (false, RepNames.tuplePrefix ^ Int.toString (length tys))
		       val reps = map tr tys
		   in
		       (!save) (CompileProdSum.makeProdClass (rep,reps));
		       rep
		   end
	 | SOME MILTys.Address => 
		   (*@HACK: assumes an address ref won't be flattened*)
		   add (VMTy.Address (if null tys 
				      then RTOps.object
			              else (tr (hd tys))))
	 | SOME (MILTys.Field(classtn,field)) =>
	      let val classrep = tr (MILTy.tyname classtn)
	      in
		  if TyName.isClass classtn
		  then add classrep
		  else add (VMTy.Address(classrep)) 
	      end
         | SOME (MILTys.Static(classtn,field)) =>
	      (*HACK: assumes a static ref won't be flattened*)
	      add (RTOps.int)
         | NONE => (RTOps.object))
    | MILTy.Con [] =>
      let
        val rep = internalClass (false, RepNames.dataClass)
      in
        add rep;
        (!save) (CompileProdSum.makeTopConClass (rep,SumRep.tagRep()));
        rep
      end
      
    | MILTy.Con tys =>
      let
        val rep = new (false, RepNames.conPrefix ^ Int.toString (length tys))
        val reps = map tr tys
        val superrep = tr (MILTy.con [])
      in
        (!save) (CompileProdSum.makeConClass (superrep,rep,reps,SumRep.tagRep()));
        rep
      end

    | MILTy.Exn (exn, tys) =>   
      (* External exceptions are treated like external classes *)   
      if TyName.isExternal (Exn.name exn)
      then add (VMTy.Class (Exn.name exn))
      else
  
      (* We attempt to map ML exceptions to a class with the same 
         name except where there are clashes e.g. due to rebinding *)
      let
        val longid = TyName.longid (Exn.name exn)
        val rep = 
          if Longid.Set.member(!exnClassSupply, longid)
          then
            new (false, RepNames.exnPrefix)
          else
          (
            exnClassSupply := Longid.Set.add(!exnClassSupply, longid);
            add (VMTy.Class (TyName.external (Id.fromString "", 
					      Id.fromString "!" :: longid,
					      0)))
          )
        val reps = map tr tys
        val superrep = tr (MILTy.tyname TyNames.mlexnTyName)
      in
        (!save) 
          (CompileException.makeExnClass(superrep,rep,reps,
            Longid.toString longid));
        rep
      end
    | MILTy.Arrow _ =>
      add (internalClass (false, RepNames.funClass))
    | MILTy.Closure(NONE, []) =>
      add (internalClass (false, RepNames.funClass))

    | _ => new (false, RepNames.closurePrefix)
  end
in
 val tyToRep' = fn ty => (*@TODO: MILTy.memoize ?*) (c:=(!c)+1;tyToRep' ty)
end

fun tyToRep (env : CompileEnv.Env) ty =
let
  val ty = MILTy.forceBounds (#kindenv env) ty
  val rep = tyToRep' ty
in
  rep
end
(*@TODO: is this still necessary? *)
fun externalTyToRep (env : CompileEnv.Env) ty =
if MILTy.eq (ty, MILTys.bool)
then VMTy.ValueClass TyNames.boolTyName
else tyToRep env ty

fun globs () = internalClass (false, RepNames.globalClass)

fun check () = ()
(*
let
  val xs = Dups.duplicates Types.java_type_equal (Map.listItems (!reps))
in
  if null xs then ()
  else Debug.Debug.print 
    ("\nDuplicates: " ^ Pretty.simpleVec "," Types.java_type_toString xs)
end
*)

fun dump prln = 
(prln "Representations:";
 Map.appi
  (fn (ty,jty) => prln ("  " ^ MILTy.toString ty ^ " : " ^ 
    VMTy.toString jty)) (!reps);
 check ())


structure StringMap = MapFn(struct type ord_key = string 
				   val compare = String.compare 
			    end);
fun dumpShared () = 
(Debug.print "\nSharing:";
 let val map =
     Map.foldli
     (fn (ty,jty,m) => 
      let val s = VMTy.toString jty
	  val entry = case StringMap.find(m,s) of SOME tys => ty::tys | NONE => [ty]
      in
	  StringMap.insert(m,s,entry)
      end)
      (StringMap.empty) (!reps)
 in 
     StringMap.appi
     (fn (jty,tys) => (Debug.print ("\n" ^ jty ^ ": " ^ (Int.toString(List.length tys)));
                       List.app (fn ty => (Debug.print "\n   ";
					   Debug.print (MILTy.toString ty))) tys))
     map
 end
 )


fun finish dosave = 
  (SumRep.finish(dosave,!save);
   StaticNewObj.finish(dosave,!save);
   PrintManager.dump showReps dump;
   if Controls.get showSharing then dumpShared () else ();
   reps := Map.empty;
   exportedClasses := TyName.Set.empty)


(*
fun nullValue rep =
case rep of
  Types.F(0, Types.BOOLEAN) => Constants.BOOLEAN (RTInt.fromInt 0)
| Types.F(0, Types.BYTE) => Constants.BYTE (RTInt.fromInt 0)
| Types.F(0, Types.CHAR) => Constants.CHAR (RTInt.fromInt 0)
| Types.F(0, Types.DOUBLE) => Constants.DOUBLE (RTDouble.fromReal 0.0)
| Types.F(0, Types.FLOAT) => Constants.FLOAT (RTFloat.fromReal 0.0)
| Types.F(0, Types.INT) => Constants.INT (RTInt.fromInt 0)
| Types.F(0, Types.LONG) => Constants.LONG (RTLong.fromInt 0)
| Types.F(0, Types.SHORT) => Constants.SHORT (RTInt.fromInt 0)
| _ => Constants.NULL
  *)

end
