(*======================================================================*)
(* Environment of an entity (structure, signature, functor)             *)
(*======================================================================*)
structure SepCompEnv :> SEPCOMPENV =
struct

local open UnitTypes 
in


(*----------------------------------------------------------------------*)
(* What SML environment do we have cached for this entity?  		*)
(*----------------------------------------------------------------------*)
fun getE entity = 
case Entity.Map.find(!UnitManager.cache, entity) of
  NONE => 
  NONE

| SOME (Sig (_,E), _) => 
  SOME E

| SOME (Str { E, ...}, _) =>
  SOME E


(*----------------------------------------------------------------------*)
(* Construct a skeleton environment for external classes and packages   *)
(*----------------------------------------------------------------------*)
fun getImportedEnv { classes = usedclasses, packages = usedpackages } =
let

(*  val _ = PrintManager.print("\n classes: "^(Pretty.simpleVec ", " Longid.toString (Longid.Set.listItems usedclasses)));
    val _ = PrintManager.print("\n packages: "^(Pretty.simpleVec ", " Longid.toString (Longid.Set.listItems usedpackages)));
*)

  val info = PackageManager.getTop ()

  fun convert longid (PackageManager.Package { packages, classes }) =
  let
    val usedpackage = Longid.Set.member(usedpackages, longid)  orelse 
	              Longid.Set.member(usedclasses,longid)

    (*@TODO: is the use of InterOp.getConstructors etc. redundant? *)

    fun makeTy id = TransInter.classToML (longid @ [id])
     
    fun hasConstructor id =
    let
      fun publicOrProtected flags = (*@TODO: review *)
	  Symbol.Set.member(flags,Id.publicSym) orelse
	  Symbol.Set.member(flags,Id.protectedSym) 
      val longid = longid @ [id]
    in
      case PackageManager.hasConstructor longid of
        SOME b => b
      | NONE => not (null (InterOp.getConstructors publicOrProtected (makeTy id)))
    end

    fun isEnum id =
    let
      val longid = longid @ [id]
    in
       case  PackageManager.isEnum longid of
        SOME false => NONE
      | _ => InterOp.isEnumType (makeTy id)
    end

    fun makeTyStr id = 
	let val ty = makeTy id 
	in
	    TyStr.makeConcrete([],ty)
	end

    fun makeValBind id = 
    if hasConstructor id
    then SOME (ValBind.Special(makeTy id, NONE))
    else case isEnum id  of
	   SOME ty =>
	     let val classty = makeTy id
		 val SOME (_,tn) = SMLTy.fromConsType classty
		 val datDef = ([], tn,Symbol.Map.singleton(id,SOME ty))
	     in
		SOME (ValBind.ConSch(SMLSch.TypeScheme([],SMLTy.funType(ty,classty)),datDef))
	     end
	 | _ => NONE 

    (* Create type bindings for each class *)
    val TE = Symbol.Map.mapi (fn (id, _) => makeTyStr id) classes

    (* Create "constructor" value bindings for each class; don't bother
       if the enclosing package isn't used *)
    val VE = 
      if usedpackage 
      then Symbol.Map.mapPartiali (fn (id, _) => makeValBind id) classes
      else Symbol.Map.empty 


    (* Create structure bindings each class; only fill in those used *)
    val SE1 = Symbol.Map.mapi (fn (id, _) => 
      if  Longid.Set.member(usedclasses, longid @ [id]) 
      then case TransInter.lookupExtClass (longid @ [id]) of
        NONE => EnvOps.emptyE
      | SOME cd => TransInter.extClassToStruct cd
      else EnvOps.emptyE) classes


    (* Create structure bindings for each package *)
    val SE2 = Symbol.Map.mapi
      (fn (id, ref p) => 
          case Symbol.Map.find(SE1,id) of
	     SOME E => EnvOps.EplusE E (convert (longid @ [id]) p)
          | NONE => (convert (longid @ [id]) p)
      ) packages

  in
      Env.Env(EnvOps.SEplusSE  SE1  SE2, TE, VE)
  end
in
  convert [] info
end

end (* of local *)
end (* of struct *)

