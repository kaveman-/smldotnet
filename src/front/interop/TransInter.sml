(*======================================================================*)
(* Translate interop types into SML types				*)
(*======================================================================*)
structure TransInter :> TRANSINTER =
struct

(*----------------------------------------------------------------------*)
(* Convert an external class handle into an SML type.			*)
(*----------------------------------------------------------------------*)
fun classToML c = 
let
 (*@TODO crusso: avoid repeated traversals of package map*)
  val class = case PackageManager.isValueType c of
    SOME true => false
  | _ => true

  val eq = case PackageManager.hasEquality c of
    SOME true => true
  | _ => false

  val SOME assembly = PackageManager.getAssembly c

  val SOME depth = PackageManager.getDepth c
in
  (* @todo akenn: get eq right *)
  if class 
  then 
    if eq then SMLTy.baseType (TyName.externalEq (assembly, c,depth))
    else SMLTy.baseType (TyName.external (assembly, c, depth))
  else 
    if eq then SMLTy.baseType (TyName.externalValEq (assembly,c,depth))
    else SMLTy.baseType (TyName.externalVal (assembly, c, depth))
end

(*----------------------------------------------------------------------*)
(* The cache of external class definitions, given SML types.		*)
(*----------------------------------------------------------------------*)
type Cache = InterOpTypes.ClassDef Longid.Map.map
val cache = ref (Longid.Map.empty : Cache)

(*----------------------------------------------------------------------*)
(* Given an external class identifier, return its info using SML types.	*)
(* This information is cached.						*)
(*----------------------------------------------------------------------*)
fun lookupExtClass longid =
  case Longid.Map.find(!cache, longid) of
    SOME result => 
    SOME result

  | NONE => 
    case PackageManager.getClass longid of
      SOME data =>
      (cache := Longid.Map.insert(!cache, longid, data); 
      SOME data)

    | NONE => 
      NONE

(*----------------------------------------------------------------------*)
(* Given the SML-typed info about a class, return an environment	*)
(* that appropriately models its static members as value bindings.	*)
(*----------------------------------------------------------------------*)
fun classTypeToVE ty =
let
  val SOME ([], tyname) = SMLTy.fromConsType ty
  val longid = TyName.longid tyname
  val cd = case lookupExtClass longid of
             SOME cd => cd
           | NONE => Debug.fail ("TransInter.classTypeToVE: cannot find " ^ Longid.toString longid)
in
  extClassToVE cd
end

and extClassToVE ({ longid, fields, methods, super, interfaces, ...} 
  : InterOpTypes.ClassDef) =
  let
    val classty = classToML longid
    val enumty = case super of 
	           SOME super => 
	              if SMLTy.eq(super,SMLTy.baseType(TyNames.enumTyName)) 
			  then case List.find (fn {name,... } => Symbol.equal(name, Id.fromString "value__")) fields of
			      SOME {ty,...} => SOME ty 
			    | NONE => NONE
		      else NONE
                 | NONE => NONE 

    (* Add a null value if classty is a non-enum value class *)
    val nullVE = case SMLTy.fromConsType classty of
   		      SOME ([],tn) => if not (TyName.isClass tn) andalso not(isSome enumty) 
					   then Symbol.Map.singleton(Id.null,ValBind.Special(classty, SOME Id.null))
				       else Symbol.Map.empty
		    | _ => Symbol.Map.empty

    val VE =
      foldl 
        (fn ({ name, flags, value,... }, VE) =>

        (* Only public static fields are accessible through structures *)
        if Symbol.Set.member(flags, Id.staticSym)
        andalso Symbol.Set.member(flags, Id.publicSym)
        then
	    case enumty of 
              SOME ty' => (* enum literals are treated as pseudo nullary constructors *)
		  let val SOME ([],tn) = SMLTy.fromConsType classty
		  in
		      Symbol.Map.insert(VE,name,
					ValBind.ConSch(SMLSch.TypeScheme([],classty),
					               ([],tn,Symbol.Map.singleton(List.last longid,SOME ty'))))
		  end
	    | NONE => Symbol.Map.insert(VE, name,ValBind.Special(classty, SOME name))
        else VE)

      (foldl 
        (fn ({ name, flags, ... }, VE) =>

        (* Only public static methods are accessible through structures *)
        if Symbol.Set.member(flags, Id.staticSym)
        andalso Symbol.Set.member(flags, Id.publicSym)
        then Symbol.Map.insert(VE, name, 
          ValBind.Special(classty, SOME name))
        else VE)
        Symbol.Map.empty
        methods
      )
      fields
  
    val superVE = 
      case super of NONE => Symbol.Map.empty
                  | SOME ty => classTypeToVE ty

    val intVEs = map classTypeToVE interfaces
  in
    foldr (Symbol.Map.unionWith #2) VE (nullVE::superVE::intVEs)
  end
  
(* the types of mutable (static and instance) fields are added as convenient abbreviations, not including types of 
   inherited fields *)
fun extClassToTE (c as { longid, fields, methods, super, interfaces, ...} : InterOpTypes.ClassDef) =
    if isSome super andalso SMLTy.eq(valOf(super),SMLTy.baseType(TyNames.enumTyName)) 
	then Symbol.Map.empty 
    else
      foldl 
        (fn ({ name,flags,ty,value}, TE) =>
 	 if Symbol.Set.member(flags, Id.finalSym) 
	     then TE
	 else let val classty = classToML longid
		  val refkind =  if Symbol.Set.member(flags, Id.staticSym)
				     then SMLTy.staticRefType(classty,SMLTy.baseType(TyName.external(Id.fromString "", [name], 0)))
				 else SMLTy.fieldRefType(classty,SMLTy.baseType(TyName.external(Id.fromString "", [name], 0)))
	      in Symbol.Map.insert(TE,name,TyStr.makeConcrete([],SMLTy.refType(ty,refkind)))
	      end) 
	Symbol.Map.empty 
	fields 

fun extClassToStruct c =
let
  val VE = extClassToVE c
  val TE = extClassToTE c
in
  Env.Env(Symbol.Map.empty, TE, VE)
end
    

end




