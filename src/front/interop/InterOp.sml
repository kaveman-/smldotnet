(*======================================================================*)
(* Various operations on types used for inter-operability		*)
(*======================================================================*)

(*@TODO: REVIEW:
         we allow widening from an interface to object,
         and getMethods and getInheritedMethods implicity treats System.Object
	 as a superclass of an interface.
*)
(*@FUTURE: modify getMeta to report Object as a superclass of each interface *)
(*@TODO: REVIEW: getMethods(classty) lists interface methods too - should it do this? *)

structure InterOp :> INTEROP =
struct

val objectType = SMLTy.baseType (TyNames.objectTyName)

fun isValueType ty = 
case SMLTy.fromConsType ty of
  NONE => false
| SOME ([], tyname) => TyName.isExternal tyname andalso not (TyName.isClass tyname)
| _ => false

(*@TODO: review *)
fun isValueTypeRef ty = 
case SMLTy.fromRefType ty of
  NONE => false
| SOME (ty,_) => isValueType ty


(*----------------------------------------------------------------------*)
(* Return true if ty is a primitive interop type, defined to be a	*)
(* nullary tyname that came from the PRIM signature.			*)
(*----------------------------------------------------------------------*)
fun isPrimitive ty =  
  isValueType ty orelse
(*@TODO: this looks wrong, or redundant *)
  SMLTy.eq (ty, SMLPrimTy.boolType)

(*----------------------------------------------------------------------*)
(* Return true if ty is an interop type, with or without options.       *)
(*----------------------------------------------------------------------*)
local
fun isInterop(ty) =
  isPrimitive ty orelse
  case SMLPrimTy.fromOption ty of 
       SOME optionTy => 
	  (SMLTy.isClass optionTy
	   orelse
	   case SMLTy.fromArrayType optionTy of 
	       SOME elemTy => isInterop(elemTy)
	     | NONE => false)
     | NONE => false
in
(* also allow outermost non-null CLR Reference types and addresses *)
val isInterop = fn ty =>
  SMLTy.isClass ty orelse
  case SMLTy.fromArrayType ty of 
      SOME elemTy => isInterop(elemTy)
    | NONE => 
  case SMLTy.fromAddressType ty of 
      SOME elemty => isInterop(elemty)
    | NONE => 
  isInterop ty
end

(*----------------------------------------------------------------------*)
(* Given a predicate for exportability of internal class types, return  *)
(* an 'exportable' predicate for types.                                 *)
(* A type is exportable if it is:                                       *)
(*   primitive; or                             		                *)
(*   <ty> where <ty> is an exportable class type; or			*)
(*   <ty> option where <ty> is an exportable class type; or             *)
(*   <ty> array where <ty> is exportable; or				*)
(*   <ty> array option where <ty> is exportable.                        *) 
(*----------------------------------------------------------------------*)

fun isExportable isExportableClass ty =
  isPrimitive ty orelse
  let
    val tyopt = SMLPrimTy.fromOption ty
    val ty = getOpt (tyopt, ty)
  in
    case SMLTy.fromArrayType ty of
      SOME ty => 
      isExportable isExportableClass ty
    | NONE =>
      case SMLTy.proj ty of
(* SL: or *)
(*
        (SMLTy.Con(tyname, []) | SMLTy.Class (SMLTy.MLClass { tyname, ... })) => 
        ((TyName.isClass tyname) andalso TyName.isExternal tyname) orelse isExportableClass tyname
*)
        SMLTy.Con(tyname, []) => 
        ((TyName.isClass tyname) andalso TyName.isExternal tyname) orelse isExportableClass tyname
      | SMLTy.Class (SMLTy.MLClass { tyname, ... }) => 
        ((TyName.isClass tyname) andalso TyName.isExternal tyname) orelse isExportableClass tyname
      | _ => false
  end

(*----------------------------------------------------------------------*)
(* Given a predicate for exportability of internal class types, return  *)
(* an 'importable' predicate for interop types.                         *)
(* An interop type is importable if it is:                              *)
(*   primitive; or                             		                *)
(*   <ty> & where ty is an importable type		                *)
(*   <ty> option where <ty> is an exported class type; or               *)
(*   <ty> array option where <ty> is importable.                        *) 
(*----------------------------------------------------------------------*)
fun isImportable isExportableClass ty =
  isPrimitive ty orelse
  case SMLTy.fromAddressType ty of
      SOME ty =>
	      isImportable isExportableClass ty
    | NONE =>
  (case SMLPrimTy.fromOption ty of
    NONE => false
  | SOME ty =>
    case SMLTy.fromArrayType ty of
      SOME ty => 
      isImportable isExportableClass ty
    | NONE =>
      case SMLTy.proj ty of
(* SL: or *)
(*
        (SMLTy.Con(tyname, []) | SMLTy.Class (SMLTy.MLClass { tyname, ... })) => 
        TyName.isExternal tyname orelse isExportableClass tyname
*)
        SMLTy.Con(tyname, []) => 
        TyName.isExternal tyname orelse isExportableClass tyname
      | SMLTy.Class (SMLTy.MLClass { tyname, ... }) => 
        TyName.isExternal tyname orelse isExportableClass tyname
      | _ => false)

val toparrayclass = SMLTy.baseType (TyNames.arrayTyName)

fun fromExternalClass ty =
case SMLTy.fromConsType ty of
  SOME ([], tyname) => 
  if TyName.isExternal tyname then SOME (TyName.longid tyname) else NONE

| _ =>
  NONE

fun lookupExtClass longid =
  case TransInter.lookupExtClass longid of
    NONE =>
    Debug.fail ("InterOp.lookupExtClass: class " ^ Longid.toString longid ^ " does not exist")
  | SOME a => a

(*------------------------------------------------------------------------*)
(* Is this type an (external) Enum Type and what is its underlying type?  *)
(*------------------------------------------------------------------------*)
fun isEnumType classty = 
    case fromExternalClass classty of
	NONE => NONE
      | SOME longid => 
	    let val { super, fields, ... } = lookupExtClass longid
		fun isValueField ({name,... } : InterOpTypes.FieldInfo) =
		    Symbol.equal(name, Id.fromString "value__")
	    in  case super of 
		  NONE => NONE
		| SOME super =>
		      if SMLTy.eq(super,SMLTy.baseType(TyNames.enumTyName)) 
			  then case List.find isValueField fields of
			      NONE => Debug.fail "Interop.isEnumType missing value__ field in enum type"
			    | SOME {ty,...} => SOME ty
		      else NONE
 
	    end;

(*----------------------------------------------------------------------*)
(* What's the superclass?						*)
(*----------------------------------------------------------------------*)
fun super ty =
case SMLTy.fromClassType ty of
  SOME (SMLTy.MLClass { super, ... }) => SOME super
| NONE =>
  case fromExternalClass ty of
    SOME longid =>
    let val { flags, super = superopt, ... } = lookupExtClass longid    
    in  if (Symbol.Set.member(flags,Id.interfaceSym))
	then SOME objectType (*@TODO: review *)
	else superopt 
    end
  | NONE => NONE

(*----------------------------------------------------------------------*)
(* Is this class an interface?						*)
(*----------------------------------------------------------------------*)
fun isInterface ty =
let
  val flags = 
  case SMLTy.fromClassType ty of
    SOME (SMLTy.MLClass { flags, ... }) => flags
  | NONE =>
    case fromExternalClass ty of
      SOME longid =>
      let val { flags, ... } = lookupExtClass longid    
      in flags end
    | NONE => Symbol.Set.empty
in
  Symbol.Set.member(flags, Id.interfaceSym)
end

(*----------------------------------------------------------------------*)
(* What are its interfaces?						*)
(*----------------------------------------------------------------------*)
fun interfaces ty =
case SMLTy.fromClassType ty of
  SOME (SMLTy.MLClass { interfaces, ... }) => interfaces
| NONE =>
  case fromExternalClass ty of
    SOME longid =>
    let
      val { interfaces, ... } = lookupExtClass longid    
    in interfaces end
  | NONE => []

(*----------------------------------------------------------------------*)
(* Return the package containing a class				*)
(*----------------------------------------------------------------------*)
fun package ty =
  case fromExternalClass ty of
    SOME longid =>
    rev (List.tl (rev longid))

  | _ => 
    []

(*----------------------------------------------------------------------*)
(* Extends/implements relation						*)
(*----------------------------------------------------------------------*)
fun sub (ty1, ty2) =
let
  fun search root =
    SMLTy.eq(root, ty2)
    orelse
    (let
      val ints = interfaces root
      val superopt = super root
    in
      (case superopt of
        NONE => 
        false

      | SOME super => search super)
      orelse List.exists search ints
    end)
in
  search ty1
end

(*----------------------------------------------------------------------*)
(* Return true if type ty1 can be converted to type ty2 by means of	*)
(* a widening reference conversion                                      *)
(* Both types must be interop types.					*)
(*----------------------------------------------------------------------*)
fun refWidens (ty1, ty2) =
( (*  print ("\nrefWidens (" ^ SMLTy.toString ty1 ^","^ SMLTy.toString ty2 ^ "): "
      ^ "isInterop = " ^ Bool.toString (isInterop ty1) ^ "," ^ Bool.toString (isInterop ty2)); *)
  isInterop ty1 andalso isInterop ty2 andalso
  (
    case SMLTy.fromArrayType ty1 of
      SOME ty1 => 
      sub (toparrayclass,ty2) orelse
      (case SMLTy.fromArrayType ty2 of
        SOME ty2 => refWidens (ty1, ty2)
      | NONE => false)
    
    | NONE =>
      (case SMLPrimTy.fromOption ty1 of
        SOME ty1 =>
        (case SMLPrimTy.fromOption ty2 of
          SOME ty2 => refWidens (ty1, ty2)
        | NONE => false)
      | NONE => 
        (case SMLPrimTy.fromOption ty2 of
          SOME ty2 => refWidens (ty1, ty2)
        | NONE => sub (ty1, ty2)))
  )
)


(*----------------------------------------------------------------------*)
(* Return true if type ty1 can be converted to type ty2 by means of	*)
(* a narrowing reference conversion                                     *)
(* Both types must be interop types.					*)
(*----------------------------------------------------------------------*)
fun refNarrows (ty1, ty2) =
  isInterop ty1 andalso isInterop ty2 andalso
  (
    case SMLTy.fromArrayType ty2 of
      SOME ty2 => 
      sub (toparrayclass,ty1) orelse 
      (case SMLTy.fromArrayType ty1 of
        SOME ty1 => refNarrows (ty1, ty2)
      | NONE => false)
    
    | NONE =>
      (case SMLPrimTy.fromOption ty2 of
        SOME ty2 =>
        (case SMLPrimTy.fromOption ty1 of
          SOME ty1 => refNarrows (ty1, ty2)
        | NONE => false)
      | NONE => 
        sub (ty2, ty1))
  )

(*----------------------------------------------------------------------*)
(* Method argument widening (refWidens + identity on all types)		*)
(*----------------------------------------------------------------------*)
fun argWidens (ty1, ty2) =
  SMLTy.eq (ty1, ty2)
  orelse refWidens (ty1, ty2)


(*----------------------------------------------------------------------*)
(* Does this class derive from a delegate class?	                *)
(*----------------------------------------------------------------------*)
fun isDelegateType classty =
    let val delegateTy = SMLTy.consType([],TyNames.delegateTyName)
	val multicastDelegateTy = SMLTy.consType([],TyNames.multicastDelegateTyName)
    in sub(classty,delegateTy) andalso not(SMLTy.eq(classty,delegateTy)) 
	orelse sub(classty,multicastDelegateTy) andalso not(SMLTy.eq(classty,multicastDelegateTy)) 
    end

(*----------------------------------------------------------------------*)
(* What's the appropriate SML type for this field def?			*)
(*----------------------------------------------------------------------*)
fun fieldTy classty ({name,flags,ty,value, ... } : InterOpTypes.FieldInfo) =
  if Symbol.Set.member(flags, Id.finalSym)
  then (ty, value)
  else if Symbol.Set.member(flags, Id.staticSym)
  then (SMLTy.refType(ty,SMLTy.staticRefType(classty,SMLTy.baseType(TyName.external(Id.fromString "", [name], 0)))),value)
  else (SMLTy.refType(ty,SMLTy.fieldRefType(classty,SMLTy.baseType(TyName.external(Id.fromString "", [name], 0)))),value)

(*----------------------------------------------------------------------*)
(* What's the appropriate SML type for this method def?			*)
(*----------------------------------------------------------------------*)
fun methTy ({ flags, argtys, resty, ... } : InterOpTypes.MethodInfo) =
  let
    (* Void args maps to unit; multiple args to tuples *)
    val smlargty = 
      case argtys of
        [] => SMLPrimTy.unitType
      | [ty] => ty
      | tys => SMLTy.tupleType tys

    (* void result maps to unit *)
    val smlresty =
      case resty of
        NONE => SMLPrimTy.unitType
      | SOME ty => ty
  in
    SMLTy.funType (smlargty, smlresty)
  end

(*----------------------------------------------------------------------*)
(* What's the appropriate SML constructor type for this constructor def?*)
(*----------------------------------------------------------------------*)
fun constructorTy classty ({ flags, argtys, ... } : InterOpTypes.MethodInfo) =
    (classty, SMLTy.funType (case argtys of
      [] => SMLPrimTy.unitType
    | [ty] => ty
    | tys => SMLTy.tupleType tys, classty))

(*----------------------------------------------------------------------*)
(* List types for static fields in the specified class with the		*)
(* specified name. Static fields are not inherited. 			*)
(*----------------------------------------------------------------------*)
fun getStaticFields (classty, lab) =
case fromExternalClass classty of
  NONE => []
| SOME longid =>
  let
    val { fields, super, ... } = lookupExtClass longid
    val nullFields =  if Symbol.equal(lab,Id.null) 
	              then case SMLTy.fromConsType classty of
			     SOME ([],tn) => 
				 if not(TyName.isClass tn) andalso 
				    case super of 
					SOME super => not(SMLTy.eq(super,SMLTy.baseType(TyNames.enumTyName)))
				    | _ => true
				 then [(classty,SOME Constants.NULL)]
				 else []
			   | NONE => []
		      else []
    val validfields =
      List.filter (fn { name, flags, ... } => Symbol.equal(name,lab)
        andalso Symbol.Set.member(flags, Id.staticSym)
        andalso Symbol.Set.member(flags, Id.publicSym)) fields
    
  in
    nullFields@(map (fieldTy classty) validfields)
  end


(*----------------------------------------------------------------------*)
(* List types for static methods in the specified class with the	*)
(* specified name. Static methods are not inherited.			*)
(*----------------------------------------------------------------------*)
fun getStaticMethods (classty, lab) =
case fromExternalClass classty of
  NONE => []
| SOME longid =>
  let
    val { methods, ... } = lookupExtClass longid
    val validmethods =
      List.filter (fn { name, flags, ... } => Symbol.equal(name,lab)
        andalso Symbol.Set.member(flags, Id.staticSym) 
        andalso Symbol.Set.member(flags, Id.publicSym))
        methods
  in
    map methTy validmethods
  end

(*----------------------------------------------------------------------*)
(* List types for non-static public fields inherited by the specified   *)
(* class type with the specified name. The types are paired with their	*)
(* defining class (necessarily external).				*)
(*----------------------------------------------------------------------*)
fun getFields (classty, lab) =
  if isSome(isEnumType classty) then [] else
  case fromExternalClass classty of
    NONE => 
    []
    (*@TODO: list fields inherited by internal classes *)
  | SOME longid =>
    let
      val { super, fields, methods, ... } = lookupExtClass longid

      (* Only public fields are accessible from ML *)
      fun isAccessibleField ({ flags, name, ... } : InterOpTypes.FieldInfo) =
        Symbol.Set.member(flags, Id.publicSym)
        andalso not (Symbol.Set.member(flags, Id.staticSym))
        andalso Symbol.equal(name, lab)

      val accessibleFields = List.filter isAccessibleField fields 
    in
      (* If there aren't any fields in this class then try the superclass *)
      (* If there are fields then they must hide those from the superclass *)
      if null accessibleFields
      then 
        case super of
          NONE => []
        | SOME ty => getFields (ty, lab)
      else map (fn f => (classty, #1 (fieldTy classty f))) accessibleFields
    end
(*----------------------------------------------------------------------*)
(* List types for non-static public methods inherited by the specified  *)
(* class type with the specified name. The types are paired with their	*)
(* defining class.							*)
(*----------------------------------------------------------------------*)
fun getMethods (classty, lab) =
let
  val (super, interfaces, methods) =
  case SMLTy.fromArrayType classty of
    SOME ty => (SOME toparrayclass,[],[])
  | NONE =>
  case fromExternalClass classty of
    NONE =>
    (case SMLTy.fromClassType classty of
      NONE => 
      (NONE, [], [])

    | SOME (SMLTy.MLClass { methods, super, interfaces, flags, ... }) => 
      let
        (* Public is just assumed; static not possible *)
        fun isAccessibleMethod ({ name, flags, ... } : SMLTy.Method) =
          Symbol.equal(name, lab) 
        val methods = List.filter isAccessibleMethod methods
	val superopt = 
(*@TODO: review if Symbol.Set.member(flags,Id.interfaceSym) then NONE else *) 
	    SOME super 
      in
        (superopt, interfaces, methods)
      end
    )

  | SOME longid =>
    let 
      val { methods, super, interfaces, flags, ... } = lookupExtClass longid
      fun isAccessibleMethod ({ name, flags, ... } : InterOpTypes.MethodInfo) =
      let 
        val public = Symbol.Set.member(flags, Id.publicSym)
        val protected = Symbol.Set.member(flags, Id.protectedSym)
        val static = Symbol.Set.member(flags, Id.staticSym)
      in
        Symbol.equal(name, lab) andalso (public orelse protected) andalso not static
      end
      val methods = List.filter isAccessibleMethod methods
      val methods = map (fn m as { name, flags, ... } =>
        { name = name, flags = flags, ty = methTy m }) methods

      val superopt = if Symbol.Set.member(flags,Id.interfaceSym) 
		     then  SOME objectType
		     else  super
    in
      (superopt, interfaces, methods)
    end

  (* Get inherited methods *)
  val inheritedmethods = 
    List.concat 
    (map (fn ty => getMethods (ty, lab)) (OptionOps.toList super @ interfaces))

  val methods = map (fn { ty, flags, ... } => (flags,(classty,ty))) methods

  (* Remove those inherited methods that are overridden *)
  fun isOverridden (_,(_,ty)) = 
    List.exists (fn (_,(_,ty')) => SMLTy.eq(ty,ty')) methods
in
  methods @
  List.filter (not o isOverridden) inheritedmethods
end



(*----------------------------------------------------------------------*)
(* List function types for      constructors in the specified class     *)
(* type.								*)
(*----------------------------------------------------------------------*)

fun getConstructors pred classty =
  case fromExternalClass classty of
    NONE =>
    (case SMLTy.fromClassType classty of
      NONE => 
      []

    | SOME (SMLTy.MLClass { initargty, ... }) => 
      case initargty of
        NONE => []
      | SOME initargty => [(classty, SMLTy.funType(initargty, classty))]
    )
  | SOME longid =>
    let 
      val { methods, ... } = lookupExtClass longid
      fun isAccessibleMethod ({ name, flags, ... } : InterOpTypes.MethodInfo) =
      let 
        val static = Symbol.Set.member(flags, Id.staticSym)
      in 
        Symbol.equal(name, Id.fromString "<init>") andalso pred flags
        andalso not static      
      end
      val derivesDelegateTy = isDelegateType classty 
      fun isInvokeMethod ({ name, flags, ... } : InterOpTypes.MethodInfo) =
	  Symbol.equal(name, Id.fromString "Invoke") 
      val conmethods = if derivesDelegateTy 
			then case List.filter isInvokeMethod methods of
			    [m] => 
				let val ty = SMLTy.funType(methTy m, classty)
				in
				    case (List.filter isAccessibleMethod methods) of
					[] => []
                                    | _  => [(classty,ty)] (* only add a constructor if classty publishes one *)
				end
			  | _ => []
		    else map (constructorTy classty) (List.filter isAccessibleMethod methods)
    in
	conmethods
    end

(*----------------------------------------------------------------------*)
(* Given an SML argument type, deconstruct to multiple args		*)
(*----------------------------------------------------------------------*)
fun unpackArgTy argty =
  case SMLTy.fromProd argty of
    SOME argtys => argtys
  | NONE => [argty]

(*----------------------------------------------------------------------*)
(* Given a method function type, deconstruct to multiple args and 	*)
(* optional result.							*)
(*----------------------------------------------------------------------*)
fun unpackMethTy ty =
let
  val SOME (argty, resty) = SMLTy.fromFunType ty
  val restyopt = 
    if SMLTy.eq (resty, SMLPrimTy.unitType) then NONE else SOME resty
in
  (unpackArgTy argty, restyopt)
end
  
(*----------------------------------------------------------------------*)
(* Return true if m1 is a more specific method type than m2.		*)
(*----------------------------------------------------------------------*)
fun moreSpecific (m1 as (class1, funty1), m2 as (class2, funty2)) =
let
  val (argtys1,_) = unpackMethTy funty1
  val (argtys2,_) = unpackMethTy funty2
in
  argWidens (class1, class2) andalso length argtys1 = length argtys2 andalso
  ListPair.all argWidens (argtys1, argtys2)
end

(*----------------------------------------------------------------------*)
(* Return true if the specified argument type can be passed to the 	*)
(* specified method type.						*)
(*----------------------------------------------------------------------*)
fun argsMoreSpecific (argty, (class, funty)) =
let
  val argtys1 = unpackArgTy argty
  val (argtys2,_) = unpackMethTy funty
in
  length argtys1 = length argtys2 andalso 
  ListPair.all argWidens (argtys1, argtys2)
end

(*----------------------------------------------------------------------*)
(* Given a list of method types, return the unique most specific type.  *)
(*----------------------------------------------------------------------*)
fun mostSpecific methtys =
let
  fun most (m, []) = SOME m
    | most (m1, (m2::ms)) =
      if moreSpecific (m1, m2) then most (m1, ms) else
      if moreSpecific (m2, m1) then most (m2, ms) else
      NONE
in
  case methtys of
    [] => NONE
  | m::ms => most (m, ms)
end


(*----------------------------------------------------------------------*)
(* Return NONE if this class is exportable, under the assumption that	*)
(* all of "classes" are exportable.                                     *)
(* Otherwise return SOME reason.                                        *)
(*----------------------------------------------------------------------*)
fun isExportableClassType isExternalClass 
   (classty as SMLTy.MLClass{tyname,flags=classflags,super,interfaces,initargty,methods}) =
let
  val isDelegate = isDelegateType (SMLTy.classType classty) 

  (* isExportableClassType extended to a predicate on SMLTy.Type *)
  fun isExportableClass isExternalClass ty = 
      case SMLTy.fromClassType ty of 
       NONE =>  (* assume its an external class *) true 
     | SOME (mlclass as SMLTy.MLClass{tyname,...}) => 
	   (isExternalClass tyname) 

  fun findBadConstructorArgs () = 
      case initargty of
	NONE => NONE
      | SOME ty => 
      if isDelegate
      then NONE (* correct by construction, but check Invoke *)
      else
      let val argtys = unpackArgTy ty
	  fun importable ty = isInterop ty andalso isImportable isExternalClass ty
      in
	  if List.all importable argtys
	      then NONE
	  else SOME ("constructor argument types " ^
		     SMLTy.toString ty ^
		     " contain non-importable type " ^ 
		     SMLTy.toString (valOf(List.find(not o importable) argtys)))
      end
	   
  fun findBadMethodArgs [] = findBadConstructorArgs()
    | findBadMethodArgs ({name,flags,ty}::methods) =
      let val (argtys,restyopt) = unpackMethTy ty
	  fun importable ty = isInterop ty andalso isImportable isExternalClass ty
      in
      if Symbol.Set.member(flags,Id.privateSym) 
      orelse List.all importable argtys
      then findBadMethodArgs methods
      else SOME 
        ("argument types of non-private method " ^ Id.toString name ^ 
	 " : " ^ SMLTy.toString ty ^
         " contain non-importable type " ^ 
	 SMLTy.toString (valOf(List.find (not o importable) argtys)))
      end

  fun  findBadMethodResults [] = findBadMethodArgs methods
     | findBadMethodResults ({name,flags,ty}::methods) = 
      let val (_,restyopt) = unpackMethTy ty 
      in 
	  case restyopt of 
	      NONE => findBadMethodResults methods
	    | SOME resty =>
		  if Symbol.Set.member(flags,Id.privateSym) 
		      then findBadMethodResults methods
		  else
		      if (Symbol.Set.member(classflags,Id.sealedSym) (*@TODO: review *)
			  orelse Symbol.Set.member(flags,Id.finalSym) (*@TODO: review *))
			 andalso not(isDelegate) 
			 then 
			      if isInterop resty andalso isExportable (*@TODO: review*)  isExternalClass resty
				  then findBadMethodResults methods
			      else SOME ("non-private, final method " ^ Id.toString name ^
					 " : " ^ SMLTy.toString ty ^
					 " has non-interop result type " ^
					  SMLTy.toString resty)
		      else
			  if isInterop resty andalso isImportable isExternalClass resty
			      then findBadMethodResults methods
			  else SOME ("non-private, non-final method " ^ Id.toString name ^ 
				     " : " ^ SMLTy.toString ty ^
				     " has non-exportable result type " ^
				     SMLTy.toString resty)
      end		

  fun findBadInterface [] = findBadMethodResults methods
    | findBadInterface (int::ints) = 
      if isExportableClass isExternalClass int
      then findBadInterface ints
      else SOME ("interface " ^ SMLTy.toString int ^ " is not exportable")

  val result =
    if isExportableClass isExternalClass super
    then findBadInterface interfaces
    else SOME ("superclass " ^ SMLTy.toString super ^ " is not exportable")
in
  case result of
    NONE => NONE
  | SOME reason => SOME ("Class " ^ SMLTy.toString (SMLTy.classType classty)  ^  
			 " cannot be exported: " ^ reason)
end


(* factor with getMethods *)
fun getInheritedMethods(classty,includeRoot) =
let
  val (super, interfaces, methods) =
  case fromExternalClass classty of
    NONE =>
    (case SMLTy.fromClassType classty of
      NONE => 
      (NONE, [], [])

    | SOME (SMLTy.MLClass { methods, flags, super, interfaces, ... }) => 
      let
        (* Public is just assumed; static not possible *)
        fun isAccessibleMethod ({ name, flags, ... } : SMLTy.Method) =
          (* Symbol.equal(name, lab)  *) true
        val methods = List.filter isAccessibleMethod methods
	val methods = List.map(fn {name,flags,ty} => 
			       let val (argtys,resty) = unpackMethTy ty 
			       in
				   (classty,{name=name,flags=flags,argtys=argtys,resty=resty})
			       end
			       ) methods
	val superopt = 
	    (* if Symbol.Set.member(flags,Id.interfaceSym) then NONE else *)
	    SOME super
      in
        (superopt, interfaces, methods)
      end
    )

  | SOME longid =>
    let 
      val { methods, super, interfaces, flags, ... } = lookupExtClass longid
      fun isAccessibleMethod ({ name, flags, ... } : InterOpTypes.MethodInfo) =
      let 
        val public = Symbol.Set.member(flags, Id.publicSym)
        val protected = Symbol.Set.member(flags, Id.protectedSym)
        val static = Symbol.Set.member(flags, Id.staticSym)
      in
          (public orelse protected) andalso not static (*TODO: virtual *)
      end
      val methods = List.filter isAccessibleMethod methods
      val superopt =  if Symbol.Set.member(flags,Id.interfaceSym) 
		      then SOME objectType  (*@TODO: review *)
		      else super
    in
	(superopt, interfaces, List.map(fn mi => (classty,mi))methods)
    end

  (* Get inherited methods *)
  val inheritedmethods = 
    List.concat 
    (map (fn ty => getInheritedMethods(ty,true)) (OptionOps.toList super @ interfaces))


  val methods = if includeRoot then methods else []  

  (* Remove those inherited methods that are overridden *) 
  fun isOverridden (_,{name,argtys,resty,flags}) = 
    List.exists (fn (_,{name=name',argtys=argtys',resty=resty',flags=flags'}) =>
                   Symbol.equal(name,name')
		   andalso Eq.list SMLTy.eq (argtys,argtys')
		   andalso Eq.option SMLTy.eq (resty,resty')) methods 
in
  methods @
  List.filter (not o isOverridden) inheritedmethods
end


fun getClassFlags(classty) =
  case fromExternalClass classty of
    NONE =>
    (case SMLTy.fromClassType classty of
      NONE => 
      Symbol.Set.empty
    | SOME (SMLTy.MLClass { flags,... }) => 
      flags)
  | SOME longid =>
    let 
      val {flags,... } = lookupExtClass longid
    in
      flags
    end


(*----------------------------------------------------------------------*)
(* List types for non-static public members inherited by the specified  *)
(* class type. The types are paired with their name                     *)
(* defining class (necessarily external).				*)
(* NOTE: only intended for language service use				*)
(*----------------------------------------------------------------------*)
fun getMembers(classty) =
let 
fun getFields (classty) =
  if isSome(isEnumType classty) then [] else
  case fromExternalClass classty of
    NONE => 
    []
    (*@TODO: list fields inherited by internal classes *)
  | SOME longid =>
    let
      val { super, fields, methods, ... } = lookupExtClass longid

      (* Only public fields are accessible from ML *)
      fun isAccessibleField ({ flags, name, ... } : InterOpTypes.FieldInfo) =
        Symbol.Set.member(flags, Id.publicSym)
        andalso not (Symbol.Set.member(flags, Id.staticSym))
      val accessibleFields = List.filter isAccessibleField fields 
    in
      (* If there aren't any fields in this class then try the superclass *)
      (* If there are fields then they must hide those from the superclass *)
      if null accessibleFields
      then 
        case super of
          NONE => []
        | SOME ty => getFields (ty)
      else map (fn f as {name,...}=> (name,(classty, #1 (fieldTy classty f)))) accessibleFields
    end


fun getMethods (classty) =
let
  val (super, interfaces, methods) =
  case SMLTy.fromArrayType classty of
    SOME ty => (SOME toparrayclass,[],[])
  | NONE =>
  case fromExternalClass classty of
    NONE =>
    (case SMLTy.fromClassType classty of
      NONE => 
      (NONE, [], [])

    | SOME (SMLTy.MLClass { methods, super, interfaces, flags, ... }) => 
      let
        (* Public is just assumed; static not possible *)
        fun isAccessibleMethod ({ name, flags, ... } : SMLTy.Method) =
          true
        val methods = List.filter isAccessibleMethod methods
	val superopt = 
            if Symbol.Set.member(flags,Id.interfaceSym) 
            then NONE (*@TODO: review *)
	    else SOME super 
      in
        (superopt, interfaces, methods)
      end
    )

  | SOME longid =>
    let 
      val { methods, super, interfaces, flags, ... } = lookupExtClass longid
      fun isAccessibleMethod ({ name, flags, ... } : InterOpTypes.MethodInfo) =
      let 
        val public = Symbol.Set.member(flags, Id.publicSym)
        val protected = Symbol.Set.member(flags, Id.protectedSym)
        val static = Symbol.Set.member(flags, Id.staticSym)
      in
        not(Symbol.equal(name,Id.fromString "<init>")) andalso (public orelse protected) andalso not static
      end
      val methods = List.filter isAccessibleMethod methods
      val methods = map (fn m as { name, flags, ... } =>
        { name = name, flags = flags, ty = methTy m }) methods

      val superopt = if Symbol.Set.member(flags,Id.interfaceSym) 
		     then  NONE (*@TODO: review *)
		     else  super
    in
      (superopt, interfaces, methods)
    end

  (* Get inherited methods *)
  val inheritedmethods = 
    List.concat 
    (map (fn ty => getMethods (ty)) (OptionOps.toList super @ interfaces))

  val methods = map (fn { ty,name,flags, ... } => (name,(classty,ty))) methods

  (* Remove those inherited methods that are overridden *)
  fun isOverridden (_,(name,ty)) = 
    List.exists (fn (_,(name',ty')) => SMLTy.eq(name,name') andalso SMLTy.eq(ty,ty')) methods
in
  methods @
  List.filter (not o isOverridden) inheritedmethods
end

in  getFields(classty)@getMethods(classty)
end

end

