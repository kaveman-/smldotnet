(*======================================================================*)
(* Additional, post-elaboration  checks on _classtype declarations.     *)
(*@TODO: review and complete                                            *)
(*======================================================================*)
structure ElabCheckClass :> ELABCHECKCLASS =
struct

structure T = SMLTerm

local open 
  Syntax 
  SMLTy 
  SMLPrimTy  
  ElabState
in
structure IMap=IntMap

(*@TODO: move to Interop *)
fun fromInterop ty = if InterOp.isInterop ty
			 then SOME (getOpt(SMLPrimTy.fromOption ty,ty))
                     else NONE



structure Map = Symbol.Map

fun mem modifier mods = Symbol.Set.member(mods,modifier)

structure Class = struct
    val validflags as [ABSTRACT,INTERFACE,SEALED,PRIVATE,PUBLIC] =
	[Id.abstractSym,Id.interfaceSym,Id.sealedSym,Id.privateSym,Id.publicSym]
    fun checkFlags (loc,flags) = 
	let val others = Symbol.Set.listItems
	                 (Symbol.Set.difference(flags,Symbol.Set.addList(Symbol.Set.empty,
									 validflags)))
	in
	if List.null others 
	then
	case List.map(fn flag => Symbol.Set.member(flags,flag))
 	       [ABSTRACT,INTERFACE,SEALED,PRIVATE,PUBLIC] of
	       [true,   false,    false,true,   false] => ()
	      |[true,   false,    false,false,  true] => ()
	      |[true,   false,    false,false,  false] => ()
	      |[false,  true,     false,true,   false] => ()
	      |[false,  true,     false,false,  true] => ()
	      |[false,  true,     false,false,  false] => ()
	      |[false,  false,    true, true,   false] => ()
	      |[false,  false,    true, false,  true] => ()
	      |[false,  false,    true, false,  false] => ()
	      |[false,  false,    false, true, false] => ()
	      |[false,  false,    false, false, false] => ()
	      |[_,           _,       _,  _,    _] =>
                 error (Error.error(loc, "illegal combination of class modifiers"),[])
              | _ => Debug.fail("ElabCheckClass.Class.checkFlags")
	else
	      error (Error.error(loc, "illegal class flags: "^
				      Pretty.simpleVec "," Id.toString others),
		     [])
	end
end


structure Method = struct
    val validflags as [ABSTRACT,FINAL,PROTECTED,PRIVATE,PUBLIC] =
	[Id.abstractSym,Id.finalSym,Id.protectedSym,Id.privateSym,Id.publicSym]
    fun checkFlags (loc,name,flags,ty) = 
	let val others = Symbol.Set.listItems
	                 (Symbol.Set.difference(flags,Symbol.Set.addList(Symbol.Set.empty,
									 validflags)))
	in
	if List.null others 
	then
	case List.map(fn flag => Symbol.Set.member(flags,flag))
 	       [ABSTRACT,FINAL,PROTECTED,PRIVATE,PUBLIC] of
	       [true,   false,    true,  false,   false] => ()
	      |[true,   false,    false, true,  true] => ()
	      |[true,   false,    false, false,  true] => () (*?*)
	      |[true,   false,    false, false,  false] => () (*?*)
	      |[false,  true,     true,  false,   false] => ()
	      |[false,  true,     false, true,    false] => ()
	      |[false,  true,     false, false,  true] => ()
	      |[false,  true,     false, false,  false] => ()
	      |[false,  false,    true,  false,   false] => ()
	      |[false,  false,    false,  true,  false] => ()
	      |[false,  false,    false,  false, true] => ()
	      |[false,  false,    false,  false, false] => ()
	      |[_,           _,       _,  _,    _] =>
                 error (Error.error(loc, "illegal combination of method modifiers"),[])
              | _ => Debug.fail("ElabCheckClass.Class.checkFlags")
	else
	      error (Error.error(loc, "illegal method flags: "^
				      Pretty.simpleVec "," Id.toString others),
		     [("on method: "^Id.toString name,ty)])
	end
end

fun isAbstractClass flags = mem Class.ABSTRACT flags orelse 
                            mem Class.INTERFACE flags

(*----------------------------------------------------------------------*)
(* Check validity of method types.				        *)
(* (1) If more than one method has the same name and no. of args, then: *)
(*     (a) they must all have interop types for args and result; and    *)
(*     (b) the external arg types must be distinct                      *)
(*         (after option removal).                                      *)
(*----------------------------------------------------------------------*)
fun checkMethodSigs loc (methods: SMLTy.Method list) =
let
  fun gatherMethods ({name,flags,ty}, m) =
  let  val (argtys,restyopt) = InterOp.unpackMethTy ty  (*@TODO: may fail if ty is not a function!*)
  in
  case Map.find(m, name) of
    NONE => 
    Map.insert(m, name, IMap.insert(IMap.empty, length argtys,
      [(argtys, restyopt)]))
  | SOME im =>
    case IMap.find(im, length argtys) of
      NONE =>
      Map.insert(m, name, 
        IMap.insert(im, length argtys, [(argtys, restyopt)]))

    | SOME ms =>
      Map.insert(m, name, 
        IMap.insert(im, length argtys,(argtys,restyopt)::ms))
  end

  val m = foldl gatherMethods Map.empty methods

  fun checkMethods [] = ()
    | checkMethods ((name, im)::m) =
      let
        fun checkMethods' [] = checkMethods m 
          | checkMethods' ((numargs, sigs)::im) =
            if length sigs <= 1 then checkMethods' im
            else
            let
              fun checkSigs argtyss [] = ()
                | checkSigs argtyss ((argtys,restyopt)::rest) =
                  case Option.map fromInterop restyopt of  (*@TODO:review *)
                    SOME _ => 
                    error (Error.error(loc, 
				       "multiple methods with name " ^  
				       Id.toString name ^ 
				       " do not all have interop types"), [])

                | _ =>            
                  let 
                    val argtys1 = List.mapPartial fromInterop argtys
                  in
                    if length argtys1 <> numargs
                    then error (Error.error(loc,
                      "multiple methods with name " ^ 
                       Id.toString name ^
                      " do not all have interop types"), [])
                    else
                      if List.exists (fn argtys2 => 
                        Eq.list SMLTy.eq (argtys1,argtys2)) argtyss
                      then error (Error.error(loc,
                        "multiple methods with name " ^ 
                        Id.toString name ^ 
                        " have same signature"), [])
                      else checkSigs (argtys1::argtyss) rest
                  end
            in
              checkSigs [] sigs;
              checkMethods' im
            end
     
      in
        checkMethods' (IMap.listItemsi im)
      end                        
in
  checkMethods (Map.listItemsi m)
end
  

(*----------------------------------------------------------------------*)
(* Check validity of inheritance.                    			*)
(* First, superclasses                                                  *)
(* (a) If superclass is not public then it must be in the same assembly *)
(* (b) The superclass must not be final.                                *)
(* Note: it's not possible for a class to subclass itself because (in   *)
(* contrast to C#) we don't type check several classes simultaneously.  *)
(*                                                                      *)
(* Second, superinterfaces.                                             *)
(* (a) If any interface is not public then it must be in the same       *)
(*     assembly.                                                        *)
(* (b) Superinterfaces must be distinct.                                *)
(*                                                                      *)
(* Third, overridden/hidden methods                                     *)
(* (a) Types of arguments and result must match overridden/hidden       *)
(*     method.                                                          *)
(* (d) Disallow overriding methods in signatures.                       *) 
(*                                                                      *)
(*----------------------------------------------------------------------*)
fun checkInherit (loc,insig) 
  (class as 
   (MLClass(classinfo as {flags,super=superclassty,interfaces=ints,methods=methods,...}) : SMLTy.ClassType) 
  ) =

let

  val classty = SMLTy.inj (Class class)
  val inheritedmethods = InterOp.getInheritedMethods(classty,false)

  fun checkMethod (methodinfo as {name,flags as methmods,ty}) =
  let
    val (argtys,restyopt) =  InterOp.unpackMethTy ty  (*@TODO: may fail *)
    val numargs = length argtys
    val isExportableMethod = 
      List.all (InterOp.isImportable TyName.isClass) argtys
      andalso
      case restyopt of 
	  SOME resty => InterOp.isExportable TyName.isClass resty
	| NONE => true

    val inheritedmethods = 
      List.filter (fn (_, {name=name',argtys=argtys',...}) => 
        Symbol.equal(name,name') andalso length argtys' = numargs) 
      inheritedmethods
  in
    if not isExportableMethod
    then 
      if null inheritedmethods then ()
      else 
        (error (Error.error(loc, 
			    "possibly-overriding method has non-exportable type: "), 
			    [(Id.toString name,ty)]))
    else 
    let
      val inheritedmethods = 
        List.filter (fn (_, {argtys=argtys', ...}) =>
          Eq.list SMLTy.eq (argtys, argtys')) inheritedmethods
    in
      case inheritedmethods of
        [] => ()
      | (_,{name=name',flags=methmods',argtys=argtys',resty=restyopt'})::_ =>
        if not (Eq.option SMLTy.eq (restyopt, restyopt'))
        then 
          (error (Error.error(loc, 
			      "overriding method has wrong result type: "),
			      [(Id.toString name,ty)]))
        else 
        let
          val final' = mem Method.FINAL methmods'
        in
          if final' 
          then 
            (error (Error.error(loc,"final method cannot be overridden: "),
				[(Id.toString name,ty)]))
          else
          let
(*            val public =  mem Method.PUBLIC methmods 
            val public' = mem Method.PUBLIC methmods' *)
            val protected = mem Method.PROTECTED methmods
            val protected' = mem Method.PROTECTED methmods'
            val private = mem Method.PRIVATE methmods
            val public =  not (protected' orelse private)
            val public' = not protected'

          in
            if public' andalso not public
            then (error (Error.error(loc,
				     "non-public method cannot override public method: "),
				     [(Id.toString name,ty)]))
            else  if protected' andalso not (protected orelse public )
            then (error (Error.error(loc,
				     "non-public/protected method cannot override protected method: "),
				     [(Id.toString name,ty)]))
            else if private andalso not (public' orelse protected')
            then (error (Error.error(loc,
				     "private method cannot override non-public/protected method: "),
				     [(Id.toString name,ty)]))
            else if insig 
            then (error (Error.error(loc,
				     "overriding method not permitted in signature: "),
				     [(Id.toString name,ty)]))
            else ()
          end
        end
      end
  end

  fun checkSuper () =
    let
      val flags	= InterOp.getClassFlags superclassty
    in
      if mem Class.INTERFACE flags      
      then error (Error.error(loc, 
        "specified superclass is an interface type"), [])
      else if not (mem Class.PUBLIC flags)
(*@TODO: review: *)
	      andalso not(isSome (SMLTy.fromClassType superclassty)) (* internal *)
      then error (Error.error(loc, 
          "non-public superclass not accessible"), [])
      else 
      if mem Class.SEALED flags
      then error (Error.error(loc, "superclass is final"), [])
      else ()
    end

  fun checkInt intty =
    let
      val flags	= InterOp.getClassFlags intty
    in
      if not (mem Class.INTERFACE flags)
      then error (Error.error(loc, 
        "specified superinterface is a class type"), [])
      else if not (mem Class.PUBLIC flags)
(*@TODO: review: *)
	      andalso not(isSome (SMLTy.fromClassType intty)) (* internal *)
      then error (Error.error(loc, 
          "non-public superinterface not accessible"), [])
      else ()
    end

  fun checkInts () =
    ((case Dups.duplicates SMLTy.eq ints of
        [] => ()
      | dups => error(Error.error(loc, "duplicate superinterfaces: " ^ 
          Pretty.simpleVec "," SMLTy.toString dups), []));
    map checkInt ints;
    checkSuper ())

(************************************************************************)
(* Check there are no abstract methods left in a non-abstract class.    *)
(************************************************************************)

  fun checkAbstract () = 
    (*@TODO: remove crummy repeat of getInheritedMethods *)
    let val inheritedmethods = InterOp.getInheritedMethods(classty,true) (* include root methods *)
    in
	if isAbstractClass flags then ()
	else  
	    let
		fun checkMethod (classty',{name,flags,argtys,resty}) =
		    if mem Method.ABSTRACT flags
			andalso not (SMLTy.eq(classty,classty'))
			andalso (not(InterOp.isInterface classty') orelse
				 List.exists (fn int => SMLTy.eq(int,classty')) ints)
			    then error (Error.error(loc, 
						    "abstract method not implemented: " ^ Id.toString name),[])
		    else ()
	    in
		app checkMethod inheritedmethods
	    end
    end
	
in
  checkInts ();
  List.app checkMethod methods;
  checkAbstract ();
  ()
end


(*----------------------------------------------------------------------*)
(* Abstract methods may only appear in abstract classes.                *)
(* Concrete methods can't have modifier [abstract]                      *)                                                 (*----------------------------------------------------------------------*)
fun checkMethodBinds (loc,SMLTy.MLClass{flags,super,...},methods:SMLTerm.Method list) =
  let  
       fun checkInterfaceMethod {name,flags,body,ty,attributes} =
	       if Option.isSome body
		   then error(Error.error(loc,"interface method must be abstract "^Id.toString name), [])
	       else ()
       fun checkClassMethod {name,flags,body,ty,attributes} = 
	       if Option.isSome body 
		  then if Symbol.Set.member(flags,Id.abstractSym) 
			   then error(Error.error(loc,"concrete method has abstract modifier "^Id.toString name), [])
		       else ()
	       else error(Error.error(loc,"concrete class declares abstract method "^Id.toString name), [])
       fun checkAbstractClassMethod {name,flags,body,ty,attributes} =
	       if Option.isSome body andalso Symbol.Set.member(flags,Id.abstractSym) 
		   then error(Error.error(loc,"concrete method has abstract modifier "^Id.toString name), [])
	       else ()
  in  
      List.app (fn {name,flags,ty,...} => Method.checkFlags(loc,name,flags,ty)) methods;
      List.app (if Symbol.Set.member(flags,Class.INTERFACE)  
	        then checkInterfaceMethod
	        else if Symbol.Set.member(flags,Class.ABSTRACT) 
		    then checkAbstractClassMethod
		    else checkClassMethod) methods
  end


fun checkClassDec (
  SMLTerm.ClassType
  {loc,
   tyname (* TyName.TyName*),
   attributes(* AttExp list*),
   flags (* Symbol.Set.set*),
   conattributes(* AttExp list*),
   superty (* SMLTy.Type*),
   superarg (* Exp*),
   interfaces (* SMLTy.Type list*),
   methods (* Method list*),
   localdec (* Dec*),
   argpat (* Pat*),
   argty (* SMLTy.Type *)
   }) =
  let
    (* reconstruct the MLClass *)
    (*@FUTURE: better to have it handy in the dec *)
    val methodsigs = List.map (fn {name,flags,ty,...} => {name=name,flags=flags,ty=ty}) methods 
    val class = SMLTy.MLClass{tyname= tyname,flags= flags,
			      super= superty,
			      interfaces= interfaces, 
			      initargty=SOME argty,
			      methods=methodsigs}
  in
      if SMLTy.eq(SMLTy.baseType (TyNames.multicastDelegateTyName),superty)
	  then () (* assume correct by construction *)
      else (Class.checkFlags(loc,flags);
	    checkMethodBinds (loc,class,methods);
	    checkMethodSigs loc methodsigs;
	    checkInherit (loc,false) class)
  end 

end (* of local *)

end (* of struct *)