(*======================================================================*)
(* Compile a programmer-defined class.                                  *)
(*======================================================================*)
structure CompileClassType :> COMPILECLASSTYPE =
struct


local 
  open CompileEnv TyRep
in

fun compileOne env (classty, (attributes,flags, super, implements),
		 fields, methods) =
let

  val classrep as VMTy.Class class = tyToRep env classty
  (* adjust flags: (default) public becomes private for non-exported classes *)
  val flags =  if TyRep.isExportedClass classty
	       then flags
	       else Symbol.Set.map(fn flag => if Symbol.equal(flag,Id.publicSym) then Id.privateSym else flag) flags
 
  val superrep = Option.map (tyToRep env) super
  val implementsreps = map (tyToRep env) implements

  fun compileAttribute (argtys,resty,bytes) =
      (map (externalTyToRep env) argtys,
       externalTyToRep env resty,
       bytes);

  fun compileMethod (name,attributes,flags, argtys, resty, absopt) =
    let
      (* All argument types including "this" *)
      val isstatic = Symbol.Set.member(flags, Id.staticSym)
      val argreps = map (externalTyToRep env) argtys

      val (argtys', argreps') =
        if isstatic 
        then (argtys, argreps)
        else (classty::argtys, classrep :: argreps)

      val resrep = Option.map (externalTyToRep env) resty
    in
      case absopt of
        NONE => 
        { name = name, override = NONE,
	  attributes = map compileAttribute attributes,
          flags = flags, code = RTInstrs.Empty, stack = 0, locals = [],
          args = map (fn rep => (Id.fromString "", rep)) argreps, resty = resrep } : RTResult.Method

      | SOME ((f,_), (argvars, e) : MILTerm.Abstr) =>
        let
        in
          CompileMethod.compile env 
            (name, NONE, attributes, flags, (ListPair.zip(argvars, argtys'), e))
        end
    end

  (*@BUG: do we ever use constopt (it is discarded below)? *)
  fun compileField (name, flags, ty, constopt) =
  let
    val rep = externalTyToRep env ty
  in
    {
      name = name,
      flags = flags,
      ty = rep,
      value = NONE
    }
  end
in
  {
    name = RTOps.classRepToLongid classrep,
    attributes = map compileAttribute attributes,
    flags = flags, 
    super = getOpt(superrep, RTOps.object),
    interfaces = implementsreps,
    fields = map compileField fields,
    methods = map compileMethod methods
  } : RTResult.Class
end

fun compile env classes =
    let val res = map (compileOne env) classes
	fun cmp ({name=(longid1,depth1),...}:RTResult.Class,
		 {name=(longid2,depth2),...}:RTResult.Class)
	    = depth1 <= depth2
    in
	QuickSort.sort cmp res
    end

end (* of local open *)
end (* of struct *)
