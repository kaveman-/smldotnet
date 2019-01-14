(*======================================================================*)
(* Compile the G class, given global variable types, global function    *)
(* definitions, and a <clinit> term.                                    *)
(*======================================================================*)
structure CompileGlobals :> COMPILEGLOBALS =
struct

local 
  open MILTerm RepNames CompileEnv TyRep CompileOps
  structure I = RTInstrs
in

fun compile env (globvars, globfuns, clinit) =

let
  (*@TODO: this function accumulates all methods before saving them
           we could do better and emit the methods eagerly, freeing
	   space used by code sequences *)
           
           
  fun compileFun (tyvars, f:MILTerm.BoundVar, (xs,e), cty) =
  let
    val env = envPlusTyVars env tyvars
  in
    CompileMethod.compile env (Id.fromString (RTSymbols.getSym (#1 f)), NONE,
			       [],(*@TODO: consider non-empty attributes *)
			       assemblystatic, (xs,e))
  end

  fun makeGlobField ((x,ty):MILTerm.TypedVar) =  
  let
    (*@NOTE: this assumes the ref is an (unflattened) ML ref *)
    val rep = tyToRep env ty (*  (hd (#1 (valOf (MILTy.fromRefty ty)))) *)
  in
    {
      name = Id.fromString (RTSymbols.getSym (#1 x)),
      flags = assemblystatic,
      ty = rep,
      value = NONE
    } : RTResult.Field
  end

  (* Must be the last thing to be compiled as nones are required *)
  fun compileClinit e =
    let
      val method as { name, override, attributes, stack, locals, resty, args, flags, code } =
        CompileMethod.compile env 
        (RuntimeNames.classConstructor, NONE, [] (*@TODO: consider non-empty attributes *), assemblystatic, ([],e))
      val (noneinstrs, nonestack) = CompileOnePlus.makeNones ()
    in
      if (case noneinstrs of I.Empty => true | _ => false) andalso (case e of Triv [] => true | _ => false)
      then []
      else [{ name = name,  override = NONE,
	      attributes = [], (*@TODO: consider non-empty attributes *)
	      stack = Int.max(nonestack,stack),
	      locals = locals, resty = resty, args = args, flags = flags,
              code = I.Join(noneinstrs, code) }]
    end

  (* the initMethod is used to force initialization via the class constructor *)
  val initMethod = {name = RepNames.globalInitPrim, 
                    override = NONE,
		    attributes = [], 
		    stack = 0,
		    locals = [], resty = NONE, args = [], 
		    flags = Symbol.Set.add(assemblystatic,entrypoint) (*@TODO: review*), 
		    code = I.Single I.ret }

  (* the initMethod is used to force initialization via the class constructor *)
  val gcSafePointMethod = {name = CompileOps.GCSafePointMethod, 
                           override = NONE,
                           attributes = [], 
                           stack = 0,
                           locals = [], resty = NONE, args = [], 
                           flags = assemblystaticnoinlining, 
                           code = I.Single I.ret }



  val globmethods = 
      PrintManager.process ("Compiling global functions",false)
      (fn () => Var.Map.listItems (Var.Map.map compileFun globfuns))
  val clinitMethod = 
      PrintManager.process ("Compiling global methods",false)
      (fn () => compileClinit clinit)
  val methods = clinitMethod @ (initMethod::gcSafePointMethod::globmethods)
  val fields = 
    { name = RepNames.exnLocMessage,
      flags = assemblystatic,
      ty = RTOps.string,
      value = NONE } ::      
    { name = RepNames.exnClassCount,
      flags = assemblystatic,
      ty = RTOps.int,
      value = NONE } ::      
      (PrintManager.process("Compiling none fields",false)
       CompileOnePlus.makeNoneFields)
       @
      (PrintManager.process("Compiling global fields",false)
       (fn () => map makeGlobField (Var.Map.listItems globvars)))
in
  (*@FUTURE: in principle, we could omit this class when there are no global members,
    but we currently rely on the presence of the dummy initMethod *)
  [{
     name = RTOps.classRepToLongid (globs ()),
     attributes = [], (*@TODO: consider non-empty attributes *)
     super = RTOps.object,
     interfaces = [],
     flags = private,
     fields = fields,
     methods = methods
  }] : RTResult.Class list
end

end (* of local open *)
end (* of struct *)
