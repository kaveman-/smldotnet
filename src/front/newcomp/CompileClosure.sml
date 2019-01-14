(*======================================================================*)
(* Compile a closure class.                                             *)
(*======================================================================*)
structure CompileClosure :> COMPILECLOSURE =
struct

local 
  open CompileEnv CompileFixedOps CompileOps TyRep
  structure I = RTInstrs
in

fun compile env (i, { fvtys, meths }) =
let

  val closrep = tyToRep env (MILTy.closure (SOME i, []))
  fun compileApp (appmeth, { tyvars, fvtys, fundef = (boundvar, (xs, e), cty) }) =
    let
      val env = envPlusTyVars env tyvars
    in
      CompileMethod.compile env 
        (if Controls.get useOverrides then Id.fromString (RTSymbols.defineSym boundvar) else RepNames.appMethod appmeth,
         if Controls.get useOverrides then SOME (RepNames.appMethod appmeth) else NONE,
	 [], (*@TODO consider non-empty attributes *)
	 assemblyfinal,
	 ((boundvar,MILTy.closure(SOME i, fvtys))::xs,e))
    end
in
  {
    name = RTOps.classRepToLongid closrep,
    attributes = [],
    flags = privatesealed,
    super = tyToRep env (MILTy.closure (NONE,[])),
    interfaces = [],
    fields = makeProdFields assembly (map (tyToRep env) fvtys),
    methods = makeInitMethod 
                   (tyToRep env (MILTy.closure (SOME i, fvtys)), 
                    tyToRep env (MILTy.closure (NONE, [])),
                    ListOps.mapi (fn (i,ty) => (RepNames.argLabel i, tyToRep env ty))
                                 fvtys, [])
              :: (map compileApp (IntMap.listItemsi meths))
  }
end

(*----------------------------------------------------------------------*)
(* Create the single class definition used for all functions		*)
(*----------------------------------------------------------------------*)
fun makeTopFun apps =
let
  val toprep = tyToRep CompileEnv.empty (MILTy.closure(NONE,[]))
in
  {
    name = RTOps.classRepToLongid toprep,
    attributes = [],
    super = RTOps.object,
    interfaces = [],
    fields = [],
    flags = private,
    methods = makeInitMethod (toprep, RTOps.object, [], []) ::
    ListOps.mapi (fn (i, funty) =>
    let
      val SOME (argtys, cty) = MILTy.fromArrow funty
      val (_, restys) = MILTy.fromCmp cty
      val argreps = map (tyToRep CompileEnv.empty) argtys
      val resty = OptionOps.hd restys
      val resrep = Option.map (tyToRep CompileEnv.empty) resty
    in
      {
        name = RepNames.appMethod i, 
        override = NONE,
	attributes = [], (*@TODO consider non-empty attributes *)
        flags = assembly,
        resty = resrep,
        args = map (fn rep => (Id.fromString "", rep)) argreps,
        code = I.fromList [I.ldnull, I.throw],
        stack = 1,
        locals = []
      }
    end) apps
  }
end

end (* of local open *)
end (* of struct *)
