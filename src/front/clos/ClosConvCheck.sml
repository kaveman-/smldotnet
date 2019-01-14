(*======================================================================*)
(* Type check and census check the results of closure conversion.	*)
(*======================================================================*)
structure ClosConvCheck :> CLOSCONVCHECK =
struct

val [checkTypes, checkCensus] = 
    map (Controls.add false) ["clos.type", "clos.census"]

fun check 
  ( {fundefs, globvars, classdefs, closdefs, appmeths, clinit,methtys,...} 
  : ClosConvTypes.Result) =
let
  val checkTypes = Controls.get checkTypes

  val globenv = Var.Map.map (fn (_,ty) => ty) globvars
  val funenv = Var.Map.empty

  fun checkFun (tyvars, f:MILTerm.BoundVar, (xs, e) :MILTerm.TAbstr, cty) =
    ((*if checkTypes then ignore (TypeCheck.checkAll
      { kindenv = Var.extend(Var.Map.empty, tyvars), 
        tyenv = Var.extend(globenv, map (fn ((x,sym),ty) => (x,ty)) xs), 
        funenv = funenv } e) else (); *) (#1 f::map (#1 o #1) xs,e))

(*
  fun checkClosMeth (i, { tyvars, fvtys, fundef = (f, (xs,e), cty) }) =
    ((* if checkTypes then ignore (TypeCheck.checkAll
      { kindenv = Var.extend(Var.Map.empty, tyvars), 
        tyenv = Var.Map.insert(Var.extend(Var.Map.empty, map (fn ((x,sym),ty) => (x,ty)) xs), f, 
          MILTy.closure (SOME i, fvtys)), 
        funenv = funenv } e) else (); *)
     (map (#1 o #1) xs,e))
*)

(*
  fun checkClos (i, { fvtys, meths }) =
    (map checkClosMeth (IntMap.listItemsi meths))
*)

  fun checkMethodDef ((_,_,_,_,_,SOME (m,(xs,e))) : MILTerm.MethodInfo) = [(map #1 xs,e)]
    | checkMethodDef (_,_,_,_,_,NONE) = []

  fun checkClassDef (_,_,_,methods) =
    List.concat (map checkMethodDef  methods)
in
  if checkTypes orelse Controls.get checkCensus
  then
  let
    val (varss:Var.Var list list,terms) = ListPair.unzip (map checkFun (Var.Map.listItems fundefs) @ 
(*                 List.concat (ListOps.mapi checkClos closdefs) @ *)
                List.concat (map checkClassDef classdefs) @ 
                [([],clinit)])
    val vars = List.concat varss
  in
    if checkTypes then ignore (TypeCheck.checkAll
     { tyenv = globenv, 
      kindenv = Var.Map.empty, funenv = funenv } clinit)
    else ();
    PrintManager.dump checkCensus (Census.checkCmps (vars, terms))
  end
  else ()
end

end

