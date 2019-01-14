(*======================================================================*)
(* Pretty-print the results of closure conversion.			*)
(*======================================================================*)
structure ClosConvPretty :> CLOSCONVPRETTY =
struct

infixr 6 ++		(* Concatenation *)
infixr 6 +/		(* Concatenation with space *or* newline *)

open NewPretty MILPretty

fun pFunDef (f': Var.Var, (tyvars, f : MILTerm.BoundVar, tabs : MILTerm.TAbstr, cty)) =
  bold (text "Global function ") ++ text (Var.toString f') ++ text " " ++ italic (text (Var.toString (#1 f))) ++ text ":" ++ line ++ pTAbstr tabs

(*
fun pClosDef { fvtys, meths } =
     Pretty.simpleVec "\n" (fn (v,i) => Var.toString v ^ " : " ^Int.toString i)
     (Var.Map.listItemsi appmeths) ^ "\n");
*)

fun pResult ({fundefs, globvars, classdefs, closdefs, appmeths, clinit, methtys, bindeffects } : ClosConvTypes.Result) =
(
  rule ++ text "Global functions:" ++ line ++  
    stack (map pFunDef (Var.Map.listItemsi fundefs)) ++  
  rule ++ text "Closure classes:" ++ line ++
(*     stack (map pAppMeth appmeths) ++  *)
  rule ++ text "Class constructor:" ++
    pCmp clinit ++
  rule
)

fun dump r = if Debug.html() then NewPretty.prettyHtml 120 Debug.print (pResult r) else NewPretty.pretty 80 Debug.print (pResult r)

end

