(*======================================================================*)
(* Types required by scope analysis.                                    *)
(*======================================================================*)
structure ScopeTypes =
struct

(*----------------------------------------------------------------------*)
(* Bindings of various kinds that can be floated or hoisted		*)
(*----------------------------------------------------------------------*)
datatype Binding =
  ValBind of MILTerm.BoundVar * MILTerm.Val
| CmpBind of MILTerm.TypedVar list * MILTerm.Cmp
| FunBind of (Var.Var * MILTy.Kind) list * MILTerm.FunKind * MILTerm.FunDef

(*----------------------------------------------------------------------*)
(* The result of scope analysis and the input to the scope changer.	*)
(*----------------------------------------------------------------------*)
type Info =
{
  (* Bindings to be hoisted inwards, to just inside the change of scope *)
  (* The paths are truncated back to the nearest enclosing function/method *)
  hoistedbindings : Binding MILPathOps.Map.map,

  (* Bindings to be floated outwards, to just outside a function/method *)
  floatedbindings : Binding list Var.Map.map,

  (* Bindings to be replaced by a simple value binding *)
  replacedbindings : MILTerm.Val Var.Map.map,

  (* All variables whose bindings should be hoisted *)
  hoistedvars : Var.Set.set,

  (* All variables whose bindings should be floated *)
  floatedvars : Var.Set.set,

  (* Function kinds to be changed *)
  funkinds : MILTerm.FunKind Var.Map.map
}

(*----------------------------------------------------------------------*)
(* Given a path, remove everything outside the innermost function/method*)
(*----------------------------------------------------------------------*)
fun strip path =
let
  fun s (acc,[]) = rev acc
    | s (acc,(item as MILPath.LetFun _)::items) = rev (item::acc)
    | s (acc,item::items) = s (item::acc, items)
in
  s ([], path)
end

end

