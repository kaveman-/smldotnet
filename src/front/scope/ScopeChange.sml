(*======================================================================*)
(* Float or hoist value, computation and function bindings.		*)
(* Input: ccMIL								*)
(* Output: ccMIL							*)
(*======================================================================*)
structure ScopeChange :> SCOPECHANGE =
struct

local 
  open MILTerm ScopeTypes
in

(*----------------------------------------------------------------------*)
(* Transform the computation term e.					*)
(*----------------------------------------------------------------------*)
fun transform info tyenv term  =
let
  val { hoistedbindings, floatedbindings, replacedbindings, 
        floatedvars, hoistedvars, funkinds
      } = info

(*----------------------------------------------------------------------*)
(* Translate and transform a binding					*)
(*----------------------------------------------------------------------*)
fun transBind env (b,e) =
case b of
  ValBind(x, v) => 
  LetVal(x, v, e)

| CmpBind(xs, e') =>
  Let(transCmp env e', (xs, e))

| FunBind(tyvars, kind, Fun(f, (xs, e'))) =>
  LetFun(tyvars, kind, Fun(f, (xs,transNewScope 
    (MILPath.LetFun { var = #1 f, kind = SOME kind, recursive = false }) 
    env e')), e)

| FunBind(tyvars, kind, RecFun defs) =>
  LetFun(tyvars, kind, RecFun(map (fn (f,g,(xs,e'),cty) =>
    (f,g,(xs, transNewScope (MILPath.LetFun { var = #1 f, 
      kind = SOME kind, recursive = true }) env e'),cty)) defs), e)

(*----------------------------------------------------------------------*)
(* Enter a new scope, hoist bindings to this scope, transform term.     *)
(*----------------------------------------------------------------------*)
and transNewScope' env e =
let
  val e = transCmp env e
in
  foldl (transBind env) e (MILPathOps.Map.find(hoistedbindings, env))
end

and transNewScope (item as MILPath.LetFun _) env e = transNewScope' [item] e
  | transNewScope item env e = transNewScope' (item::env) e

(*----------------------------------------------------------------------*)
(* We're about to enter a function or method defn; insert floated       *)
(* bindings if any exist.						*)
(*----------------------------------------------------------------------*)
and floatInto env (vars : MILTerm.BoundVar list) e =
    foldl (fn (var,e) =>    
      case Var.Map.find(floatedbindings, #1 var) of
        NONE => e
      | SOME bs => foldl (transBind env) e bs) e vars

(*----------------------------------------------------------------------*)
(* Transforms a computation term.					*)
(*----------------------------------------------------------------------*)
and transCmp env ce =
let
  fun transCases itemf (v, cases, optdefault, cty) =
    let 
      val defresult = Option.map (transNewScope (itemf NONE) env) optdefault
      fun transCase (i, (xs, e)) = 
        (i, (xs, transNewScope (itemf (SOME i)) env e))
      val cases = map transCase cases
    in
      (v, cases, defresult, cty)
    end

in
  case ce of

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
  Let(e1, (xs, e2)) =>
  (* It's going to be hoisted in or has been floated out *)
  if List.exists (fn ((x,_),_) => 
    Var.Set.member(hoistedvars, x) orelse Var.Set.member(floatedvars, x)) xs
  then transCmp env e2
  else
  let
    val e1 = transCmp env e1
    val e2 = transCmp env e2
  in
    Let(e1, (xs, e2))
  end
    
(*......................................................................*)
(* Value bindings                                                       *)
(*......................................................................*)
| LetVal(x, v, e) =>
  (* It's going to be hoisted in *)
  if Var.Set.member(hoistedvars, #1 x)
  then transCmp env e
  else

  (* It's already been floated out, but we may want to re-abstract *)
  if Var.Set.member(floatedvars, #1 x)
  then
    let
      val e = transCmp env e
    in
      case Var.Map.find(replacedbindings, #1 x) of
        NONE => e
      | SOME v => LetVal(x, v, e)
    end
  else LetVal(x, v, transCmp env e)

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  Encap (transCmp env e)

(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case a =>
  Case (transCases MILPath.CaseCon a)

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon a =>
  CaseSCon (transCases MILPath.CaseSCon a)
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| TypeCase a =>
  TypeCase (transCases MILPath.TypeCase a)

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(e, handlers, (xs, otherwise)) =>
  let
    val e = transCmp env e
    val otherwise = transCmp env otherwise
  in
    TryLet(e, map (fn (xs,e) => (xs, transCmp env e)) handlers,
      (xs, otherwise))
  end

(*......................................................................*)
(* Internal class definition						*)
(*......................................................................*)
| LetClass(classname,classinfo, fields, methods, e) =>
  let
    (* Collect the 'function' variables for non-abstract methods *)
    val methodvars = 
      List.mapPartial (fn (_,_,_,_,_,SOME(x,_)) => SOME x | _ => NONE) methods

    (* Transform each method *)
    fun transMethod (name, atts, mods, tys, tyopt, optabs) =
        (name, atts, mods, tys, tyopt, 
          Option.map (fn (f : MILTerm.BoundVar, (xs, e)) =>
            (f, (xs, transNewScope 
                  (MILPath.LetFun { var = #1 f, kind = NONE, recursive = false}) 
                  env e))) optabs)

    val methods = map transMethod methods

    (* Transform the continuation *)
    val e = transCmp env e
  in
    (* Float definitions out *)
    floatInto env methodvars 
    (LetClass(classname,classinfo, fields, methods, e))
  end

(*......................................................................*)
(* Recursive functions.                                                 *)
(*......................................................................*)
| LetFun(tyvars, kind, RecFun recbinds, e) =>
  let
    val representative = #1 (#1 (hd recbinds))
    val kind = getOpt(Var.Map.find(funkinds, representative), kind)
  in

    (* It's going to be hoisted in *)
    if Var.Set.member(hoistedvars, representative)
    then transCmp env e
    else

    (* It's already been floated out but we may want to re-abstract *)
    if Var.Set.member(floatedvars, representative)
    then
      let
        val e = transCmp env e
        val e = foldl (fn ((f,_,_,_),e) =>
          case Var.Map.find(replacedbindings, #1 f) of
            NONE => e
          | SOME v => LetVal(f, v, e)) e recbinds
      in
        e
      end

    else
    let
      fun transDef (f : MILTerm.BoundVar , g, (xs, e), cty) =
          let
            val e = transNewScope (MILPath.LetFun { var = #1 f, kind = SOME kind, 
              recursive = true }) env e
          in
            (f, g, (xs, e), cty)
          end

      val newdef = RecFun (map transDef recbinds)

      val e = transCmp env e
    in
      floatInto env (map #1 recbinds) (LetFun(tyvars, kind, newdef, e))
    end
  end

(*......................................................................*)
(* Non-recursive function defn.                              		*)
(*......................................................................*)
| LetFun(tyvars, kind, Fun (f, (typedvars,e1)), e2) =>
  let
    val kind = getOpt(Var.Map.find(funkinds, #1 f), kind)
  in
  (* It's going to be hoisted in *)
  if Var.Set.member(hoistedvars, #1 f) 
  then transCmp env e2
  else

  (* It's already been floated out but we may want to re-abstract *)
  if Var.Set.member(floatedvars, #1 f)
  then
    let
      val e2 = transCmp env e2
    in
      case Var.Map.find(replacedbindings, #1 f) of
        NONE => e2
      | SOME v => LetVal(f, v, e2)
    end
  else 
  let
    val e1 = transNewScope (MILPath.LetFun { var = #1 f, recursive = false, 
      kind = SOME kind } ) env e1
    val newdef = Fun (f, (typedvars, e1))
    val e2 = transCmp env e2
  in
    floatInto env [f] (LetFun(tyvars, kind, newdef, e2))
  end
  end

(*----------------------------------------------------------------------*)
(* Everything else is left alone					*)
(*----------------------------------------------------------------------*)
| _ =>
  ce

end

val term = transNewScope' [] term
(*@HACK: is this renaming really necessary? we need it to compile bmark/raytrace  *)
val term = PrintManager.process("Renaming term", false) (fn () => Census.renameCmp term) 
in
   term
end

end (* of local open *)
end (* of struct *)

