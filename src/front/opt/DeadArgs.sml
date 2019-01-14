(*======================================================================*)
(* Remove dead arguments from functions                                 *)
(*======================================================================*)
structure DeadArgs :> TRANSFORMER =
struct


local 
  open MILTerm 
  fun isLive ((x,_),ty) = not (Census.isDead x)
  val stats = Controls.add false "deadargs.stats"
in

fun transform tyenv e =
let

fun getVar (Var x) = SOME x
  | getVar (TApp(v, ty)) = getVar v
  | getVar _ = NONE

(*----------------------------------------------------------------------*)
(* Do the translation on computation terms.				*)
(*----------------------------------------------------------------------*)
fun t env e =
let
  val tt = t env
  fun tabs (xs,e) = (xs, tt e)
  fun tcases (v, cases, e, cty) = (v, map (fn (i,a) => (i,tabs a)) cases, Option.map tt e, cty)
in
  case e of
  App(v, vs) =>
  (case getVar v of
    NONE => e
  | SOME x =>
    (case Var.Map.find(env, x) of
      NONE => e
    | SOME live => App(v, List.mapPartial (fn (v,true) => SOME v | (v,false) => (Census.addVal(v,~1); NONE)) (ListPair.zip (vs,live)))))

| Encap e => Encap (tt e)
| Let(e, a) => Let(tt e, tabs a)
| LetVal(x, v, e) => LetVal(x, v, tt e)
| Case a => Case (tcases a)
| CaseSCon a => CaseSCon (tcases a)
| TypeCase a => TypeCase (tcases a)
| TryLet(e, h, a) => TryLet(tt e, map tabs h, tabs a)
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun tmethod (method as (name, atts, mods, tys, tyopt, optabs)) =
        case optabs of 
          NONE => method
        | SOME (f, a) => (name, atts, mods, tys, tyopt, SOME (f, tabs a))
  in
    LetClass(classname, classinfo, fields, map tmethod methods, tt e)
  end
  
| LetFun(tyvars, AnyFun, Fun(f, a), cont) => LetFun(tyvars, AnyFun, Fun(f, tabs a), tt cont)
| LetFun(tyvars, AnyFun, RecFun defs, cont) =>
  LetFun(tyvars, AnyFun, RecFun (map (fn (f,g,a,cty) => (f,g,tabs a,cty)) defs), tt cont)

| LetFun(tyvars, kind, Fun(f, (xs, e)), cont) =>
  let
    val _ = app (fn x => if isLive x then () else Stats.inc "dead arguments") xs
  in
    LetFun(tyvars, kind, Fun(f, (List.filter isLive xs, tt e)),
      t (Var.Map.insert(env, #1 f, map isLive xs)) cont)
  end

| LetFun(tyvars, kind, RecFun defs, cont) =>
  let
    val _ = app (fn (_,_,(xs,e),_) => app (fn x => if isLive x then () else Stats.inc "dead arguments") xs) defs
    val denv = foldl (fn ((f,g,(xs,e),_),env) => Var.Map.insert(env, #1 g, map isLive xs)) env defs
    val cenv = foldl (fn ((f,g,(xs,e),_),env) => Var.Map.insert(env, #1 f, map isLive xs)) env defs
  in
    LetFun(tyvars, kind, RecFun (map (fn (f,g,(xs,e),cty) => (f,g,(List.filter isLive xs, t denv e),cty)) defs),
      t cenv cont)
  end

| _ => e

end

val _ = Stats.clear ()
val e = t Var.Map.empty e

in
  if Controls.get stats then Stats.dump() else ();
  e
end

end (* of local open *)

val _ = Opts.add "deadargs" (transform, "Removing dead arguments from functions")

end (* of struct *)

