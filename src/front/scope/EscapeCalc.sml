(*======================================================================*)
(* Do a very simple escape analysis of the whole program, enough to     *)
(* determine which known functions are `local', which are `first-order' *)
(* and which are `higher-order'.                                        *)
(*======================================================================*)
structure EscapeCalc :> SCOPECALC =
struct

local 
  open MILTerm ScopeTypes
in

val [knownFunctions, localFunctions, hoistLocalFun] = 
    map (Controls.add true) 
    ["knownFunctions", "localFunctions", "hoistLocalFun"]

fun calc tyenv e =
let

(*----------------------------------------------------------------------*)
(* Environment is a scope and a flag indicating tail 			*)
(*----------------------------------------------------------------------*)
  type Env = MILPath.Path * bool

  fun envPlusScope (path, tail) item = (item::path, tail)
  fun envNonTail (path, tail) = (path, false)
  fun envTail (path, tail) = (path, true)
  fun scopeOf (path, tail) = path
  fun isTail (path, tail) = tail
  val initialEnv = ([], true)

(*----------------------------------------------------------------------*)
(* Map from function variables to their `kind'.                         *)
(*----------------------------------------------------------------------*)
  val funkinds = ref (Var.Map.empty : FunKind Var.Map.map)
  val newfunkinds = ref (Var.Map.empty : FunKind Var.Map.map)

(*----------------------------------------------------------------------*)
(* Scopes of applications; only bother if they are tail positions	*)
(*----------------------------------------------------------------------*)
  val appscopes = ref (Var.Map.empty : MILPath.Path list Var.Map.map)

(*----------------------------------------------------------------------*)
(* Bindings that should be hoisted inwards.				*)
(*----------------------------------------------------------------------*)
  val hoistedbindings =
    ref (MILPathOps.Map.empty : Binding MILPathOps.Map.map)

(*----------------------------------------------------------------------*)
(* Variables whose bindings get hoisted            			*)
(*----------------------------------------------------------------------*)
  val hoistedvars = ref Var.Set.empty

(*----------------------------------------------------------------------*)
(* Higher-order use of a variable					*)
(*----------------------------------------------------------------------*)
fun addHigherOrder x =

(*......................................................................*)
(* Add to function kinds        					*)
(*......................................................................*)
  case Var.Map.find(!funkinds, x) of
    NONE => ()
  | SOME AnyFun => ()
  | _ => funkinds := Var.Map.insert(!funkinds, x, AnyFun)
 
(*----------------------------------------------------------------------*)
(* Known application							*)
(*----------------------------------------------------------------------*)
fun addApp env x =
case Var.Map.find(!funkinds, x) of
   NONE => ()
 | SOME AnyFun => ()
 | SOME KnownFun => ()


| SOME LocalFun =>
  if isTail env then 
  (case Var.Map.find(!appscopes, x) of
    NONE => appscopes := Var.Map.insert(!appscopes, x, [scopeOf env])
  | SOME ss => appscopes := Var.Map.insert(!appscopes, x, scopeOf env :: ss))
  else 
  funkinds := Var.Map.insert(!funkinds, x, KnownFun)

fun isUnknown x = 
  case Var.Map.find(!funkinds, x) of SOME AnyFun => true | _ => false

fun isKnown x =
  case Var.Map.find(!funkinds, x) of SOME KnownFun => true | _ => false

fun isLocal x =
  case Var.Map.find(!funkinds, x) of SOME LocalFun => true | _ => false

fun normaliseScope locals vars = 
  List.filter 
  (fn MILPath.LetFun { kind, ... } => kind <> SOME LocalFun | _ => false) 
    (map 
    (fn MILPath.LetFun { kind = SOME kind, var, recursive } =>
        MILPath.LetFun { 
    kind = SOME (getOpt(Var.Map.find(!newfunkinds, var),
      if List.exists (fn var' => Var.eq(var,var')) locals then LocalFun else kind)), 
      var = var,
          recursive = recursive } | item => item)
    vars)

val normaliseScope' = normaliseScope []

(*----------------------------------------------------------------------*)
(* Add a set of function variables.				        *)
(* In order to determine the function's kind, the uses and definition   *)
(* must have been analysed already.                                     *)
(*----------------------------------------------------------------------*)
fun addFunVars env (tyvars,kind,def) (fs : Var.Var list, gs : Var.Var list) =
let
  fun addnew newkind =
    newfunkinds := foldl (fn (x,kinds) => Var.Map.insert(kinds, x, newkind))
    (!newfunkinds) (fs @ gs)

  fun addold kind =
    funkinds := foldl (fn (x,kinds) => Var.Map.insert(kinds, x, kind))
    (!funkinds) (fs @ gs)
  fun showVars vs = List.app (fn v => (Debug.print " "; Debug.print (Var.toString v))) vs
in
  (* If any of the uses are higher-order then the whole defn is *)
  if List.exists isUnknown fs orelse List.exists isUnknown gs 
  then 
    if kind <> AnyFun 
    then ((* Debug.print ("\nInvalid local/known function used as unknown function"); *)
          showVars fs; 
          addnew AnyFun; addold AnyFun)
    else addold AnyFun
  else

  (* If any of the uses are non-local then the whole defn is *)
  if List.exists isKnown fs orelse List.exists isKnown gs 
  then 
    if kind=LocalFun 
    then ((* Debug.print "\nInvalid local function used as known function";  *)
          showVars fs; 
          addnew KnownFun; addold KnownFun)
    else 
    if kind=AnyFun andalso Controls.enabled knownFunctions 
    then (addnew KnownFun; addold KnownFun)
    else addold kind
  else

  (* Otherwise potentially they're all local, so now check the scopes *)
  let
    val defscope = normaliseScope fs (scopeOf env)

    fun getScope v = 
      if Var.isDummy v then []
      else
      case Var.Map.find(!appscopes, v) of
        NONE => []
      | SOME scopes => 
        let val scopes = map (normaliseScope fs) scopes
        in
          scopes
        end
  
    val fscopes = List.concat (map getScope fs)
    val gscopes = List.concat (map getScope gs)

    fun sameScope [] = true
      | sameScope [_] = true
      | sameScope (scope1::(rest as (scope2::_))) =
        MILPathOps.eq (scope1, scope2) andalso sameScope rest

      (* All recursive applications must be in same scope as the definition *)
      val gsame = sameScope (defscope::gscopes)

      (* All other applications must share the same scope *)
      (* We could hoist (in which case, omit defscope) *)
      val fsame = sameScope (defscope::fscopes)
  in
    if fsame andalso gsame 
    then
      if kind=LocalFun then () 
      else if Controls.enabled localFunctions then addnew LocalFun else ()
    else 
    if gsame andalso sameScope fscopes 
    then 
      if kind=LocalFun then ()
      else if Controls.enabled hoistLocalFun 
      then 
      (
        addnew LocalFun; 
        hoistedvars := Var.Set.addList(!hoistedvars, fs @ gs);
        hoistedbindings := 
          MILPathOps.Map.insert(!hoistedbindings, 
            strip (normaliseScope' (hd fscopes)),
            FunBind (tyvars, LocalFun, def))
      ) 
      else ()
    else 
      if kind=LocalFun 
      then ((* Debug.print "\nInvalid local function in separated scopes ";  *)
	    showVars fs; 
	    addnew KnownFun; addold KnownFun)
      else 
      if kind=AnyFun andalso Controls.enabled knownFunctions 
      then (addnew KnownFun; addold KnownFun)
      else addold KnownFun
  end
end

(*----------------------------------------------------------------------*)
(* Analyse a value term.						*)
(* If app=true, the value is on the lhs of an "application" (so v is in *)
(* v vs or !v or v := v') and should not be considered higher-order.	*)
(*----------------------------------------------------------------------*)
fun flowVal app (env : Env) v =
case v of
  Var x => if app then addApp env x else addHigherOrder x
| (SCon _) => ()
| Fold(v,_) => flowVal app env v
| Unfold v => flowVal app env v
| TAbs(_,v) => flowVal app env v
| TApp(v,_) => flowVal app env v
| As(v,_) => flowVal app env v
| Proj(_, _, v) => flowVal false env v
| Inj(_, _, vs,_) => List.app (flowVal false env) vs
| ExCon(_, vs) => List.app (flowVal false env) vs
| Tuple vs => List.app (flowVal false env) vs

  
(*----------------------------------------------------------------------*)
(* Analyse a computation term and return the transformed term.		*)
(* [The only changes are to letfun constructs]				*)
(*----------------------------------------------------------------------*)
fun flowCmp (env : Env) e =
let
  fun flowCases itemf (v, cases, optdef, cty) =
    let 
      fun flowCase (i, (xs, e)) =
          flowCmp (envPlusScope env (itemf (SOME i))) e
    in
      flowVal false env v;
      app flowCase cases;
      ignore (Option.map (flowCmp (envPlusScope env (itemf NONE))) optdef)
    end
in
  case e of

  App(v, vs) =>
  (flowVal true env v; app (flowVal false env) vs)

| Let(e, (xs,e')) =>
  (flowCmp (envNonTail env) e; flowCmp env e')
    
| LetVal(x, v, e) =>
  (flowVal false env v; flowCmp env e)

| Triv vs => app (flowVal false env) vs
| Special(_, vs, _) => app (flowVal false env) vs

| Encap e =>
  flowCmp env e

| Case a =>
  flowCases MILPath.CaseCon a

| CaseSCon a =>
  flowCases MILPath.CaseSCon a
      
| TypeCase a =>
  flowCases MILPath.TypeCase a

| Throw(v, tys, loc) =>
  flowVal false env v
  
| TryLet(e0, handlers, (_, e1)) =>
  (flowCmp (envNonTail env) e0; 
   app (flowCmp env o #2) handlers; 
   flowCmp env e1)

| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun flowMethod (name, atts, mods, tys, tyopt, optabs) =
          ignore (Option.map (fn ((f,_), (xs,e)) =>  flowCmp 
            (envTail (envPlusScope env (MILPath.LetFun { var = f, 
              recursive = false, kind = NONE }))) e) optabs)
  in
    app flowMethod methods;
    flowCmp env e
  end

| LetFun(tyvars, kind, funbind as RecFun recbinds, e) =>
  let
    val fs = map (#1 o #1) recbinds
    val gs = map (#1 o #2) recbinds

    fun flowDef ((f,_), _, (xs, e) : MILTerm.TAbstr, cty) =
    let
      val defnenv = envPlusScope env (MILPath.LetFun { var = f, 
        recursive = true, kind = SOME kind })
      val defnenv = if MILTermOps.isLocal kind
                    then defnenv else envTail defnenv
    in
      flowCmp defnenv e
    end

(*@crusso *)
    val initialKind = if null tyvars then LocalFun else KnownFun

    val _ = funkinds := foldl 
      (fn (f,m) => Var.Map.insert(m, f, initialKind)) (!funkinds) (fs@gs)
    val _ = app flowDef recbinds
    val _ = flowCmp env e
    val newkind = addFunVars env (tyvars,kind,funbind) (fs, gs)
  in
    ()
  end

| LetFun(tyvars, kind, funbind as Fun ((f,_), (xs, e1)), e2) =>
  let
    val defnenv = envPlusScope env (MILPath.LetFun { var = f,
      recursive = false, kind = SOME kind} )
    val defnenv = 
      if MILTermOps.isLocal kind then defnenv else envTail defnenv

(*@crusso *)
    val initialKind = if null tyvars then LocalFun else KnownFun

    val _ = funkinds := Var.Map.insert(!funkinds, f, initialKind)
    val _ = flowCmp defnenv e1
    val _ = flowCmp env e2
    val newkind = addFunVars env (tyvars,kind,funbind) ([f], [])
  in
    ()
  end

end

in
  Controls.reset ();
  flowCmp initialEnv e;
  Controls.printCounts PrintManager.print;
  let
    val info =   
    {
      funkinds = !newfunkinds,
      floatedbindings = Var.Map.empty,
      replacedbindings = Var.Map.empty,
      hoistedbindings = !hoistedbindings,
      hoistedvars = !hoistedvars,
      floatedvars = Var.Set.empty
    } : ScopeTypes.Info
  in
    ScopePretty.dump info;
    info
  end
end


end (* of local open *)
end (* of struct *)

