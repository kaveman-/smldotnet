(*======================================================================*)
(* Gather liveness and stackability information for local variable 	*)
(* allocation            						*)
(*======================================================================*)
structure Liveness :> LIVENESS =
struct

open MILTerm MILTermOps

val stackVals = Controls.add true "codegen.stackVals"
val stackCmps = Controls.add false "codegen.stackCmps"
val repeatCalc = Controls.add false "codegen.repeatCalc"

(* Get the variable hiding in a value *)
fun getValVar (Var x) = SOME x
  | getValVar (TApp(v,_)) = getValVar v
  | getValVar (TAbs(_,v)) = getValVar v
  | getValVar (Unfold v) = getValVar v
  | getValVar (Fold(v,_)) = getValVar v
  | getValVar _ = NONE

(* Stackability *)
local
  val stackable = ref Var.Set.empty
  val showStackable = Controls.add false "codegen.showStackable"
in
  fun isStackable x = Var.Set.member(!stackable, x)
  fun addStackable x = stackable := Var.Set.add(!stackable, x)  
  fun openStackable () = stackable := Var.Set.empty
  fun closeStackable () =
  (
    Stats.add("stackable bindings/method", Var.Set.numItems (!stackable));
    if Controls.get showStackable then Debug.print ("\nSTACKABLE VARS: " ^ Pretty.simpleVec "," Var.toString (Var.Set.listItems (!stackable))) else ()
  )
end

(* Effect information and variable linearity *)
local
  val useEffects = Controls.add true "codegen.useEffects"
  val bindeffects = ref (Var.Map.empty : Effect.Effect Var.Map.map)
in
  fun isLinear x = Census.getVar x = 1
  fun isLinearNoEffects x = 
    isLinear x andalso 
    Controls.get useEffects andalso 
    (case Var.Map.find(!bindeffects, x) of NONE => false | SOME eff => Effect.sub(eff, Effect.partial))
  fun initEffectInfo m = bindeffects := m
end

(* Liveness information *)
local
  val changed = ref false
  val bindLive = ref (Var.Map.empty : Var.Set.set Var.Map.map)
  val blockLive = ref (Var.Map.empty : Var.Set.set Var.Map.map)
  val showLive = Controls.add false "codegen.showLive"
  fun liveToString live =
    Pretty.simpleVec "\n" (fn (f, xs) => "  " ^ Var.toString f ^ ":" ^
      Pretty.simpleVec "," Var.toString (Var.Set.listItems xs)) (Var.Map.listItemsi live)
in
  fun openLive() = (bindLive := Var.Map.empty; blockLive := Var.Map.empty)
  fun findLive x = Var.Map.find(!bindLive, x)
  fun findBlockLive f = Var.Map.find(!blockLive, f)
  fun addBindLive((x,_), xs) = bindLive := Var.Map.insert(!bindLive, x, xs)
  fun addBlockLive((f,_), xs) = 
  case Var.Map.find(!blockLive, f) of
    NONE => (changed := true; blockLive := Var.Map.insert(!blockLive, f, xs))
  | SOME ys => if Var.Set.numItems xs = Var.Set.numItems ys then () else (changed := true; blockLive := Var.Map.insert(!blockLive, f, xs))
  fun blockLiveChanged () = let val result = !changed in changed := false; result end
  fun closeLive() = 
    if Controls.get showLive 
    then 
    (
      Debug.print ("\nLIVE VARS FOR BLOCKS:\n" ^ liveToString (!blockLive));
      Debug.print ("\nLIVE VARS FOR BINDINGS:\n" ^ liveToString (!bindLive))
    )
    else ()
 
end

(* Block arguments information *)
local
  val blockinfo = ref (Var.Map.empty : Var.Var list list Var.Map.map)
in
  fun openBlockInfo() = blockinfo := Var.Map.empty  
  fun defBlock (f, xs) = blockinfo := Var.Map.insert(!blockinfo, f, map (fn _ => []) xs)
  fun useBlock(f, vs) = 
  case Var.Map.find(!blockinfo, f) of
    NONE => ()
  | SOME xss =>
    blockinfo := Var.Map.insert(!blockinfo, f, 
      ListPair.map (fn (v, xs) => case getValVar v of NONE => xs | SOME x => x::xs) (vs,xss))
  fun getBlockArgsInfo f = Var.Map.find(!blockinfo, f)
end

fun analyse e =
let
 
  (* Analyse a term to see which variable bindings can be "inlined" to their use site, hence
     using the stack rather than a local. Prerequisites:
     1. variable must be used exactly once
     2. the use site must be in the same basic block (not under a case or in another local block)
     3. if the binding is to a computation, inlined bindings must occur in the same order as
        they were bound. With some effect information, we could relax this constraint. *)
  val empty = (Var.Set.empty,[])

  fun av (a as (valvars,cmpvars)) v =
    case v of
    SCon _ => cmpvars
  | Fold(v, _) => av a v
  | Unfold v => av a v
  | TApp(v,_) => av a v
  | TAbs(_,v) => av a v
  | Proj(_,_,v) => av a v
  | As(v,_) => av a v
  | ExCon(_, vs) => avs a vs
  | Tuple vs => avs a vs
  | Inj(_, _, vs, _) => avs a vs

  | Var x =>
    (if Var.Set.member(valvars, x)
    then (addStackable x; cmpvars)
    else if isLinear x andalso isStackable x then raise Fail ("duplicate variable " ^ Var.toString x) 
    else 
    case cmpvars of
      [] => []    
    | (y::ys) => if Var.eq(x,y) then (addStackable x; ys) else cmpvars)

  and avs (a as (valvars,cmpvars)) [] = cmpvars
    | avs (a as (valvars,cmpvars)) (v::vs) = 
      let val cmpvars = avs a vs
      in av (valvars,cmpvars) v end
 
  (* Analyse a non-continuation computation term *)  
  fun ac (a as (valvars,cmpvars)) e =
    case e of
      App(v,vs) => avs a (v::vs)
    | Special(_,vs,_) => avs a vs
    | Triv vs => avs a vs
    | Throw(v,_,_) => av a v

  (* Analyse a continuation computation term *)
  fun acont (a as (valvars,cmpvars)) e =
  let fun acases (v, cases, def, cty) = (av a v; app (fn (_, (_,e)) => acont empty e) cases; 
                                            Option.app (acont empty) def)
  in
    case e of
      Case cases => acases cases
    | CaseSCon cases => acases cases
    | TypeCase cases => acases cases
    | LetVal(x, v, e) => 
      let val cmpvars = av a v
      in acont (if isLinear(#1 x) then Var.Set.add(valvars, #1 x) else valvars, cmpvars) e end
    | Let(e,([(x,ty)],e')) => 
      let val cmpvars = ac a e
      in if isLinearNoEffects(#1 x) 
         then acont (Var.Set.add(valvars, #1 x), cmpvars) e'
         else if isLinear(#1 x) 
         then acont (valvars, (#1 x)::cmpvars) e' 
         else acont (valvars, []) e' 
      end
    | Let(e,(xs,e')) => 
      (ac a e; acont (valvars,[]) e')
    | TryLet(e, h, tabs) => 
      (acont a e; app (acont empty o #2) h; acont empty (#2 tabs))
    | LetFun(_,_,def,e) => 
      (acont a e;
       case def of
          Fun (f, (xs,e)) => (defBlock(#1 f, xs); acont empty e)
        | RecFun defs => app (fn (f,g,(xs,e),cty) => (defBlock(#1 f, xs); acont empty e)) defs)
    | e => ignore (ac a e)
  end

  fun remove (s, xs:MILTerm.BoundVar list) = 
    foldr (fn ((x,_), s) => if Var.Set.member(s,x) then Var.Set.delete(s, x) else s) s xs

  fun removeVars (s, xs:Var.Var list) = 
    foldr (fn (x, s) => if Var.Set.member(s,x) then Var.Set.delete(s, x) else s) s xs

  (*......................................................................*)
  (* Live variables in a value, list of values, and non-continuation      *)
  (* computation, respectively. Recurse into stacked terms.		  *)
  (*......................................................................*)
  fun liveVal stacked acc v =
  case v of
    Var x => (case Var.Map.find(stacked,x) of NONE => Var.Set.add(acc, x) | SOME e => liveCmp stacked acc e)
  | SCon _ => acc
  | Fold(v,_) => liveVal stacked acc v
  | As(v,_) => liveVal stacked acc v
  | Unfold v => liveVal stacked acc v
  | Inj(_,_,vs,_) => liveVals stacked acc vs
  | ExCon(_,vs) => liveVals stacked acc vs
  | Tuple vs => liveVals stacked acc vs
  | Proj(_,_,v) => liveVal stacked acc v
  | TAbs(_,v) => liveVal stacked acc v
  | TApp(v,_) => liveVal stacked acc v

  and liveVals stacked acc [] = acc
    | liveVals stacked acc (v::vs) = liveVals stacked (liveVal stacked acc v) vs

  and liveCmp stacked acc e =
  case e of
    App(v,vs) => liveVals stacked (liveVal stacked acc v) vs
  | Special(_,vs,_) => liveVals stacked acc vs
  | Triv vs => liveVals stacked acc vs
  | Throw(v,_,_) => liveVal stacked acc v
  | _ => MILPretty.failCmp e "Liveness.liveCmp: unexpected non-continuation term"

  (*......................................................................*)
  (* Determine live variables in blocks. Essentially this is the free     *)
  (* variables, closed under calls to other blocks			  *)
  (*......................................................................*)
  fun blockLiveTAbs ((xs,e):TAbstr) = remove(blockLiveCont e, map #1 xs)
  and blockLiveAbs ((xs,e):Abstr) = remove(blockLiveCont e, xs)
  and blockLiveCont e =
  let
    fun liveCases (v, cases, default, cty) =
      liveVal Var.Map.empty (foldr (fn ((_,a),acc) => Var.Set.union(acc, blockLiveAbs a))
                            (case default of NONE => Var.Set.empty | SOME e => blockLiveCont e) cases)
                  v
  in
    case e of
      LetVal(x,v,e) => liveVal Var.Map.empty (remove(blockLiveCont e,[x])) v
    | Let(e1, ([xty as (x,_)], e2)) => liveCmp Var.Map.empty (remove(blockLiveCont e2,[x])) e1 
    | Let(e, a) => liveCmp Var.Map.empty (blockLiveTAbs a) e
    | TryLet(e, H, a) =>
      let val contlive = blockLiveTAbs a
          val handlerlive = foldr (fn (a,acc) => Var.Set.union(acc, blockLiveTAbs a)) Var.Set.empty H
      in liveCmp Var.Map.empty (Var.Set.union(handlerlive, contlive)) e
      end
    | Case a => liveCases a
    | CaseSCon a => liveCases a
    | TypeCase a => liveCases a
    | LetFun(_, LocalFun, Fun(f,a), e2) => (addBlockLive(f,blockLiveTAbs a); blockLiveCont e2)
    | LetFun(_, LocalFun, RecFun defs, e) =>
      let
        val result = foldr (fn ((_,_,a,_),acc) => Var.Set.union(acc,blockLiveTAbs a)) Var.Set.empty defs
      in
        List.app (fn (f,_,_,_) => addBlockLive(f, result)) defs;
        List.app (fn (_,g,_,_) => addBlockLive(g, result)) defs;
        blockLiveCont e
      end

    | App(v,vs) =>
      (case getValVar v of
        NONE => liveCmp Var.Map.empty Var.Set.empty e
      | SOME f =>
        case findBlockLive f of
          NONE => liveCmp Var.Map.empty Var.Set.empty e
        | SOME xs => liveVals Var.Map.empty xs vs
      )
      
    | _ => liveCmp Var.Map.empty Var.Set.empty e
  end

  fun iterateBlockLive (iter, e) =
  (
    blockLiveCont e;
    if blockLiveChanged()
    then iterateBlockLive(iter+1, e)
    else Stats.add ("fixpoint iterations/method", iter+1)
  )
  
  (*......................................................................*)
  (* Determine live variables for binders.				  *) 
  (*......................................................................*)
  fun bindLiveTAbs stacked (xs,e) =
  let val contlive = bindLiveCont stacked e
  in
    List.app (fn (x,_) => addBindLive(x,contlive)) xs; 
    remove(contlive, map #1 xs)
  end

  and bindLiveAbs stacked (xs,e) =
  let val contlive = bindLiveCont stacked e
  in
    List.app (fn x => addBindLive(x,contlive)) xs; 
    remove(contlive, xs)
  end

  and bindLiveCont stacked e =
  let
    fun liveCases (v, cases, default, cty) =
      liveVal stacked (foldr (fn ((_,a),acc) => Var.Set.union(acc, bindLiveAbs stacked a))
                     (case default of NONE => Var.Set.empty | SOME e => bindLiveCont stacked e) cases)
                  v
  in
    case e of
      LetVal(x,v,e) =>
      if isStackable(#1 x) andalso Controls.get stackVals
      then bindLiveCont (Var.Map.insert(stacked, #1 x, Triv [v])) e
      else
      let val contlive = bindLiveCont stacked e
      in addBindLive(x,contlive); liveVal stacked (remove(contlive,[x])) v end
    | Let(e1, ([xty as (x,_)], e2)) =>
      if isStackable(#1 x) andalso Controls.get stackCmps
      then bindLiveCont (Var.Map.insert(stacked, #1 x, e1)) e2
      else
      let val contlive = bindLiveCont stacked e2
      in addBindLive(x,contlive); liveCmp stacked (remove(contlive,[x])) e1 end
    | Let(e, a) =>
      let val contlive = bindLiveTAbs stacked a
      in liveCmp stacked contlive e end
    | TryLet(e, H, a) =>
      let val contlive = bindLiveTAbs stacked a
          val handlerlive = foldr (fn (a,acc) => Var.Set.union(acc, bindLiveTAbs stacked a)) Var.Set.empty H
      in
        liveCmp stacked (Var.Set.union(handlerlive, contlive)) e
      end
    | Case a => liveCases a
    | CaseSCon a => liveCases a
    | TypeCase a => liveCases a
    | LetFun(_, LocalFun, Fun(f,a), e2) => (bindLiveTAbs stacked a; bindLiveCont stacked e2)
    | LetFun(_, LocalFun, RecFun defs, e) =>
      let
        val result = foldr (fn ((_,_,a,_),acc) => Var.Set.union(acc,bindLiveTAbs stacked a)) Var.Set.empty defs
      in
        bindLiveCont stacked e
      end
    | App(v,vs) =>
      (case getValVar v of
        NONE => liveCmp stacked Var.Set.empty e
      | SOME f =>
        case findBlockLive f of
          NONE => liveCmp stacked Var.Set.empty e
        | SOME xs => liveVals stacked xs vs
      )
      
    | _ => liveCmp stacked Var.Set.empty e
  end
in
  openStackable();
  openLive();
  acont empty e;
  iterateBlockLive (0,e);
  bindLiveCont Var.Map.empty e;
  closeStackable();
  closeLive()
end (* of let *)
end (* of struct *)