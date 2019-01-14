(*======================================================================*)
(* Global free variable counts for `current' term.			*)
(* Assumption 1: bound variables in the term are distinct.              *)
(* Assumption 2: the census is used in a single threaded way; there's   *)
(* only one census so no backtracking is allowed.                       *)
(*======================================================================*)
structure Census :> CENSUS = 
struct

local 
  open MILTerm
in

(*----------------------------------------------------------------------*)
(* A census is a count of free occurrences of each variable.		*)
(*----------------------------------------------------------------------*)
type Census = DynIntArray.array

(* First unused variable in the census *)
val supply = ref Var.initial

(* Start of free list; zero indicates no free spaces *)
val free = ref 0

(* The census array itself *)
val census = DynIntArray.array 0

(*----------------------------------------------------------------------*)
(* Add n to the census count of variable x.				*)
(*----------------------------------------------------------------------*)
fun inc(x,n) = 
  DynIntArray.update(census, Var.index x, DynIntArray.sub(census, Var.index x) + n)

(*----------------------------------------------------------------------*)
(* Add n to the census counts for variables in value term v.       	*)
(*----------------------------------------------------------------------*)
fun censusVal (n,v) = 
case v of
  Var x => inc(x,n)
| SCon _ => ()
| Inj(_,_,vs,_) => censusValList (n,vs)
| ExCon(_,vs) => censusValList (n,vs)
| Tuple vs => censusValList (n,vs)
| Unfold v => censusVal (n,v)
| Fold(v,_) => censusVal (n,v)
| Proj(_,_,v) => censusVal (n,v)
| TApp(v,_) => censusVal (n,v)
| TAbs(_,v) => censusVal (n,v)
| As(v,_) => censusVal (n,v)


(*----------------------------------------------------------------------*)
(* Add n to the census counts for variables in computation term v.     	*)
(*----------------------------------------------------------------------*)
and censusCmp (n,e) =
let
  fun censusCases (v, cases, eopt, _) =
    (censusVal (n,v);
     (case eopt of NONE => () | SOME e => censusCmp (n,e));
     app (fn (i,(_,body)) => censusCmp (n,body)) cases)
in
case e of

  App(v, vs) =>
  (censusVal (n,v); censusValList (n,vs))

| Special(_, vs, cty) => 
  censusValList (n,vs)
| Let(e1, (_, e2)) => 
  (censusCmp (n,e1); censusCmp (n,e2))
| Triv vs => censusValList (n,vs)
| Case cases => censusCases cases
| CaseSCon cases => censusCases cases
| TypeCase cases => censusCases cases
| TryLet(e, tabss, (_,body)) =>
  (censusCmp (n,e);
  app (fn tabs => censusTAbstr (n,tabs)) tabss;
  censusCmp (n,body))
| LetFun(tyvars, kind, def, e) => 
  (censusCmp (n,e);
  case def of
    Fun (_,(_,e)) => censusCmp (n,e)
  | RecFun defs => app (fn (_,_,(_,e),_) => censusCmp (n,e)) defs)
| LetClass(classname,info,fields,methods,e) =>
  (censusCmp (n,e); 
  app (fn (_,_,_,_,_,SOME (_,(_,e))) => censusCmp (n,e)
        | _ => ()) methods)

| Throw(v,_,_) => censusVal (n,v)
| LetVal(x, v, e) => (censusVal (n,v); censusCmp (n,e))
| Encap e => censusCmp (n,e)

end

(*----------------------------------------------------------------------*)
(* Add n to the census counts for variables in value terms vs.       	*)
(*----------------------------------------------------------------------*)
and censusValList (n,vs) = app (fn v => censusVal (n,v)) vs

(*----------------------------------------------------------------------*)
(* Add n to the census counts for variables in a typed abstraction.    	*)
(*----------------------------------------------------------------------*)
and censusTAbstr (n, (_, body)) = censusCmp (n,body)

fun maxVar () = !supply

fun clearCensus () = (DynIntArray.clear census; free := 0)

fun freshVar n =
(*if !free = 0
then  *)
let
  val (sup,v) = Var.fresh (!supply)
in
  supply := sup;
  DynIntArray.update(census, Var.index v, n);
  v
end
(*
else 
let
  val v = !free
in
  free := DynIntArray.sub(census, v);
  DynIntArray.update(census, v, n);
  v
end
*)


fun freshBoundVar n ((x,sym) : MILTerm.BoundVar) =  
let
  val x = freshVar n
in
  ((x,sym), MILTerm.Var x)
end

fun freshBoundAnonVar n =
let
  val x = freshVar n
in
  ((x,MILTermOps.getAnonVarName()), MILTerm.Var x)
end

fun freshTypedVar n (((x,sym),ty) : MILTerm.TypedVar) =  
let
  val x = freshVar n
in
  (((x,sym),ty), MILTerm.Var x)
end

fun freshBoundVars n = ListPair.unzip o map (freshBoundVar n)
fun freshTypedVars n = ListPair.unzip o map (freshTypedVar n)
fun freshTypedAnonVars n = 
  ListPair.unzip o (map (fn ty => let val (x,xv) = freshBoundAnonVar n in ((x,ty),xv) end))

(*----------------------------------------------------------------------*)
(* What variables are bound in e?					*)
(* Return a list so that we can check for duplicates.			*)
(*----------------------------------------------------------------------*)
fun boundTAbstr ((xs,e):TAbstr) = map (fn ((x,_),_) => x) xs @ boundCmp e
and boundAbstr ((xs,e):Abstr) = map #1 xs @ boundCmp e
and boundCmp e =
let
  fun boundCases (_, cases, optdef, _) =
    foldl (fn ((_,abs),xs) => xs @ boundAbstr abs)
    (case optdef of NONE => [] | SOME e => boundCmp e) cases
in
case e of
  App _ => []
| Special _ => []
| Throw _ => []
| Triv _ => []
| Let(e, tabs) => boundCmp e @ boundTAbstr tabs
| LetVal(x, v, e) => #1 x :: boundCmp e
| Case cases => boundCases cases
| CaseSCon cases => boundCases cases
| TypeCase cases => boundCases cases
| TryLet(e, handlers, tabs) => 
  foldl (fn (tabs, xs) => xs @ boundTAbstr tabs) (boundTAbstr tabs) handlers @ boundCmp e

| LetFun(tyvars, kind, def, e) => 
  let val xs = boundCmp e
  in
    case def of
      Fun (f,tabs) => 
      #1 f :: xs @ boundTAbstr tabs

    | RecFun defs => 
      foldl (fn ((f,g,tabs,_),xs) => xs @ #1 f :: #1 g :: boundTAbstr tabs)
        xs defs
  end
        
| LetClass(_,_,_,methods,e) =>
  foldl (fn ((_,_,_,_,_,SOME (_,abs)),xs) => xs @ boundAbstr abs | ((_,_,_,_,_,NONE),xs) => xs)
    (boundCmp e) methods

| Encap e => 
  boundCmp e

end

fun addCmp (ce,0) = ()
  | addCmp (ce,n) = censusCmp (n, ce)

fun addVal (ve,0) = ()
  | addVal (ve,n) = censusVal (n, ve)

fun addVar (v,0) = ()
  | addVar (v,n) = inc (v, n)

fun initCensus (e, s) = 
  (supply := s; free := 0; clearCensus (); addCmp (e,1))

(*----------------------------------------------------------------------*)
(* Mark a variable as inlined.						*)
(*----------------------------------------------------------------------*)
fun inlineVar x = DynIntArray.update(census, Var.index x, ~1)

(*----------------------------------------------------------------------*)
(* Remove a variable from the census; it must not appear in the term,	*)
(* even as a bound variable.                                            *)
(*----------------------------------------------------------------------*)
fun removeVar x = 
if Var.isDummy x then () else
let
  val ix = Var.index x
in
  DynIntArray.update(census, ix, !free);
  free := ix
end

fun removeBoundVar (x,sym) = removeVar x

(*----------------------------------------------------------------------*)
(* Remove all bound variables from the census.                          *)
(* Note that we must kill the bound variables *after* traversing the    *)
(* term in which they occur free.                                       *)
(*----------------------------------------------------------------------*)
fun removeCmp e =
let
  fun removeCases ((v, cases, eopt,cty) : 'a Cases) =
    (censusVal(~1,v); 
     (case eopt of NONE => () | SOME e => removeCmp e);
     app (removeAbstr o #2) cases)
in
case e of
  Let(e, tabs) =>
  (removeCmp e; removeTAbstr tabs)
| Case cases => removeCases cases
| CaseSCon cases => removeCases cases
| TypeCase cases => removeCases cases
| TryLet(e, tabss, tabs) =>
  (removeCmp e; app removeTAbstr tabss; removeTAbstr tabs)
| LetFun(tyvars, kind, def, e) => 
  (removeCmp e;
  case def of
    Fun (f,tabs) => (removeTAbstr tabs; removeBoundVar f)
  | RecFun defs => 
    (app (removeTAbstr o #3) defs;
     app (fn (f,g,_,_) => (removeBoundVar f; removeBoundVar g)) defs)
  )
| LetClass(classname,info,fields,methods,e) =>
  (removeCmp e;
  app (fn (_,_,_,_,_,SOME (f,abs)) => (removeAbstr abs; removeBoundVar f)
        | _ => ()) methods)

| LetVal(x, v, e) => (censusVal(~1,v); removeCmp e; removeBoundVar x)
| Encap e => removeCmp e
| e => censusCmp(~1, e)

end

and removeTAbstr (xs,e) = (removeCmp e; app (removeBoundVar o #1) xs)
and removeAbstr (xs,e) = (removeCmp e; app removeBoundVar xs)

fun getVar x = DynIntArray.sub(census, Var.index x)
fun isDead x = Var.isDummy x orelse getVar x = 0

(*----------------------------------------------------------------------*)
(* Rename a single bound variable					*)
(*----------------------------------------------------------------------*)
fun renameBoundVar (r, (x,sym)) = 
let
  val x' = freshVar 0
in
  (Var.Map.insert(r, x, x'), (x',sym))
end

(*----------------------------------------------------------------------*)
(* Rename bound variables						*)
(*----------------------------------------------------------------------*)
fun renameBoundVars (r, xs) =
  foldr (fn ((x,sym), (r,xs)) => 
    if Var.isDummy x then (r, (x,sym)::xs)
    else
      let val x' = freshVar 0
      in (Var.Map.insert(r, x, x'), (x',sym)::xs) end)
    (r,[]) xs
  
(*----------------------------------------------------------------------*)
(* Rename typed bound variables						*)
(*----------------------------------------------------------------------*)
fun renameTypedVars (r, xs) =
  foldr (fn (((x,sym),ty), (r,xs)) => 
    if Var.isDummy x then (r, ((x,sym),ty)::xs)
    else
      let val x' = freshVar 0 
      in (Var.Map.insert(r, x, x'), ((x',sym),ty)::xs) end)
    (r,[]) xs

(*----------------------------------------------------------------------*)
(* Rename a free variable						*)
(*----------------------------------------------------------------------*)
fun renameVar r v =    
  (case Var.Map.find(r, v) of
    SOME v' => 
    (inc (v', 1); v')

  | NONE => 
    (inc (v, 1); v)
  )

(*----------------------------------------------------------------------*)
(* Rename all bound variables in a term					*)
(* Don't change the dummy ones!                                         *)
(* Also add the new (and unchanged) variables into the census.          *)
(*----------------------------------------------------------------------*)
fun renameVal r ve =
let 
  val rc = renameCmp r
  val rv = renameVal r
in
case ve of
  Var v =>  
  Var (renameVar r v)

| SCon _ => 
  ve

| Inj(ty, i, ves, si) => 
  Inj(ty, i, map rv ves, si)

| ExCon(excon, ves) => 
  ExCon(excon, map rv ves)

| Tuple ves => 
  Tuple(map rv ves)

| Proj(i, n, ve) => 
  Proj(i, n, rv ve)

| TApp(ve, tys) => 
  TApp(rv ve, tys)

| TAbs(tyvars, ve) => 
  TAbs(tyvars, rv ve)

| As(v, ty) =>
  As(rv v, ty)

| Fold(ve, ty) => 
  Fold(rv ve, ty)

| Unfold ve => 
  Unfold(rv ve)

end

and renameTAbstr r (vs,ce) =
  let val (r, vs) = renameTypedVars (r,vs)
  in
    (vs, renameCmp r ce)
  end

and renameAbstr r (vs,ce) =
  let val (r, vs) = renameBoundVars (r,vs)
  in
    (vs, renameCmp r ce)
  end

and renameCmp r ce =
let 
  val rc = renameCmp r
  val rv = renameVal r
  fun renameCase (i, abs) = (i, renameAbstr r abs)
    
  fun renameCases (ve, cases, ceopt, cty) =
    (rv ve, map renameCase cases, Option.map rc ceopt, cty)

  fun renameLetFun (tyvars, kind, Fun (f, tabs), ce) =
      let
        val (r',f') = renameBoundVar (r,f)
      in
        (tyvars, kind, Fun (f', renameTAbstr r tabs), renameCmp r' ce)
      end

      (* Allow the source term to have identical external and internal fvars *)
    | renameLetFun (tyvars, kind, RecFun defs, ce) =
      let
        fun rename1 ((f, g, tabs, cty), (rin,rout,result)) =
        let
          val (rout,f') = renameBoundVar (rout, f)
          val (rin,g') = renameBoundVar (rin, g)
        in
          (rin, rout, (f', g', tabs, cty)::result)
        end
        val (rin,rout,defs) = foldr rename1 (r, r, []) defs
      in
        (tyvars, kind, RecFun (map (fn (f, g, tabs, cty) => 
            (f, g, renameTAbstr rin tabs, cty)) defs), renameCmp rout ce)
      end

in
case ce of
  App(ve, ves) => 
  App(rv ve, map rv ves)

| Special(jop, ves, cty) => 
  Special(jop, map rv ves, cty)

| Let(ce, tabs) => 
  Let(rc ce, renameTAbstr r tabs)

| Triv ves => 
  Triv(map rv ves)

| Case cases => 
  Case(renameCases cases)

| CaseSCon cases => 
  CaseSCon(renameCases cases)

| TypeCase cases => 
  TypeCase(renameCases cases)

| Throw(ve, cty, loc) =>
  Throw(rv ve, cty, loc)

| TryLet(ce, tabss, tabs) =>
  TryLet(rc ce, map (renameTAbstr r) tabss, renameTAbstr r tabs)

| LetFun args =>
  LetFun (renameLetFun args)

| LetClass(classname, classinfo, fields, methods, ce) =>
  let
    val methods = map (fn (arg as (s, att, m, tys, tyopt, absopt)) => 
    case absopt of
      NONE => 
      arg

    | SOME ((f,sym),abs) => 
      let
        val f' = freshVar 0
      in
        (s, att, m, tys, tyopt, SOME ((f',sym), renameAbstr r abs))
      end)
     methods
  in
    LetClass(classname, classinfo, fields, methods, rc ce)
  end

| LetVal((x,sym), v, e) =>
  let
    val x' = freshVar 0
  in
    LetVal((x',sym), rv v, renameCmp (Var.Map.insert(r, x, x')) e)
  end

| Encap e =>
  Encap (rc e)


end (* of let *)


val renameCmp = renameCmp Var.Map.empty
val renameTAbstr = renameTAbstr Var.Map.empty

(*----------------------------------------------------------------------*)
(* Check that the counts for variables in e are correct;	        *)
(* also check that bound variables are distinct. 			*)
(*----------------------------------------------------------------------*)
fun checkCmps (boundlist, es) prln  = 
  let
    (* Trash the free list so that it doesn't confuse the check *)
    fun gather (0,result) = result
      | gather (x,result) = 
        let
          val old = DynIntArray.sub(census, x)
        in
          DynIntArray.update(census, x, 0);
          gather(old, x::result)
        end

    val frees = gather (!free, [])

    (* Restore the free list later *)
    fun restore (y,[]) = 
        ()

      | restore (y,x::xs) = 
        (DynIntArray.update(census, y, x); restore (x, xs))
      
    val boundlist = boundlist @ List.concat (map boundCmp es)
    val boundset = 
    foldr (fn (x, set) =>
      if Var.isDummy x then set
      else
      if Var.Set.member(set, x)
      then (prln ("Bound variable occurs more than once: " ^ Var.toString x); set)
      else Var.Set.add(set, x)) Var.Set.empty boundlist

    fun check x = 
      if Var.index x < 0 then ()
      else      
      (if not (Var.Set.member(boundset, x)) then ()
       else if DynIntArray.sub(census, Var.index x) <> 0
       then prln ("Census incorrect by " ^ 
        Int.toString (DynIntArray.sub(census, Var.index x)) ^ " for variable " ^ 
        Var.toString x)
       else (); 
      check (Var.fromInt(Var.index x-1)))
    
    val n = Var.supplyIndex (!supply)

    fun total (x,result) =
      if x < 0 
      then prln (
        "Census: " ^ Int.toString result ^ 
        " used from " ^ Int.toString (n+1) ^ 
        " including " ^ Int.toString (length frees) ^ " free.")
      else
      if DynIntArray.sub(census, x) <> 0 
      then total(x-1,result+1)
      else total(x-1,result)
  in
    map (fn e => addCmp (e, ~1)) es;
    check (Var.fromInt n);
    map (fn e => addCmp (e, 1)) es;
    total (n,0);
    (case frees of [] => ()
                 | x::xs => (restore (x,xs); free := x))
  end

end (* of local open *) 

end (* of struct *)

