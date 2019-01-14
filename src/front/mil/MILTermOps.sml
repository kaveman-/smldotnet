(*======================================================================*)
(* Various useful operations on MIL terms				*)
(*======================================================================*)
structure MILTermOps :> MILTERMOPS =
struct


local open MILTerm Gen in

(* Flag to name anonymous vars, tracking their source *)
val nameAnonVars = Controls.add false "debug.nameAnonVars"


(* Set the name for anonymous vars *)
local
    val anonVarName = ref ([]:MILTerm.SourceInfo)
in

fun setAnonVarName sourceInfo = 
    let val sourceInfo = if Controls.get nameAnonVars then sourceInfo else []
        val saved = !anonVarName 
    in  
	anonVarName := sourceInfo;
	fn () => anonVarName := saved
    end

fun getAnonVarName() = !anonVarName 

end




(*----------------------------------------------------------------------*)
(* Environments								*)
(*----------------------------------------------------------------------*)
type TyEnv = MILTy.Type Var.Map.map
type KindEnv = MILTy.Kind Var.Map.map
fun addTyVars (env, tyvars) =
  foldr (fn ((x,kind),env) => Var.Map.insert(env, x, kind)) env tyvars
fun addTypedVars (env, typedvars) =
  foldr (fn (((x,_),ty),env) => Var.Map.insert(env, x, ty)) env typedvars
fun addBoundVars (env, boundvars, tys) =
  ListPair.foldr (fn ((x,_), ty, env) => Var.Map.insert(env, x, ty)) env 
  (boundvars, tys)
fun addBoundVar (env, (x,_), ty) =
  Var.Map.insert(env, x, ty)

(*----------------------------------------------------------------------*)
(* Symbolic Environments                								*)
(*----------------------------------------------------------------------*)
type SymbolEnv = MILTerm.SourceInfo Var.Map.map
fun addTypedSymbols (env, typedvars) =
  foldr (fn (((x,si),_),env) => if List.null si then env else Var.Map.insert(env, x, si)) env typedvars
fun addBoundSymbols (env, boundvars) =
  foldr (fn ((x,si), env) => if List.null si then env else Var.Map.insert(env, x, si))env 
  boundvars
fun addBoundSymbol (env, (x,si)) =
  if List.null si then env else Var.Map.insert(env, x, si)

(*----------------------------------------------------------------------*)
(* Make a type abstraction term.                                        *)
(*----------------------------------------------------------------------*)
fun tabs (tyvars, ve) =
  if null tyvars then ve else TAbs(tyvars, ve)

(*----------------------------------------------------------------------*)
(* Make a type application term.					*)
(*----------------------------------------------------------------------*)
fun tapp (ve, tys) =
  if null tys then ve else TApp(ve, tys)

fun isLocal LocalFun = true
  | isLocal _ = false

val trueVal = Fold(Inj(MILTy.sum [[],[]], 1, [],Id.fromString "true"), MILTys.bool)
val falseVal = Fold(Inj(MILTy.sum [[],[]], 0, [],Id.fromString "false"), MILTys.bool)

val dummyBoundVar = (Var.dummy, [])

fun cond (v, e1, e2, cty) = 
  Case(Unfold v, [(0, ([], e2)), (1, ([], e1))], NONE, cty)

(*----------------------------------------------------------------------*)
(* Does a type variable occur free in a value/computation term?       	*)
(*----------------------------------------------------------------------*)
fun occurs n ty = Var.Set.member(MILTy.tyvars ty, n)
fun occursCTy n (MILTy.CmpType(eff,tys)) = List.exists (occurs n) tys

fun tyVarOccursVal n v = 
case v of
  Inj(ty, _, vs,_) => occurs n ty orelse tyVarOccursValList n vs
| ExCon(ty, vs) => occurs n ty orelse tyVarOccursValList n vs
| Tuple vs => tyVarOccursValList n vs
| As(v,ty) => tyVarOccursVal n v orelse occurs n ty
| Fold(v,ty) => tyVarOccursVal n v orelse occurs n ty
| Proj(_, _, v) => tyVarOccursVal n v
| TAbs(_, v) => tyVarOccursVal n v
| Unfold v => tyVarOccursVal n v
| TApp(v, tys) => tyVarOccursVal n v orelse List.exists (occurs n) tys
| _ => false


and tyVarOccursCmp n ce =
let
  fun tyVarOccursCases (v, cases, eopt, cty) =
  tyVarOccursVal n v 
  orelse occursCTy n cty
  orelse List.exists (fn (i,abs) => tyVarOccursAbstr n abs) cases
  orelse (case eopt of NONE => false | SOME e => tyVarOccursCmp n e)

in
case ce of
  App(ve, velist) => tyVarOccursVal n ve orelse tyVarOccursValList n velist
| Special(_, vs, cty) => tyVarOccursValList n vs orelse occursCTy n cty
| Let(ce, tabs) => tyVarOccursCmp n ce orelse tyVarOccursTAbstr n tabs
| Triv velist => tyVarOccursValList n velist
| Case cases => tyVarOccursCases cases
| CaseSCon cases => tyVarOccursCases cases
| TypeCase cases => tyVarOccursCases cases
| Throw(ve, cty, loc) =>
  tyVarOccursVal n ve orelse occursCTy n cty
| TryLet(ce, tabss, tabs) => 
  tyVarOccursCmp n ce orelse List.exists (tyVarOccursTAbstr n) tabss
  orelse tyVarOccursTAbstr n tabs
| LetFun(tyvars, kind, defs, ce) => 
  tyVarOccursCmp n ce orelse tyVarOccursFunDef n defs 
| LetClass(_,_,_,_,ce) =>
  tyVarOccursCmp n ce
| Encap e => tyVarOccursCmp n e
| LetVal(v,ve,ce) => tyVarOccursVal n ve orelse tyVarOccursCmp n ce
end

and tyVarOccursValList n velist = List.exists (tyVarOccursVal n) velist
and tyVarOccursTAbstr n (vs, body) = 
  tyVarOccursCmp n body orelse List.exists (occurs n) (map #2 vs)
and tyVarOccursAbstr n (vs, body) = tyVarOccursCmp n body

and tyVarOccursFunDef n (Fun(_,tabs)) = tyVarOccursTAbstr n tabs
  | tyVarOccursFunDef n (RecFun defs) =  
    List.exists (tyVarOccursTAbstr n) (map #3 defs)

(*----------------------------------------------------------------------*)
(* Instantiate type variables?                                       	*)
(*----------------------------------------------------------------------*)
fun substVal S v =
let
  val st = MILTy.subst S
  val sv = substVal S
in
case v of
  Inj(ty, i, args, si) => Inj(st ty, i, map sv args, si)
| ExCon(excon, args) => ExCon(st excon, map sv args) 
| Tuple args => Tuple (map sv args)
| Proj(i, n, v) => Proj(i, n, sv v)
| TApp(v, tys) => TApp(sv v, map st tys)
| TAbs(tyvars, v) => TAbs(tyvars, sv v)
| As(v, ty) => As(sv v, st ty)
| Fold (v, ty) => Fold(sv v, st ty)
| Unfold v => Unfold(sv v)
| SCon _ => v
| Var _ => v

end

and substCmp S e =
let
  val st = MILTy.subst S
  fun sct cty = 
  let val (eff, tys) = MILTy.fromCmp cty
  in MILTy.cmp(eff, map st tys) end

  val sv = substVal S
  val sc = substCmp S

  fun instCases si (v, cases, eopt, cty) = (sv v,
    map (fn (i, (vars, e)) => (si i, (vars, sc e))) cases, 
    Option.map sc eopt, sct cty)

  fun instTAbstr (typedvars, e) = 
    (map (fn (var,ty) => (var, st ty)) typedvars, sc e)

in
case e of
  App(v, vs) => App(sv v, map sv vs)
| Special(x, vs, cty) => Special(x, map sv vs, sct cty)
| Let(e1, tabs) => Let(sc e1, instTAbstr tabs)
| Triv vs => Triv (map sv vs)
| Case cases => Case(instCases (fn i => i) cases)
| CaseSCon cases => CaseSCon(instCases (fn c => c) cases)
| TypeCase cases => TypeCase(instCases st cases)
| Throw(v, cty, loc) => Throw(sv v, sct cty, loc)
| TryLet(e1, handlers, tabs) => TryLet(sc e1, map instTAbstr handlers, 
  instTAbstr tabs)
| LetFun(tyvars, kind, def, e) => 
  LetFun(tyvars, kind, 
    case def of
      Fun (f, tabs) => Fun (f, instTAbstr tabs)
    | RecFun defs => RecFun (
      map (fn (f,g,tabs,cty) => (f,g,instTAbstr tabs,sct cty)) defs),
    sc e)
(* @todo akenn: should recurse into methods *)
| LetClass(class,info,fields,methods,e) => 
  LetClass(class,info,fields,methods,sc e)
| LetVal(var, v, e) => LetVal(var, sv v, sc e)
| Encap e => Encap(sc e)
end

(*----------------------------------------------------------------------*)
(* Is the size of ve larger than n?        				*)
(*----------------------------------------------------------------------*)
fun sizeVal n v =
  if n < 0 then n
  else
  (case v of
    Var _ => n
  | SCon _ => n-1
  | Tuple vs => sizeVals (n-1) vs
  | ExCon(_,vs) => sizeVals (n-1) vs
  | Inj(_,_,vs,_) => sizeVals (n-1) vs
  | Proj(_,_,v) => sizeVal (n-1) v
  | TApp(v,_) => sizeVal n v
  | As(v,_) => sizeVal n v
  | Fold(v,_) => sizeVal n v
  | Unfold v => sizeVal n v
  | TAbs(_,v) => sizeVal n v)


and sizeVals n [] = n
  | sizeVals n (ve::ves) =
    let
      val n' = sizeVal n ve
    in
      if n' < 0 then n' else sizeVals n' ves
    end

and sizeCmp n ce =
let
  fun sizeCases (ve, cases, ceopt, _) =
    let
      val n' = sizeVal (n-1) ve
      fun loop n [] = 
          (if n < 0 then n else 
          case ceopt of
            SOME ce => sizeCmp n ce
          | NONE => n)

        | loop n ((i,(_,ce))::rest) =
          let
            val n' = sizeCmp n ce
          in
            if n' < 0 then n' else loop n' rest
          end       
    in
      if n' < 0 then n' else loop n' cases
    end
in
  if n < 0 then n
  else
  (case ce of
    App(ve,ves) => sizeVals (n-1) (ve::ves)
  | Special(_,ves,_) => sizeVals (n-1) ves
  | Let(ce1,(_,ce2)) => sizeCmps n [ce1,ce2]
  | Triv ves => sizeVals n ves
  | Case cases => sizeCases cases
  | CaseSCon cases => sizeCases cases
  | TypeCase cases => sizeCases cases
  | Throw(ve,_,_) => sizeVal (n-1) ve
  | TryLet(ce1, tabss, (_,ce3)) => sizeCmps (n-1) ([ce1,ce3] @ (map #2 tabss))
  | LetFun(_,kind,def,ce) => if not (isLocal kind) then ~1
    else
    let
      fun loop n [] = sizeCmp n ce
        | loop n ((_,_,(_,ce),_)::rest) =
          let
            val n' = sizeCmp n ce
          in
            if n' < 0 then n' else loop n' rest
          end
    in
      case def of
        Fun (_, (_,ce')) => sizeCmps n [ce,ce']
      | RecFun defs => loop n defs
    end
  | LetClass(_,_,fields,methods,ce) => ~1
  | Encap e => sizeCmp (n-1) e
  | LetVal(v, ve, ce) => 
    let
      val n' = sizeVal n ve
    in
      if n' < 0 then n'
      else sizeCmp n' ce
    end)
end

and sizeCmps n [] = n
  | sizeCmps n (ce::ces) =
    let
      val n' = sizeCmp n ce
    in
      if n' < 0 then n' else sizeCmps n' ces
    end

fun valBigger (ve,n) = sizeVal n ve < 0
fun cmpBigger (ce,n) = sizeCmp n ce < 0


(*----------------------------------------------------------------------*)
(* Actual size of a term.	        				*)
(*----------------------------------------------------------------------*)
fun absSizeVal v =
  case v of
    Var _ => 0
  | SCon _ => 1
  | Inj (_,_,vs,_) => 1 + absSizeVals vs
  | ExCon(_,vs) => 1 + absSizeVals vs
  | Tuple vs => 1 + absSizeVals vs
  | Proj(_,_,v) => 1 + absSizeVal v 
  | TApp(v, _) => absSizeVal v
  | As(v,_) => absSizeVal v
  | TAbs(_,v) => absSizeVal v
  | Fold(v,_) => absSizeVal v
  | Unfold v => absSizeVal v


and absSizeVals [] = 0
  | absSizeVals (v::vs) = absSizeVal v + absSizeVals vs

and absSizeCmp e =
let
  fun sizeCases (v, cases, def, _) =
    1 + absSizeVal v + (case def of NONE => 0 | SOME e => absSizeCmp e) +
    foldr op+ 0 (map (fn (i,(_,e)) => absSizeCmp e) cases)
in
  case e of
    App(v,vs) => 1 + absSizeVal v + absSizeVals vs
  | Special(_,vs,_) => 1 + absSizeVals vs
  | Let(e1,(_,e2)) => absSizeCmp e1 + absSizeCmp e2
  | Triv vs => absSizeVals vs
  | Case cases => sizeCases cases
  | CaseSCon cases => sizeCases cases
  | TypeCase cases => sizeCases cases
  | Throw(v,_,_) => 1 + absSizeVal v
  | TryLet(e, h, (_,e')) => 
    1 + absSizeCmp e + absSizeCmp e' + foldr op+ 0 (map (absSizeCmp o #2) h)
  | LetFun(_,kind,def,e) => 
    absSizeCmp e + 
    (case def of
        Fun (_, (_,e')) => absSizeCmp e'
      | RecFun defs => foldr op+ 0 (map (absSizeCmp o #2 o #3) defs)
    )
  | LetClass(_,_,fields,methods,e) =>
    foldr (fn ((_,_,_,_,_,NONE),n) => n | ((_,_,_,_,_,SOME(_,(_,e))),n) => absSizeCmp e + n)
    (absSizeCmp e) methods
  | Encap e => absSizeCmp e
  | LetVal(_, v, e) => absSizeVal v + absSizeCmp e
end

val absSize = absSizeCmp

(*----------------------------------------------------------------------*)
(* Are two values (semantically) equal?					*)
(*   SOME true    => yes                                                *)
(*   SOME false   => no                                                 *)
(*   NONE         => don't know                                         *)
(*----------------------------------------------------------------------*)
fun valEq (v1,v2) =
case (v1,v2) of
  (Var x, Var y) => 
  if Var.eq(x,y) then SOME true else NONE

| (Var _, _) => 
  NONE

| (_, Var _) => 
  NONE

| (SCon(ty1,c1), SCon(ty2,c2)) => 
  SOME (Constants.equal(c1,c2,true))

| (Inj(ty1,i1,vs1,si1), Inj(ty2,i2,vs2,si2)) => (*TODO: consider symbol info si1 & si2 *)
  if i1<>i2 then SOME false 
  else valsEq (vs1,vs2)

| (Tuple vs1, Tuple vs2) =>
  valsEq (vs1, vs2)

| (Fold(v1,ty1), Fold(v2,ty2)) =>
  valEq (v1,v2)

| (Unfold v1, Unfold v2) =>
  valEq (v1,v2)

| _ =>
  NONE

and valsEq ([],[]) = SOME true
  | valsEq (x::xs,y::ys) = 
    (case valEq (x,y) of
      SOME false => SOME false
    | SOME true => valsEq (xs,ys)
    | NONE => 
      case valsEq (xs,ys) of
        NONE => NONE
      | SOME false => SOME false
      | SOME true => NONE)
  | valsEq _ = SOME false

(*----------------------------------------------------------------------*)
(* Determine the type of a value term.                                  *)
(*----------------------------------------------------------------------*)
fun typeOfVal tyenv v =
  case v of

(*......................................................................*)
(* Terms with explicit types                   				*)
(*......................................................................*)
  SCon (ty, _) => ty
| Fold(_, ty) => ty
| As(_, ty) => ty
| Inj(ty, _, _,_) => ty


  
(*......................................................................*)
(* Variables								*)
(*......................................................................*)
| Var x => 
  Var.lookup(tyenv, x)

(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold v =>
  let
    val subty = typeOfVal tyenv v

    (* The type of the subterm must be a mu type *)
    val a = case MILTy.fromMu subty of
      SOME a => a
    | NONE => Debug.fail "MILTermOps.typeOfVal: expected mu type"
  in
    (* Unfold this type one level *)
    MILTy.unfold a
  end

(*......................................................................*)
(* Exception introduction                                               *)
(*......................................................................*)
| ExCon _ =>
  MILTys.topExn
  
(*......................................................................*)
(* Product introduction                                           	*)
(*......................................................................*)
| Tuple vs => 
  MILTy.prod (map (typeOfVal tyenv) vs)

(*......................................................................*)
(* Product elimination                                    		*)
(*......................................................................*)
| Proj(i, n, v) => 
  let
    val prodty = typeOfVal tyenv v
    val tys = case MILTy.fromProdCon prodty of
      SOME a => a
    | NONE => Debug.fail "MILTermOps.typeOfVal: expected product type"
  in
    List.nth (tys, i)
  end

(*......................................................................*)
(* Quantifier elimination						*)
(*......................................................................*)
| TApp(v, tys) => 
  let
    val SOME a = MILTy.fromForall (typeOfVal tyenv v)
  in
    MILTy.app (MILTy.abs a, tys)
  end

(*......................................................................*)
(* Quantifier introduction						*)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val ty = typeOfVal tyenv v
  in
    MILTy.forall(tyvars, ty)
  end


(* Derive a symbol for the projection from another (non-empty) symbol *)
fun symbolOfProj(si,i) = if List.null si 
                         then si 
                         else [Id.fromString ("#" ^ Int.toString (i+1) ^ "(" ^ Id.toString (List.last si) ^ ")")]

(*----------------------------------------------------------------------*)
(* Determine the symbol of a value term.                                *)
(*----------------------------------------------------------------------*)
fun symbolOfVal symenv v =
  case v of
(*......................................................................*)
(* Terms with explicit types                   				*)
(*......................................................................*)
  SCon (ty, _) => []
| Fold(v, ty) => symbolOfVal symenv v
| As(v, ty) => symbolOfVal symenv v
| Inj(ty, _, _,_) => []
(*......................................................................*)
(* Variables								*)
(*......................................................................*)
| Var x => 
  (case Var.Map.find(symenv, x) of NONE => [] | SOME  si => si)
(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold v =>
  symbolOfVal symenv v
(*......................................................................*)
(* Exception introduction                                               *)
(*......................................................................*)
| ExCon _ =>
  [] 
(*......................................................................*)
(* Product introduction                                           	*)
(*......................................................................*)
| Tuple vs => 
  []
(*......................................................................*)
(* Product elimination                                    		*)
(*......................................................................*)
| Proj(i, n, v) => 
 (case symbolOfVal symenv v  of 
       [] => []
     | si => symbolOfProj(si,i))
(*......................................................................*)
(* Quantifier elimination						*)
(*......................................................................*)
| TApp(v, tys) => 
  symbolOfVal symenv v
(*......................................................................*)
(* Quantifier introduction						*)
(*......................................................................*)
| TAbs(tyvars, v) =>
  symbolOfVal symenv v



end (* of local open MILTerm *)

end (* of struct *)








