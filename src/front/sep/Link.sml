(*======================================================================*)
(* Linking of modules together.  					*)
(*======================================================================*)
structure Link :> LINK =
struct

local 
  open UnitTypes
  open MILTerm
  structure Map = Symbol.Map
in

val showLinkOrder = Controls.add false "link.showOrder" 
val showLinkTyMap = Controls.add false "link.showTyMap" 

(*----------------------------------------------------------------------*)
(* Dump the type name map						*)
(*----------------------------------------------------------------------*)
fun tymapToString tymap =
  Pretty.bigVec 0
  (fn (ty, ty') => MILTy.toString ty ^ " |-> " ^ MILTy.toString ty')
  (MILTy.Map.listItemsi tymap)


(*----------------------------------------------------------------------*)
(* Gather up concrete type definitions					*)
(*----------------------------------------------------------------------*)
fun makeTyMap entities =
let
  val entities = rev entities
  fun add ([], tymap) = tymap

    | add (entity::entities, tymap) =

      case (entity, Entity.Map.find(!UnitManager.cache, entity)) of
        ((Entity.Str, strid), SOME (Str {tynameTys = TNE, ...}, _)) =>
        let val f = MILTy.replace tymap
        in
          add (entities, 
            TyName.Map.foldri (fn (tyname, ty, tymap) =>
              MILTy.Map.insert(tymap, MILTy.tyname tyname, f ty)) tymap TNE)
        end

      | _ =>
        add (entities, tymap)
in
  add (entities, MILTy.Map.empty)
end

(*----------------------------------------------------------------------*)
(* Rename bound variables in term by adding an increment;               *)
(* rename free variables in term using map.                             *)
(*----------------------------------------------------------------------*)
fun rename (m,t,offset,tyvaroffset) e =
let 
  fun r x = 
    case Var.Map.find(m,x) of
      NONE => Var.fromInt(Var.index x + offset)
    | SOME x' => x'

  fun rb (x,s) = (r x, s)

  fun rk (v,k) =
  (Var.fromInt (Var.index v + tyvaroffset), 
   case k of
    MILTy.Bound ty => MILTy.Bound (t ty)
  | other => other)

  and rct cty =
  let
    val (eff,tys) = MILTy.fromCmp cty
  in
    MILTy.cmp(eff, map t tys)
  end

  and rv v =
  case v of
    Var x => Var (r x)
  | SCon (ty,c) => SCon (t ty,c)
  | Inj(ty, i, args, si) => Inj(t ty, i, map rv args, si)
  | As(v,ty) => As(rv v,t ty)
  | ExCon(ty, args) => ExCon(t ty, map rv args)
  | Tuple args => Tuple (map rv args)
  | Proj(i, n, v) => Proj(i, n, rv v)
  | TApp(v, tys) => TApp(rv v, map t tys)
  | TAbs(tyvars, v) => TAbs(map rk tyvars, rv v)
  | Fold (v, ty) => Fold(rv v, t ty)
  | Unfold v => Unfold(rv v)

  and rtabs (typedvars, e) = 
    (map (fn (x,ty) => (rb x, t ty)) typedvars, re e)

  and rabs (xs, e) = (map rb xs, re e)

  and re e =
  let
    fun rc (v, cases, eopt, cty) =  
      (rv v, map (fn (i, abs) => (i, rabs abs)) cases, Option.map re eopt, rct cty)
    fun rtc (v, cases, eopt, cty) =  
      (rv v, map (fn (i, abs) => (t i, rabs abs)) cases, Option.map re eopt, rct cty)
  in
  case e of
    App(v, vs) => App(rv v, map rv vs)
  | Special(j, vs, cty) => Special(j, map rv vs, rct cty)
  | Let(e, abs) => Let(re e, rtabs abs)
  | Triv vs => Triv (map rv vs)
  | Case cases => Case(rc cases)
  | CaseSCon cases => CaseSCon(rc cases)
  | TypeCase cases => TypeCase(rtc cases)
  | Throw(v, cty, loc) => Throw(rv v, rct cty, loc)
  | TryLet(e, handlers, abs) => TryLet(re e, map rtabs handlers, rtabs abs)
  | LetFun(tyvars, kind, def, e) => 
    LetFun(map rk tyvars, kind, 
      case def of
        RecFun recbinds =>
        RecFun (map (fn (f,g,tabs,cty) => (rb f,rb g,rtabs tabs,rct cty)) 
          recbinds)
      | Fun (f, tabs) =>
        Fun (rb f, rtabs tabs), re e)
  | LetClass(name,info,fields,methods,e) => 
     LetClass(name,info,
      map (fn (n,ms,ty,c) => (n,ms,t ty,c)) fields,
      map (fn (n,atts,ms,tys,tyopt,absopt) => 
        (n,atts,ms,map t tys,Option.map t tyopt, 
	 case absopt of NONE => NONE
          | SOME (f, abs) => SOME (rb f, rabs abs))) methods, 
	 re e)
  | LetVal(x, v, e) => LetVal(rb x, rv v, re e)
  | Encap e => Encap(re e)
  end
in
  re e
end

fun link (entities : Entity.Ref list, names) = 
let
  val _ = PrintManager.dump showLinkOrder (fn prln =>
   prln (Pretty.simpleVec "," EntityOps.toString (rev entities)))

  (* Gather up environment information for each SML structure *)
  val structures = List.mapPartial 
    (fn entity =>
     case (entity, Entity.Map.find(!UnitManager.cache, entity)) of
      ((Entity.Str, strid), SOME (Str info, _)) => SOME (strid, info)
     | _ => NONE) entities

  val tymap = makeTyMap entities

  val _ = PrintManager.dump showLinkTyMap (fn prln =>
    prln (tymapToString tymap))

  val names = 
    map (fn (tyname,s) => 
      ((case MILTy.Map.find(tymap, MILTy.tyname tyname) of
        SOME ty =>
        (case MILTy.fromTyname ty of
          SOME tyname => tyname
        | NONE => tyname)
      | NONE => tyname), s)) names

  (* Construct variable names for all the structures in the project *)
  val SE = 
    ListOps.foldri (fn (i, (strid, _), SE) => Map.insert(SE, strid, Var.fromInt (i+1)))
    Map.empty
    structures

  fun link' result [] = 
      result

    | link' (e,supply',tyvarsupply') 
      ((strid, { tyvarsupply, supply, 
                 term = (boundvars,term), E, tynameTys, ... }:StrEntry)::
        structures)=
      let  
	val t = MILTy.replaceAndRename (tymap, tyvarsupply')

        val r = 
          foldr 
          (fn ((x,[strid]),r) => Var.Map.insert(r,x,valOf(Map.find(SE, strid))))
          (Var.Map.insert(Var.Map.empty, Var.dummy, Var.dummy))
          boundvars

        val e' = rename (r, t, supply',tyvarsupply') term
        val globalvar = valOf(Symbol.Map.find(SE, strid))
        val ty = TransType.transE tynameTys E
      in
        link' 
          (Let(e', ([((globalvar,[strid]),t ty)], e)), 
           Var.supplyIndex supply + supply',
           Var.supplyIndex tyvarsupply + tyvarsupply') structures
      end

  val (e,supply,tyvarsupply) = link' (Triv [], length structures+1, 0) structures

(*
  (* Put a top-level exception handler round the whole thing *)
  val (e,supply) = 
    if Controls.isOn "exnLocs"
    then
    let 
      val (supply', exnvar) = Var.fresh supply
    in
      (TryLet(e, [([(exnvar, MILTys.topExn)], 
        Special((Ext.Invoke, NONE,SOME(Ids.symbol "printStackTrace")),
          [Coerce(Var exnvar, MILTy.base 
            (Types.CLASS ClassHandle.throwable))],
          MILTy.cmp(Effect.any, [])))], ([], Triv [])), supply')
    end else (e, supply)
*)

in
  (e,Var.supplyFromInt supply,names)
end

end (* of local open *)

end (* of struct *)
