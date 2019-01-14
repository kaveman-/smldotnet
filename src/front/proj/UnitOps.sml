(*======================================================================*)
(* Unit-of-compilation helper functions. See sig for details.		*)
(*======================================================================*)
structure UnitOps (* :> UNITOPS *) =
struct

local 
  open UnitTypes
  structure Map = Symbol.Map
in

(*----------------------------------------------------------------------*)
(* Given the imports for a module, calculate three things:		*)
(*   (1) An SML basis B under which to elaborate the module;            *)
(*   (2) A map strTys from the imported structures to their MIL types;  *) 
(*   (3) A map tynameTys from imported tynames to MIL types/constructors*) 
(* For (1) and (2), imported means direct imports (antecedents) as      *)
(*   recorded in deps                                                   *)
(* For (3), imported means all imports from any entity in prefix        *)
(*@FUTURE: For (3) it is probably safe to only add subset of prefix in  *)
(* *transitive* closure of antecedents of entity                        *)
(*----------------------------------------------------------------------*)
fun makeEnv (initialB, deps, prefix, entity) =
case Entity.Map.find(deps, entity) of
  NONE => 
  Debug.fail 
  ("UnitOps.makeEnv: dependency info missing for " ^ 
    EntityOps.description entity)

| SOME imports =>
  let
    fun add ([], result) = 
        (imports, result)

        (* For signatures just add to the SML basis *)
      | add (entity::entities, (B, strTys, tynameTys)) =
        case (entity, Entity.Map.find(!UnitManager.cache, entity)) of
        ((Entity.Sig, sigid), SOME (Sig sigma, _)) =>
        let
          val G = if Entity.Set.member(imports,entity) then Map.singleton (sigid, sigma)
		   else Map.empty 
        in
          add (entities, (EnvOps.BplusG B G, strTys, tynameTys))
        end

        (* For functors just add to the SML basis *)
      | ((Entity.Fun, funid), SOME (Fun f, _)) =>
        let
          val F = if Entity.Set.member(imports,entity) then Map.singleton (funid, f) else Map.empty 
        in
          add (entities, (EnvOps.BplusF B F, strTys, tynameTys))
        end

        (* For structures add to everything *)
      | ((Entity.Str, strid), 
          SOME (Str {E, tynameTys = tynameTys', ...}, _)) =>
        let
	  val SE = if Entity.Set.member(imports,entity) then Map.singleton (strid, E) else Map.empty
          val B = EnvOps.BplusE B (EnvOps.SEinE SE)

          (* Accumulatively apply the type map and union in *)
          val tyTys = TyName.Map.foldri (fn (tyname,ty,m) =>
            MILTy.Map.insert(m, MILTy.tyname tyname, ty)) 
            MILTy.Map.empty tynameTys
          val tynameTys = TyName.Map.unionWith #1 (tynameTys,
            TyName.Map.map (MILTy.replace tyTys) tynameTys')

          val strTys = if Entity.Set.member(imports,entity) then 
	                  let val ty = TransType.transE tynameTys E
			  in Map.insert(strTys, strid, ty) 
			  end
		       else strTys
        in
          add (entities, (B, strTys, tynameTys))
        end

      | _ => 
        Debug.fail ("SepComp.makeEnv:add: elaboration info missing for "
        ^ EntityOps.description entity)

in
 (*@TODO: remove diagnostics *)
  (*PrintManager.print 
    ("\n" ^ (EntityOps.description entity) ^  Pretty.simpleVec "\n   ," EntityOps.description (rev prefix)); *)
  add (rev prefix,(initialB,Map.empty,TyName.Map.empty))
end

end (* of local open *)
end (* of struct *)

