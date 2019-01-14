(*======================================================================*)
(* Separate compilation of an entity (structure, signature, functor)    *)
(*======================================================================*)
structure SepComp :> SEPCOMP =
struct

datatype result = Failure | Success | NoChange

open UnitTypes

open SepCompEnv

(*----------------------------------------------------------------------*)
(* Local transformations to apply					*)
(*----------------------------------------------------------------------*)
val opts = ref ["presimp", "arity1", "presimp"]

val [showSMLTerm, showSMLTypes, showSMLEnv, showModType,
     showImportTypes, showImports, showImportTypeDefs] =
    map (Controls.add false)
    ["showSMLTerm", "showSMLTypes", "showSMLEnv", 
     "showModType", "showImportTypes", "showImports", "showImportTypeDefs"]


(*----------------------------------------------------------------------*)
(* Dump the type name map						*)
(*----------------------------------------------------------------------*)
fun tynameTysToString tynameTys =
  Pretty.bigVec 0
  (fn (tyname, ty) => TyName.toString tyname ^ " = " ^ MILTy.toString ty)
  (TyName.Map.listItemsi tynameTys)


fun make { deps, order, classes, packages } =
let

val initialE = getImportedEnv { classes = classes, packages = packages }
val initialB = EnvOps.BplusE (TopEnv.initialB ()) initialE

(*----------------------------------------------------------------------*)
(* Actually do the elaboration and update the cache.			*)
(*----------------------------------------------------------------------*)
fun doelab (prefix,entity,openedstructures,topbinditem,sourcemap,fileref : Entity.FileRef) =
let
  val (imports,(B,strTys,tynameTys)) = 
    UnitOps.makeEnv (initialB, deps, prefix, entity)

  (* Optionally dump the import list to the log *)
  val _ = 
    if Controls.get showImports
    then Debug.print ("\n[" ^ EntityOps.description entity ^ " imports {" ^
    Pretty.simpleVec ", " EntityOps.description
    (List.filter (fn e => Entity.Set.member(imports, e)) prefix)
    ^ "}]")
    else ()
          
  (* Optionally dump the type defs to the log *)
  val _ = 
    if Controls.get showImportTypeDefs
    then Debug.print ("\n[" ^ EntityOps.description entity ^ 
      " imports type defs:\n" ^ tynameTysToString tynameTys)
    else ()

  val (result, errors) = 
  case topbinditem of
  (*..................................................................*)
  (* Signature elaboration					      *)
  (*..................................................................*)
    (loc,Syntax.Signature [sigbind]) =>
    let 
      val (sigdata as (_,sigma), errors) = 
      PrintManager.process ("Type checking " ^ 
        EntityOps.descriptionWithFile (entity, fileref), true)
        (fn () => Elab.infTopSigExp B openedstructures sigbind)
      
      (* Optionally dump the SML environment to the log *)
      val _ = 
        if Controls.get showSMLEnv 
        then Debug.print ("\nSML env = " ^ EnvOps.EtoString (#2 sigma))
        else ()
      
    in 
      if List.exists Error.isSerious errors
      then (NONE, errors)
      else (SOME (Sig sigma), errors)
    end

  (*..................................................................*)
  (* Structure elaboration					      *)
  (*..................................................................*)
  | (loc,Syntax.Structure [strbind]) =>
    let

      val ((strDE,psi,E,e), errors) = 
      PrintManager.process ("Type checking " ^ 
        EntityOps.descriptionWithFile (entity, fileref), true)
        (fn () => Elab.infTopStrExp B openedstructures strbind)

    in
      if List.exists Error.isSerious errors
      then (NONE, errors)
      else PrintManager.process 
      ("Compiling " ^ EntityOps.description entity, true)
      (fn () =>
      let
        (* Construct a map from structure identifiers to (var,ty) pairs *)
        val (SE, supply) =
          Entity.Set.foldr (fn (entity, (SE,supply)) => 
            case entity of
              (Entity.Str, strid) =>
              let val (supply,x) = Var.fresh supply
                  val SOME ty = Symbol.Map.find(strTys, strid)
              in
                (Symbol.Map.insert(SE, strid, (x,ty)), supply)
              end
            | _ => (SE, supply)) (Symbol.Map.empty, Var.initial) imports

        (* Translate the datatype environment *)
        val tynameTysDE = TransType.transDE tynameTys strDE

        val tynameTys = TyName.Map.unionWith #2 (tynameTys, tynameTysDE)
        
        (* Translate the realisation *)
        val tynameTysPsi = TransType.transRealisation tynameTys psi

        val tynameTys = TyName.Map.unionWith #2 (tynameTys, tynameTysPsi)

        (* Optionally dump the MIL types of the imported structures *)
        val _ = 
          if Controls.get showImportTypes
          then Debug.print ("\n[Imported structures:\n\n" ^
            Pretty.simpleVec "\n\n" (fn (id,(_,ty)) =>
            Id.toString id ^ " : " ^ MILTy.toString ty) 
            (Symbol.Map.listItemsi SE) ^ "\n")
          else ()

        (* Optionally dump the SML typed term to the log *)
        val _ = 
          if Controls.get showSMLTerm 
          then Debug.print ("\nSML term = " ^ SMLTermOps.toString e)
          else ()

        (* Optionally dump the SML environment to the log *)
        val _ = 
          if Controls.get showSMLEnv
          then Debug.print ("\nSML env = " ^ EnvOps.EtoString E)
          else ()
      
        (* Optionally dump the SML datatype environment to the log *)
        val _ = 
          if Controls.get showSMLTypes 
          then Debug.print ("\nSML types = " ^ SMLTy.DEtoString strDE)
          else ()

        (* Optionally dump the SML realisation to the log *)
        val _ = 
          if Controls.get showSMLTypes 
          then Debug.print ("\nSML realisation = " ^ 
            SMLTy.realisationToString psi)
          else ()     

        (* Translate the SML typed term into a MIL computation term *)
        val { term, cty, varsupply, tyvarsupply, errors = errors' } =
          PrintManager.process ("Translating", false)
          (fn () =>
            Trans.trans 
            { 
	      sourcemap = sourcemap,
              entity = entity,
              strexp = e,
              SE = SE,
              tynameTys = tynameTys,
              supply = supply
            })

      in
        if List.exists Error.isSerious errors'
        then (NONE, errors @ errors')
        else
        let

          (* Translate the SML environment into a MIL tuple type *)
          val ty = TransType.transE tynameTys E

	  val _ = 
            if Controls.get showModType
            then Debug.print ("\nMIL module type = " ^ MILTy.toString ty)
            else ()

          (* Construct a type environment under which to transform the term *)
          val tyenv = Symbol.Map.foldr (fn ((x, ty), tyenv) =>    
              Var.Map.insert(tyenv, x, ty)) Var.Map.empty SE

          val (term,supply) = Opts.apply (!opts) tyenv (term,varsupply)
          val boundvars = 
            map (fn (strid,(x,ty))=> (x,[strid])) (Symbol.Map.listItemsi SE)
        in
          (SOME (Str 
           { E = E, 
             supply = supply, 
             tyvarsupply = tyvarsupply,
	     (*@BUG: *)
	     (*@TODO: review 
              crusso: I don't think the interal realisation should be      
              persisted as part of tynameTysPsi, but separately,
              otherwise the result of translation is not implementation 
              independent, as it needs to be for separate compilation.
              Fortunately, it appears that an entity is consider dirty
              even when its implementation, but not its interface has  changed.
	      *)
             tynameTys = TyName.Map.unionWith #2 (tynameTysDE, tynameTysPsi), 
             term = (boundvars,term) }),
          errors @ errors')
        end
      end)
    end

  | (loc,Syntax.Functor [funbind]) =>
    let

      (* Elaborate the functor *)
      val (Phi, errors) = 
      PrintManager.process ("Type checking " ^ 
        EntityOps.descriptionWithFile (entity, fileref), true)
        (fn () => Elab.infTopFunExp B openedstructures funbind)

    in
      if List.exists Error.isSerious errors
      then (NONE, errors)
      else (SOME (Fun Phi), errors)
    end

  | _ =>
    Debug.fail ("SepComp.doelab: " ^ EntityOps.description entity ^
    " not bound")
in
  ErrorManager.printErrors (sourcemap, errors);
  result
end

fun parseAndExtract (entity,fileref) =
case ParseManager.parse fileref of
  ParseManager.Success (topbind,sourcemap) =>
  (case SyntaxCheck.find (topbind,entity) of
    NONE =>
    NONE

  | SOME topbinditem =>
     SOME (topbinditem,sourcemap)
  )

| ParseManager.NotFound =>
  Debug.fail "SepComp.elab: missing file"

| ParseManager.Fail =>
  NONE

(*----------------------------------------------------------------------*)
(* Has the environment to which an entity elaborates changed?           *)
(* If so, then all dependent entities must be recompiled.               *)
(*----------------------------------------------------------------------*)
fun infoChanged (Sig (_,E1), Sig(_,E2)) = 
    not (EnvOps.eq (E1, E2))

  | infoChanged (Str { E = E1, ... }, Str { E = E2, ... }) =
    not (EnvOps.eq (E1, E2))

  | infoChanged (Fun(_,fr1,_), Fun (_,fr2,_)) =
    not (EntityOps.fileRefEq (fr1,fr2))

  | infoChanged _ = true


val primStr = (Entity.Str, Id.fromString "Prim")
val primSig = (Entity.Sig, Id.fromString "PRIM")

(*----------------------------------------------------------------------*)
(* Type check, translate and transform entities in dependency order	*)
(*----------------------------------------------------------------------*)
(*@BUG: review  *)
(*@TODO:
  crusso: acc to akenn, an entity is wrongly, but soundly, considered dirty even when its implementation, but not its interface has
         changed (see related @BUG above).
*)
fun elab (prefix, []) changed = 
    (if Entity.Set.isEmpty changed then NoChange
     else Success)
  | elab (prefix, entity::entities) changed =
    case SourceManager.fileRefFor entity of
      NONE =>
      (PrintManager.println ("Cannot find " ^ EntityOps.description entity); 
       Failure)

    | SOME (fileref, openedstructures) =>
      case UnitManager.lookup(entity, fileref) of
      (* If it's in the cache then only elaborate if it's out of date or
         in the changed set *)
        SOME oldinfo =>
        let
          (* It will be in the dependency map unless it's a primitive *)
          val imports = 
            case Entity.Map.find(deps, entity) of
              SOME imports => imports
            | NONE => Entity.Set.empty

          val skip = Entity.Set.isEmpty (Entity.Set.intersection(changed, 
            imports))
        in
          if skip then elab (entity::prefix, entities) changed
          else 
          case parseAndExtract (entity, fileref) of
            NONE => Failure
          | SOME (topbinditem, sourcemap) =>
            (case doelab (prefix, entity, openedstructures, topbinditem, sourcemap, fileref) of
              NONE => 
              Failure
            | SOME result =>
              (UnitManager.update (entity, SOME fileref, result);
              elab (entity::prefix, entities) 
                   (if infoChanged (oldinfo, result)
                    then Entity.Set.add(changed, entity) else changed))
            )
        end

      | NONE =>
        if EntityOps.eq(entity, primStr)
        then
        let
          val SOME E = getE primSig
          val (imports,(B,strTys,tynameTys)) = 
            UnitOps.makeEnv (initialB, deps, prefix, primSig)
          val entry = SepPrim.makePrimEntry (tynameTys, E)
        in
          UnitManager.update(entity, NONE, entry);
          elab (entity::prefix, entities) changed
        end

        else
        case parseAndExtract (entity, fileref) of
          SOME (topbinditem, sourcemap) =>
          (case doelab (prefix, entity, openedstructures, topbinditem, sourcemap, fileref) of
            NONE => Failure
          | SOME result => 
            (UnitManager.update(entity, SOME fileref, result);
            elab (entity::prefix, entities) (Entity.Set.add(changed, entity)))
          )

        | NONE => Failure

in
  elab ([], rev order) Entity.Set.empty
end

end (* of struct *)

