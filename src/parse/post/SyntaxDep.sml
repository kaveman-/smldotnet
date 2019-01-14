(*======================================================================*)
(* Dependency analysis of structures and signatures.			*)
(*======================================================================*)
structure SyntaxDep :> SYNTAXDEP =
struct

open SmallSyntax 

(*----------------------------------------------------------------------*)
(* Sets of entity references						*)
(*----------------------------------------------------------------------*)
infixr 5 ++
val op++ = Entity.Set.union
val empty = Entity.Set.empty
fun singleSigid sigid = Entity.Set.singleton (Entity.Sig, sigid)

(*----------------------------------------------------------------------*)
(* Where did a particular environment come from?			*)
(*----------------------------------------------------------------------*)
datatype Source =
  SourceStruct			(* SML structure *)
| Class of Syntax.longid	(* external class *)
| Package of Syntax.longid	(* external package *)

(*----------------------------------------------------------------------*)
(* Structure/functor environments: the `types' of structures            *)
(* and functors and the semantic analogue of signatures.	        *)
(*----------------------------------------------------------------------*)
datatype Env = Env of (Source*Env) Symbol.Map.map
(*----------------------------------------------------------------------*)
(* The information obtained from dependency analysis; see signature.	*)
(*----------------------------------------------------------------------*)
type Info = 
{
  order : Entity.Ref list,
  deps : Entity.Set.set Entity.Map.map,
  classes : Longid.Set.set,
  packages : Longid.Set.set
}

exception Cycle

(*----------------------------------------------------------------------*)
(* The empty environment						*)
(*----------------------------------------------------------------------*)
val emptyEnv = Env Symbol.Map.empty

(*----------------------------------------------------------------------*)
(* Add a binding to an environment					*)
(*----------------------------------------------------------------------*)
fun add (Env env, id, env') = Env (Symbol.Map.insert(env, id, env'))

(*----------------------------------------------------------------------*)
(* Merge e1 and e2, with e2 over-riding e1.				*)
(*----------------------------------------------------------------------*)
fun extend (Env e1, Env e2) = Env (Symbol.Map.unionWith #2 (e1,e2))

(*----------------------------------------------------------------------*)
(* Do a dependency analysis starting at the root structures specified.  *)
(*----------------------------------------------------------------------*)
fun analyse ids =
let

(*----------------------------------------------------------------------*)
(* Convert a package map into an environment				*)
(*----------------------------------------------------------------------*)
(*
fun convert longid (PackageManager.Package { classes, packages }) =
  Env(
    Symbol.Map.unionWith #2
    (Symbol.Map.mapi (fn (id, ref p) => 
       (Package (longid @ [id]), convert (longid @ [id]) p)) packages,
     Symbol.Map.mapi (fn (id, _) => 
       (Class (longid @ [id]), emptyEnv)) classes))
*)
fun convert longid (PackageManager.Package { classes, packages }) =
  Env(let val packageEnv = 
      Symbol.Map.mapi (fn (id, ref p) => 
	(Package (longid @ [id]), convert (longid @ [id]) p)) 
      packages
      val classEnv  = 
      Symbol.Map.mapi (fn (id, _) => 
	(Class (longid @ [id]), 
	 case Symbol.Map.find(packageEnv,id) of 
	     SOME (_,env) => env 
	   | NONE => emptyEnv ))
	 classes
  in
     Symbol.Map.unionWith #2 (packageEnv,classEnv)
  end)

(*----------------------------------------------------------------------*)
(* Initial environment for checking structures and functors.		*)
(*----------------------------------------------------------------------*)
val initialEnv = convert [] (PackageManager.getTop ())

(*----------------------------------------------------------------------*)
(* The initial environment for checking signatures includes Int, Real,  *)
(* etc									*)
(*----------------------------------------------------------------------*)
val initialEnvForSig = 
  extend (initialEnv,
  Env 
  (    
    Symbol.Map.foldli 
    (fn (strid, _, env) => 
      Symbol.Map.insert(env, strid, (SourceStruct,emptyEnv)))
    Symbol.Map.empty
    (TopEnv.initialSE ())
  ))

type TopInfo =
{
  E : Env,
  entities : Entity.Set.set,
  sourcemap : SourceMap.sourcemap option,
  errors : Error.Error list
}
  
val order = ref ([] : Entity.Ref list)
val deps = ref (Entity.Map.empty : TopInfo Entity.Map.map)
val classes = ref Longid.Set.empty
val packages = ref Longid.Set.empty
val badParse = ref false

(*----------------------------------------------------------------------*)
(* Look up a structure/signature identifier in an environment.		*)
(*----------------------------------------------------------------------*)
fun lookup (Env e, id) = Symbol.Map.find(e, id)

(*----------------------------------------------------------------------*)
(* Look up an entity reference in the top-level environment.		*)
(* Note that structures and packages share the same entities.           *)
(*----------------------------------------------------------------------*)
fun lookupTop entity = Entity.Map.find(!deps, entity)

(*----------------------------------------------------------------------*)
(* Add a top-level entity to the accumulated dependency info.		*)
(*----------------------------------------------------------------------*)
fun addTop (entity, E, entities) =
    (order := entity :: !order;
    case Entity.Map.find(!deps, entity) of
      NONE => 
      deps := Entity.Map.insert(!deps, entity, 
        { E = E, entities = entities, sourcemap = NONE, errors = [] })

    | SOME { errors, ... } =>
      deps := Entity.Map.insert(!deps, entity, 
        { E = E, entities = entities, sourcemap = NONE, errors = errors }))

(*----------------------------------------------------------------------*)
(* There's been a parsing error for this entity.			*)
(*----------------------------------------------------------------------*)
fun parseError entity = 
  (badParse := true;
(*
   print (" " ^ EntityOps.description entity);
*)
   deps := Entity.Map.insert(!deps, entity, 
     { E = emptyEnv, entities = empty, sourcemap = NONE, errors = [] }))

val loc = {left=0,right=0}
fun depError (NONE, message) =
    (PrintManager.print message; badParse := true)

  | depError (SOME entity, message) = 
    (case Entity.Map.find(!deps, entity) of
      SOME { entities, E, sourcemap, errors } =>
      (badParse := true;
      deps := Entity.Map.insert(!deps, entity,
        { entities = entities, E = E, sourcemap = sourcemap,
          errors = Error.error(loc, message) :: errors }))

    | NONE =>
      (badParse := true;  
      deps := Entity.Map.insert(!deps, entity,
        { E = emptyEnv, entities = empty, sourcemap = NONE, 
          errors = [Error.error(loc, message)] })))

fun printErrors () =
(PrintManager.print "\n"
(*
Entity.Map.appi
  (fn (entity, { sourcemap, errors, ... }) =>
    case (errors, sourcemap) of
      (_::_, SOME sm) => 
      (PrintManager.print (EntityOps.description entity ^ ":");
       PrintManager.printErrors (sm, errors))

    | _ => ())
  (!deps)
*)
)

fun testCycle (entity, pending, sourcemap) =
let
  fun find acc [] = ()
    | find acc (entity'::rest) =
      if EntityOps.eq(entity,entity')
      then 
      (
        PrintManager.println (
        EntityOps.description entity ^ " is in a circular definition with " ^
        Pretty.vec ("itself", "", "", "", "", " and ") EntityOps.description
        acc); raise Cycle
      )
      else find (entity'::acc) rest
in
  find [] pending
end

(*----------------------------------------------------------------------*)
(* Analyse a single declaration.					*)
(*----------------------------------------------------------------------*)
fun analyseDecItem (env, pending, sourcemap) (decitem : DecItem) = 
case decitem of
  Local(dec1, dec2) =>
  let
    val (env1, refs1) = analyseDec (env, pending, sourcemap) dec1
    val (env2, refs2) = analyseDec (extend(env, env1), pending, sourcemap) dec2
  in
    (env2, refs1 ++ refs2)
  end

| Structure bindings =>
  foldl (fn ((strid, strexp), (finalenv, refs)) =>
    let 
      val (strenv, strrefs) = analyseStrExp (env, pending, sourcemap) strexp
    in
      (add(finalenv, strid, (SourceStruct,strenv)), refs ++ strrefs)
    end) (emptyEnv, empty) bindings

| Open longids =>
  let val (_,env,refs) = foldl
    (fn (longid, (env, envresult, refs)) =>
    let 
      val (idenv, idrefs) = analyseLongid (env, pending, sourcemap) longid
    in
      (extend (env, idenv), extend (envresult, idenv), refs ++ idrefs)
    end) (env, emptyEnv, empty) longids
  in
    (env, refs)  
  end

| Mention mention =>
  (emptyEnv, analyseMention (env,pending,sourcemap) mention)


(*----------------------------------------------------------------------*)
(* Analyse a sequence of declarations.					*)
(*----------------------------------------------------------------------*)
and analyseDec (env, pending, sourcemap) [] = (emptyEnv, empty)
  | analyseDec (env, pending, sourcemap) (decitem::dec) = 
    let
      val (env1, refs1) = analyseDecItem (env, pending, sourcemap) decitem
      val (env2, refs2) = analyseDec (extend (env, env1), pending, sourcemap) dec
    in
      (extend(env1, env2), refs1 ++ refs2)
    end

(*----------------------------------------------------------------------*)
(* Analyse a structure expression.					*)
(*----------------------------------------------------------------------*)
and analyseStrExp (args as (env, pending, sourcemap)) strexp =
case strexp of
  Struct dec =>
  analyseDec args dec

| Strid strid =>
  analyseLongid args strid

| StrConstraint(strexp, sigexp) =>
  let val (strenv, strrefs) = analyseStrExp args strexp
      val (sigenv, sigrefs) = analyseSigExp args sigexp
  in
    (sigenv, strrefs ++ sigrefs)
  end

| FunApp(funid, strexp) =>
  let
    val (argenv, argrefs) = analyseStrExp args strexp
    val entity = (Entity.Fun, funid)
    val x = lookupTop entity
  in
    case x of
      SOME { E = resultenv, entities = refs, ... } => 
      (* Union in the functor's own references because we macro expand *) 
      (resultenv, Entity.Set.union(refs, Entity.Set.add(argrefs, entity)))

    | NONE =>
      (testCycle (entity, pending, sourcemap);
        case DepManager.dep entity of
          DepManager.Success (Local([openitem as Open _],[Functor [(_, spec, strexp)]]), _) => 
          let 
               val (env1, refs1) = analyseDecItem (initialEnv, entity::pending, sourcemap) openitem
	       val (env, resultrefs) = analyseFunExp 
                    (extend(initialEnv,env1), entity::pending, SOME (entity)) (spec,strexp)
          in
            addTop(entity, env, refs1++resultrefs);
            (env, 
             refs1 ++ resultrefs ++ Entity.Set.add(argrefs, entity))
          end

        | DepManager.Success _ =>
          (PrintManager.println ("Expected single functor definition in " ^
            EntityOps.description entity);
           parseError entity; 
           (emptyEnv, Entity.Set.add(argrefs, entity)))
        
        | DepManager.ParseError => 
          (parseError entity; (emptyEnv, Entity.Set.add(argrefs, entity)))

        | DepManager.NotFound =>
          (emptyEnv, Entity.Set.add(argrefs, entity))
      )
  end

| StrLet(dec, strexp) =>
  let
    val (env1, refs1) = analyseDec args dec
    val (env2, refs2) = analyseStrExp (extend(env, env1), pending, sourcemap) strexp
  in
    (env2, refs1 ++ refs2)
  end

(*----------------------------------------------------------------------*)
(* Analyse a signature expression.					*)
(*----------------------------------------------------------------------*)
and analyseSigExp (args as (env, pending, sourcemap)) sigexp =
case sigexp of
  Sigid sigid =>
  let 
    val entity = (Entity.Sig, sigid)
    val x = lookupTop entity
  in
    case x of
      SOME { E = env, entities = imports, ... } => 
      (env, Entity.Set.singleton entity)

    | NONE =>
      (testCycle (entity, pending, sourcemap);
        case DepManager.dep entity of
          DepManager.Success (Local([openitem as Open _],[Signature [(_,sigexp)]]), _) => 
          let 
            val (env1, refs1) = analyseDecItem (initialEnvForSig, entity::pending, sourcemap) openitem
            val (env, refs) = analyseSigExp 
              (extend(initialEnvForSig,env1), entity::pending, SOME (entity)) sigexp
          in
            addTop(entity, env, refs1++refs);
            (env, Entity.Set.singleton entity)
          end

        | DepManager.Success _ =>
          (PrintManager.println ("Expected single signature definition in " ^
            EntityOps.description entity);
           parseError entity; 
           (emptyEnv, Entity.Set.singleton entity))
       
        | DepManager.ParseError => 
          (parseError entity; 
          (emptyEnv, Entity.Set.singleton entity))

        | DepManager.NotFound => 
          (emptyEnv, Entity.Set.singleton entity)
      )
  end

| SigSpec spec =>
  analyseSpec args spec

| Where(sigexp, mentions) =>
  let
    val (sigenv, refs) = analyseSigExp args sigexp
    val refs' = analyseMention args mentions
  in
    (sigenv, refs ++ refs')
  end

(*----------------------------------------------------------------------*)
(* Analyse a functor expression						*)
(*----------------------------------------------------------------------*)
and analyseFunExp (env, pending, sourcemap) (spec, strexp) =
  let
    val (env1, refs1) = 
      analyseSpec (env, pending, sourcemap) spec
    val (env2, refs2) = 
      analyseStrExp (extend (env, env1), pending, sourcemap) strexp
  in
    (env2, refs1 ++ refs2)
  end

(*----------------------------------------------------------------------*)
(* Analyse a sequence of specifications.				*)
(*----------------------------------------------------------------------*)
and analyseSpec (env, pending, sourcemap) [] = 
    (emptyEnv, empty)

  | analyseSpec (env, pending, sourcemap) (specitem::spec) = 
    let
      val (env1, refs1) = 
        analyseSpecItem (env, pending, sourcemap) specitem
      val (env2, refs2) = 
        analyseSpec (extend (env, env1), pending, sourcemap) spec
    in
      (extend(env1, env2), refs1 ++ refs2)
    end

(*----------------------------------------------------------------------*)
(* Analyse a single specification.					*)
(*----------------------------------------------------------------------*)
and analyseSpecItem (args as (env, pending, sourcemap)) specitem =
case specitem of
  StructureDesc bindings =>
  foldl (fn ((strid, sigexp), (finalenv, refs)) =>
    let 
      val (sigenv, sigrefs) = analyseSigExp (env, pending, sourcemap) sigexp
    in
      (add(finalenv, strid, (SourceStruct,sigenv)), refs ++ sigrefs)
    end) (emptyEnv, empty) bindings 

| Include sigexp =>
  analyseSigExp args sigexp

| SpecMention mention =>
  (emptyEnv, analyseMention args mention)
  
(*----------------------------------------------------------------------*)
(* Analyse a single long identifier.					*)
(* Resolution is as follows:                                            *)
(*   (1) Local -- already in the environment;                           *)
(*   (2) Global structure/package: for an identifier strid.longid       *)
(*       EITHER a top-level structure strid must exist;                 *)
(*       OR     a top-level package strid must exist.                   *)
(*----------------------------------------------------------------------*)
and analyseLongid (env, pending, sourcemap) (longstrid as id::ids) = 
let
  (* Analyse a longid that starts with a non-top-level identifier *)
  fun analyseLocal result [] = result
    | analyseLocal ((_,env), refs) (id::ids) =
      case lookup(env, id) of
        (* This could be flagged as a missing substructure error *)
        NONE => 
        ((SourceStruct, emptyEnv), refs)

      | SOME pair => 
        analyseLocal (pair, refs) ids

  val entity = (Entity.Str, id)

  val ((source,env), refs) = 
  case lookup(env, id) of
    SOME pair => 
    analyseLocal (pair, empty) ids

  | NONE => 
    case lookupTop entity of
      SOME { E = env, entities = refs, ... } =>
      analyseLocal ((SourceStruct,env), Entity.Set.singleton entity) ids

    | NONE =>      
      (testCycle (entity, pending, sourcemap);
        case DepManager.dep entity of
          DepManager.Success (Structure [(_,strexp)], _) => 
          let 
            val (env, refs) = analyseStrExp 
              (initialEnv, entity::pending, SOME (entity)) strexp
          in
            addTop(entity, env, refs);
            analyseLocal 
              ((SourceStruct,env), Entity.Set.singleton entity) ids
          end

        | DepManager.Success _ =>
          (PrintManager.println ("Expected single structure definition in " ^
            EntityOps.description entity);
           parseError entity; 
           ((SourceStruct, emptyEnv), Entity.Set.singleton entity))
       
        | DepManager.ParseError =>
          (parseError entity; 
          ((SourceStruct, emptyEnv), Entity.Set.singleton entity))

        | DepManager.NotFound => 
          ((SourceStruct, emptyEnv), Entity.Set.singleton entity)
     )
in
  (case source of
    Class longid => (classes := Longid.Set.add(!classes, longid))
  | Package longid => (packages := Longid.Set.add(!packages, longid)) 
  | SourceStruct => ());
  (env, refs)
end

and analyseMention args longids =
  Longid.Set.foldl 
    (fn (mention, refs) =>
      let val (_, refs') = analyseLongid args mention
      in refs ++ refs' end) empty longids


in
  (map (fn id => analyseLongid (initialEnv, [], NONE) [Id.fromString id]) ids;
  if !badParse 
  then (printErrors (); NONE)
  else SOME 
  { 
    deps = Entity.Map.map #entities (!deps), 
    order = !order, 
    classes = !classes,
    packages = !packages
  }) handle Cycle => (printErrors (); NONE)

end

fun sync strids =
 if SourceManager.sync ()
 then
 PrintManager.process ("Analysing dependencies", true)
 (fn () => analyse strids)
 else NONE
        
end

