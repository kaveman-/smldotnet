(*======================================================================*)
(* Process the export command.						*)
(*======================================================================*)
structure ProcessExports :> PROCESSEXPORTS =
struct

open CompileOps
structure Map = Symbol.Map

fun printExportError s = ErrorManager.printErrorString ("export: error : "^s)

(*----------------------------------------------------------------------*)
(* Look up a type identifier in the structure (expecting a class type)  *)
(*----------------------------------------------------------------------*)
fun lookupTyCon (E, [tycon]) = 
    Map.find (EnvOps.TEofE E, tycon)

  | lookupTyCon (E, strid :: longtycon) =
    (case Map.find(EnvOps.SEofE E, strid) of
      NONE => NONE

    | SOME E => lookupTyCon (E, longtycon))


(*----------------------------------------------------------------------*)
(* Process a list of export declarations.				*)
(*----------------------------------------------------------------------*)
fun process exports =
let

  val names = ref ([] : (TyName.TyName * (Longid.longid * int)) list)
  val mainClass = ref (NONE : string option)

  (* Fake entity used for type names and class types for structures *)
  val entity = (Entity.Str, Id.fromString "_export")

  (* Type name and variable supplies *)
  val tynamesupply = ref (TyName.initial entity)
  val varsupply = ref Var.initial
  val tyvarsupply = ref Var.initial

  fun freshVar () = 
  let
    val (s, v) = Var.fresh (!varsupply)
  in
    varsupply := s; v
  end

  fun freshTyName longid =
  let
    (*@todo akenn: CLASS! *)
    val (n, s) = TyName.fresh (longid, TyName.NotEq) (!tynamesupply)
  in
    tynamesupply := s; n
  end

  open MILTerm 

  fun exportStructures isExternal [] abstr =
    (UnitManager.cache := Entity.Map.insert(!UnitManager.cache, entity,
      (UnitTypes.Str 
      {
        E = EnvOps.emptyE,    (* should be ignored anyway *)
        supply = !varsupply,
        tyvarsupply = !tyvarsupply,
        term = abstr,
        tynameTys = TyName.Map.empty
      }, NONE)); SOME (!names, !mainClass))

  | exportStructures isExternal ((strid,classname,strvar, tyname, E)::rest) 
    (boundvars, term) =
let
  val TE = EnvOps.TEofE E
  val SE = EnvOps.SEofE E
  val VE = EnvOps.VEofE E

  val SOME (UnitTypes.Str { tynameTys, E, ... },_) = 
    Entity.Map.find(!UnitManager.cache, (Entity.Str, strid))
  val SOME miltys = MILTy.fromProd (TransType.transE tynameTys E)
  val n = length miltys

  fun canExportArg ty = InterOp.isInterop ty andalso InterOp.isImportable isExternal ty
  fun canExportRes ty = InterOp.isInterop ty andalso InterOp.isExportable isExternal ty
  fun fromProd ty = 
    case SMLTy.fromProd ty of
      NONE => [ty]
    | SOME tys => tys


  fun processValBind (i, [], _, fields, methods, clinit) =
      let
	(* methdef is the exported class's class constructor
	   Before initializing the exported class via clinit, it executes globalInitPrim to initializes the Globals class,
	   Currently, globalInitPrim is compiled as a call to a dummy method Globals::globalInitPrim,
	   whose execution forces the initializiation of the Globals class.
	   We don't call Global's class constructor explicitly to ensure it is called exactly once.
	*)

        val methdef =   
        (
          RuntimeNames.classConstructor,
          [], (*@TODO: consider non-empty attributes *)
	  publicstatic, 
          [], NONE,
          SOME ((freshVar (), []), ([], Let(Special((Ext.Prim (RepNames.globalInitPrim),NONE,NONE),[],MILTy.cmp (Effect.any, [])),
						      ([],
						      clinit))))
        )
      in
        exportStructures isExternal rest
        (boundvars, LetClass(MILTy.tyname tyname, 
			     ([](*attributes*),
			      publicsealed, 
			      NONE, 
			      []),
			     fields, methdef::methods, term))
      end

    | processValBind (i, (name, bind)::rest, miltys, fields, methods, clinit) =
      case bind of
        ValBind.VarSch(SMLSch.TypeScheme([], ty)) =>
        (case SMLTy.fromFunType ty of
          NONE => 
          if canExportRes ty
          then
          let
            val milty::miltys = miltys
            val flddef = 
              (
                name, 
                publicstaticinitonly, 
                milty,
                NONE
              )
            val initvar = freshVar ()
            val clinit = LetVal((initvar, []), Proj(i, n, Var strvar), 
              Let(Special((Ext.PutField, SOME (MILTy.tyname tyname),
                SOME name), [Var initvar],
                MILTy.cmp (Effect.writes, [])), ([], clinit)))
          in
            processValBind(i+1, rest, miltys, flddef::fields, methods, clinit)
          end
          else (printExportError ("Structure " ^ Id.toString strid ^ 
            " has val binding for " ^ Id.toString name ^ 
            " whose type cannot be exported"); NONE)
            
        | SOME (smlargty, resty) =>
          let
            val argtys = fromProd smlargty                   
            val void = null (fromProd resty)
          in
            if List.all canExportArg argtys andalso 
              (canExportRes resty orelse void)
            then
            let
              val milty::miltys = miltys
              val SOME ([argty], cty) = MILTy.fromArrow milty
              val argtys = 
                case MILTy.fromProd argty of
                  SOME tys => tys
                | NONE => [argty]
                  
              val (_,[rty]) = MILTy.fromCmp cty
              val restyopt = 
                case MILTy.fromProd rty of
                  SOME [] => NONE
                | _ => SOME rty

              val argvars = map (fn ty => (freshVar (), [])) argtys
              val funvar = freshVar ()
              val term =
                case argvars of
                  [] =>
                  App(Var funvar, [Tuple []])

                | [argvar] =>
                  App(Var funvar, [Var (#1 argvar)])

                | _ => 
                  let
                    val prodvar = freshVar ()
                  in
                    LetVal((prodvar, []), Tuple (map (Var o #1) argvars),
                      App(Var funvar, [Var prodvar]))
                  end
          
              val term = 
                if isSome restyopt then term 
                else Let(term, ([(MILTermOps.dummyBoundVar, MILTy.prod [])], Triv []))

	      val isMain = (Symbol.equal(name, Id.fromString "main") 
			    orelse Symbol.equal(name, Id.fromString "Main"))
	      val isEntry = isMain andalso
		            (SMLTy.eq(smlargty, SMLPrimTy.unitType)
			     orelse
			     SMLTy.eq(smlargty, SMLPrimTy.optionType 
				      (SMLTy.arrayType 
				       (SMLPrimTy.optionType SMLPrimTy.stringType))))
			    andalso (void orelse SMLTy.eq(resty, SMLPrimTy.intType))
	      val methdef = (name,
			     [], (*@TODO: consider non-empty attributes *)
			     if isEntry
				 then Symbol.Set.add(publicstatic,entrypoint)
			     else publicstatic,
				 argtys, 
				 restyopt,
				 SOME (
				       (freshVar (),[]), 
				       (argvars, LetVal((funvar, []), Proj(i, n, Var strvar), term)))
				 )

            in
	      if isEntry 
	      then mainClass:= SOME classname 
	      else if isMain then
(*@TODO: produce real error*)
		   printExportError (" structure " ^ Id.toString strid ^ 
						  " has potential entry-point " ^ Id.toString name ^ 
						  " of incorrect type  " ^ SMLTy.toString ty ^ " (ignored)")
		   else ();
	      processValBind(i+1, rest, miltys,fields,methdef::methods,clinit)
            end
            else 
              (printExportError (" structure " ^ Id.toString strid ^ 
                 " has val binding for " ^ Id.toString name ^ 
                 " whose type cannot be exported"); NONE)
          end
        )
      | ValBind.Special _ =>  
	     (* silently skip special *)
	     (*@TODO: error if class not exported *)
	     processValBind(i, rest, miltys,fields,methods,clinit)
      | _ => 
       (printExportError (" structure " ^ Id.toString strid ^ 
          " has binding for " ^ Id.toString name ^ 
          " that cannot be exported"); NONE)
in
  if Map.numItems SE > 0 (*@TODO: remove orelse Map.numItems TE > 0 *)
  then 
  (printExportError (" structure " ^ Id.toString strid ^ 
    " contains type or structure bindings; cannot export"); NONE)
  else processValBind (0, Id.fixMap VE, miltys, [], [], Triv [])
end


  (*..................................................................*)
  (* Process the declarations					      *)
  (*..................................................................*)
  fun process' ([], tynames, classdefs, strdefs) = 
      let
        fun isExternal tn = 
          List.exists (fn (tn',name) => 
		           TyName.eq(tn,tn')
		       ) tynames

        val errors = List.mapPartial (InterOp.isExportableClassType isExternal) classdefs 
      in
        case errors of
          [] =>
          (names := tynames; 
           exportStructures isExternal strdefs
           (map (fn (strid,_,x,_,_) => (x,[strid])) strdefs, Triv [Tuple []]))

        | messages => 
          (app printExportError messages; NONE)
      end

    | process' (([strname],classname)::rest, tynames, classdefs, strdefs) =
      let
	val strid = Id.fromString strname
	val classlongid = map (Id.fromString) (String.fields (fn c => c = #".") classname)
      in
    (*..................................................................*)
    (* Look up the structure					        *)
    (*..................................................................*)
      case SepCompEnv.getE (Entity.Str, strid) of
        NONE => 
        (print ("\nNo such structure: " ^ Id.toString strid); 
         NONE)

      | SOME E =>
	let
          val depth = 0
          val tyname = freshTyName [strid]
	  val TE = EnvOps.TEofE E
	  val (classdefs,tynames,errs) = 
	      Symbol.Map.foldli (fn(tycon,tystr,(classdefs,tynames,errs))=> 
			       case TyStr.fromClasstype tystr of
				    NONE =>
					(classdefs,tynames,
					 ("exported type is not a class type: " ^ Longid.toString [strid,tycon])::errs)
				  | SOME (c as SMLTy.MLClass { tyname, ... }) => 
				       (c::classdefs,(tyname,(classlongid@[tycon],depth + 1))::tynames,errs))
				(classdefs,tynames,[]) TE
        in
          if null errs then
	    process'(rest, (tyname,(classlongid,depth))::tynames, 
		     classdefs, (strid,classname,freshVar(),tyname,E)::strdefs)
  	  else (app printExportError errs; 
		NONE)
        end
      end

in
  process' (exports, [], [], [])
end

end

