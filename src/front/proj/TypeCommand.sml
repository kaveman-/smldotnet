structure TypeCommand : sig end = 
struct
      fun findBasis (longid) =
	   case longid of [] => NONE
	      | longid as (id::ids) =>
		let 
		    fun lookup(B, []) = SOME B
		      | lookup((T,FE,GE,Env.Env(SE,TE,VE),path),[id]) =
			  let fun filter(env) = case Symbol.Map.find(env,id) of
			                           SOME ty => Symbol.Map.singleton(id,ty)
						 | NONE => Symbol.Map.empty
			  in
			       SOME (T,
				     filter(FE),
				     filter(GE),
				     Env.Env(filter(SE),
					     filter(TE),
					     filter(VE)),
				     path@[id])
			  end
		      | lookup ((T,F,G,E,path), id :: longid) =
			(case Symbol.Map.find(EnvOps.SEofE E, id) of
			     NONE => NONE 
			   | SOME E => lookup((T,Symbol.Map.empty,Symbol.Map.empty,E,path@[id]),longid))
(*		    fun mkUnitEnv(entityType,envFromUnitType) =
		            let val entityRef = (entityType,id)
			    in
			    case SourceManager.fileRefFor entityRef of
			         NONE => Symbol.Map.empty
			       | SOME  (fileRef,importedStrids) =>
				 case UnitManager.lookup(entityRef,fileRef) of
				     SOME unitType => envFromUnitType unitType
				   | NONE => Symbol.Map.empty
			    end
*)

		    fun mkUnitEnv(entityType,envFromUnitType) =
		            let val entityRef = (entityType,id)
			    in
			    case DepManager.dep entityRef of
			          DepManager.Success  (smallSyntax,fileRef) =>
				    (case UnitManager.lookup(entityRef,fileRef) of
					 SOME unitType => envFromUnitType unitType
				       | NONE => Symbol.Map.empty)
				| DepManager.NotFound => Symbol.Map.empty
				| DepManager.ParseError => Symbol.Map.empty
			    end


                    (* defining usedClassesOrPackages as follows 
		       ensures we bring in any sibling constructors and child static methods of longid *)

                    val usedClassesOrPackages = Longid.Set.add(Longid.Set.singleton(List.rev (List.tl (List.rev longid))),
							       longid)
		    val initialE = SepCompEnv.getImportedEnv {classes = usedClassesOrPackages,
							      packages = usedClassesOrPackages}

		    val B = EnvOps.BplusE (TopEnv.initialB ()) initialE

		    val B = EnvOps.BplusF B (mkUnitEnv(Entity.Fun,
						      fn UnitTypes.Fun(ty) => Symbol.Map.singleton(id,ty)
						       | _  => Symbol.Map.empty))
		    val B = EnvOps.BplusG B (mkUnitEnv(Entity.Sig,
						      fn UnitTypes.Sig(ty)=> Symbol.Map.singleton(id,ty)
						       | _ => Symbol.Map.empty))
		    val B = EnvOps.BplusE B (Env.Env(mkUnitEnv(Entity.Str,
							fn UnitTypes.Str({E=ty,...})=> Symbol.Map.singleton(id,ty)
							 | _ => Symbol.Map.empty),
					             Symbol.Map.empty,
						     Symbol.Map.empty))
					     

		in  lookup(B,longid) 
		end

      fun act root arg = 
	  case arg of
	      [((arg,NONE))] =>
		  (ignore(SourceManager.sync());(*@TODO: review *)
		   case findBasis (map Id.fromString (String.fields (fn #"." => true | _ => false) arg)) of
		       SOME B =>
			   (Commands.mustQuit := true; 
                            PrintManager.println (EnvOps.BasSig B);
			    OS.Process.success)
		     | NONE => 
			   (PrintManager.println ("not found or not yet compiled");
			    OS.Process.failure))
	  | _ => (PrintManager.println "unexpected argument to command";
		  OS.Process.failure)
      

      val _ = Commands.add "type"
	  {
	   act = act,
	   query = fn () => "nothing to query for type",
	   syntax = "<longid>",
	   help = "type <longid>\n\
	    \  type show any type info for <longid>"
	   }
end
