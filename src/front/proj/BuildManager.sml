structure BuildManager =
struct
    local 
    open Pickle


    val time = wrap (Time.fromSeconds o Int32.toLarge, 
	             Int32.fromLarge o Time.toSeconds) int32	

    val fileRef = pair(pair(string,time),
	               list IdPickle.longid)

    val fileRefMap =
	wrap (foldl Entity.Map.insert' Entity.Map.empty, Entity.Map.listItemsi) 
	     (list (pair (EntityOps.pickler, fileRef)))

    val translation = 
	wrap (foldl Entity.Map.insert' Entity.Map.empty, Entity.Map.listItemsi) 
	     (list (pair (EntityOps.pickler, string))) 

    val entitySet =   wrap (fn l => Entity.Set.addList(Entity.Set.empty,l),
	                    Entity.Set.listItems)
	                   (list EntityOps.pickler)

    val depsOpt =
       option 
	 (wrap (fn (order,deps,classes,packages) => {order=order,deps=deps,classes=classes,packages=packages},
	        fn {order,deps,classes,packages} => (order,deps,classes,packages))
	    (quadruple (list EntityOps.pickler,
			wrap (foldl Entity.Map.insert' Entity.Map.empty, 
			      Entity.Map.listItemsi) 
			     (list (pair (EntityOps.pickler, entitySet))),
			wrap (fn l => Longid.Set.addList(Longid.Set.empty,l),
			      Longid.Set.listItems)
			     (list IdPickle.longid),
			wrap (fn l => Longid.Set.addList(Longid.Set.empty,l),
			      Longid.Set.listItems)
			     (list IdPickle.longid))))
                     
    val pickler = wrap ((fn (references,projPath,basisPath,translation,fileRefMap,depsOpt) =>
		           {references = references,
			    projPath = projPath,
			    basisPath = basisPath,
			    translation = translation,
	                    fileRefMap = fileRefMap,
                            depsOpt=depsOpt}),
			(fn {references,projPath,basisPath,translation,fileRefMap,depsOpt} =>
		           (references,
			    projPath,
			    basisPath,
			    translation,
	                    fileRefMap,
			    depsOpt)))
	               (sextuple (list (pair (string,string)),
				  list (pair (string,list string)),
				  list (pair (string,list string)),
				  translation,
	                          fileRefMap,
				  depsOpt))


    (* Format number: hi-byte is version no., lo-byte is BD [B]uil[D]Manager*)
    fun makePersister filename = 
	Pickle.persist (Word8Vector.fromList [0wx04, 0wxBD], filename, pickler)

    in

    local 
	val lastModTimeRef = ref (Time.now())
    in
    fun changed () =
	let val filename = OS.Path.joinDirFile { dir = RuntimeEnv.getCompilerToolDir(), file = RuntimeNames.buildFile }
	in
	    if OS.FileSys.access(filename, [])
	    then 
                    let val modTime = OS.FileSys.modTime filename
		        val changed = modTime <> !lastModTimeRef
		    in  lastModTimeRef:= modTime;
			changed
		    end
	    else false
    end
    end

    (* debug info *)
    fun toString {basisPath,
		  projPath,
		  translation,
		  references,
	          fileRefMap:(Entity.FileRef * Syntax.longid list) Entity.Map.map,
		  depsOpt= depsOpt} =
	"\n basisPath: " ^ Pretty.simpleVec ";" (fn (s,ss) => "("^ s ^ "," ^ Pretty.simpleVec "," (fn s => s) ss ^ ")" ) basisPath ^
	"\n references: " ^ Pretty.simpleVec ";" (fn (r,s) => r^s) references ^ 
	"\n projPath: " ^ Pretty.simpleVec ";" (fn (s,ss) => "("^ s ^ "," ^ Pretty.simpleVec "," (fn s => s) ss ^ ")" ) projPath ^
    	"\n translation: " ^ Pretty.simpleVec " -> " (fn (ent,s) => "("^ EntityOps.description ent ^ "," ^ s ^ ")" )
	      (Entity.Map.listItemsi translation) ^
    	"\n fileRefMap: " ^ Pretty.simpleVec " -> " (fn (ent,((file,time),longids)) => "("^ EntityOps.description ent ^ "," ^ file ^ ")")
	      (Entity.Map.listItemsi fileRefMap) ^
	"\n depsOptToString: " ^ (depsOptToString depsOpt)
    and depsOptToString depsOpt =
	case depsOpt of NONE => "NONE" |
	SOME {order=order,deps=deps,classes=classes,packages=packages} =>
	    "\norder: " ^
	    Pretty.simpleVec "," EntityOps.description order
	    ^  "\npackages: " ^
	    Pretty.simpleVec "," Longid.toString (Longid.Set.listItems packages) 
	    ^  "\nclasses: " ^
	    Pretty.simpleVec "," Longid.toString (Longid.Set.listItems classes) 
	    ^  "\ndeps: " ^
	    Pretty.simpleVec "\n" ((fn (ent,set) =>
				    EntityOps.description ent ^
				    Pretty.simpleVec "\n,   " EntityOps.description (Entity.Set.listItems set)))
				  (Entity.Map.listItemsi deps)


    fun serialize(depsOpt) = 
	let val filename = OS.Path.joinDirFile { dir = RuntimeEnv.getCompilerToolDir(), file = RuntimeNames.buildFile }
	    val (pickle,_) = makePersister filename
	    val fileRefMap = case depsOpt of 
	                        NONE =>  Entity.Map.empty
		              | SOME {order,...} => 
                                 (List.foldl(fn (e,m) => case SourceManager.fileRefFor e of 
	                                                    SOME info =>
	                                                    Entity.Map.insert'((e,info),m)
	                                                  | NONE => m)
	                                     Entity.Map.empty  order)
	    val paths = {basisPath= !SourceManager.basisPath,
			 projPath= !SourceManager.projPath,
			 translation = !SourceManager.translation,
			 references= PackageManager.getStampedReferences(),
	                 fileRefMap= fileRefMap,
			 depsOpt= depsOpt}
	in
	    PrintManager.process ("Saving build information in " ^ filename,false) 
	    (fn () =>
	       ((* PrintManager.print (toString paths); *) (*@TODO: remove *)
	        pickle paths)
	       handle Pickle.Pickle s => 
		   PrintManager.print ("\nError in serialization: " ^ s))
	end

    fun deserialize() =
	let val filename = OS.Path.joinDirFile { dir = RuntimeEnv.getCompilerToolDir(), file = RuntimeNames.buildFile }
	in
	    if OS.FileSys.access(filename, [])
		then 
		    PrintManager.process 
		    ("Reading build information from "^filename, false)
		    (fn () =>
		     let
                         val (_,unpickle) = makePersister filename
		     in
			     SOME (unpickle ())
			     handle Pickle.Unpickle s => 
				 (PrintManager.print 
				  ("\nWarning: error reading build information from " ^ filename);
			      NONE)
		     end)
	    else NONE
	end


    end
end



