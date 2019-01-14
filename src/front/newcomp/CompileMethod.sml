(*======================================================================*)
(* Compile a single method from a type abstraction			*)
(*======================================================================*)
structure CompileMethod =
struct

local 
  open MILTerm RepNames CompileEnv TyRep
  structure I = RTInstrs

in

val dump = Controls.add false "codegen.dump"
val peephole = Controls.add true "codegen.peephole"

fun compile env (name, override, attributes, flags, (xs:MILTerm.TypedVar list,e)) =
    let

      val _ = if Controls.get dump then (Debug.print ("\nMETHOD " ^ Id.toString name ^ "(" ^ Pretty.simpleVec "," (Var.toString o #1 o #1) xs ^ "):\n"); MILPretty.dumpCmp e) else ()

      val _ = Liveness.analyse e
      fun compileAttribute (argtys,resty,bytes) = (*@TODO: factor this def and one in CompileClass to separate file *)
      (map (externalTyToRep env) argtys,
       externalTyToRep env resty,
       bytes);

      val attributes = map compileAttribute attributes

      val (argvars, argtys) = ListPair.unzip xs
      val argreps = map (tyToRep env) argtys
     
      val env = envPlusArgVars env xs
      val _ = RTLocals.init (ListPair.zip (argvars, argreps))
      val (bodyty, bodyinstrs, stack) = CompileCont.compile env e
      val bodyinstrs = if Controls.get peephole then RTPeephole.peephole bodyinstrs else bodyinstrs
      val {locals,args} = RTLocals.query ()
      fun addCast (instr as (I.ldloc (i,ty))) = if RTOps.isRefType ty andalso #3 (List.nth(locals,i)) then I.fromList [I.ldloc (i,ty), I.conv (RTOps.object, ty)] else I.Single instr
        | addCast instr = I.Single instr

      val bodyinstrs = if Controls.get CompileOps.verifiable andalso List.exists (fn (_,ty,shared) => shared andalso RTOps.isRefType ty) locals
                       then I.mapInstrs addCast bodyinstrs else bodyinstrs

      val bodyinstrs = StaticNewObj.compile(length locals + length args, bodyinstrs)

      val (eff,restys) = MILTy.fromCmp bodyty
      val resreps = map (tyToRep env) restys
      val resrep = OptionOps.hd resreps
      val isstatic = Symbol.Set.member(flags, Id.staticSym)      
      fun getArgName (x,[]) = Id.fromString (Var.toString x)
        | getArgName (x,longid) = List.last longid
    in
      Stats.add ("instrs/method", I.countInstrs bodyinstrs);
      Stats.add ("locals/method", length locals);
      Stats.add ("stack/method", stack);
      Stats.add ("shared-ref locals/method", length (List.filter (fn (_,ty,shared) => shared andalso RTOps.isRefType ty) locals));
      Stats.add ("shared-int locals/method", length (List.filter (fn (_,ty,shared) => shared andalso not (RTOps.isRefType ty)) locals));
      {
        name = name,
        override = override,
        attributes = attributes,
        stack = stack,
        locals = map (fn (x,y,shared) => (x,y)) locals,
        resty = resrep,
        args = if isstatic then ListPair.zip(map getArgName argvars, argreps) else tl (ListPair.zip(map getArgName argvars, argreps)),
        flags = flags,
        code = bodyinstrs
      } : RTResult.Method
    end
    handle ex => (let fun prEnv desc env pr = Var.Map.appi (fn (v,info) => 
						  (Debug.print desc;
						   Debug.print (Var.toString v);
						   Debug.print ":";
						   Debug.print (pr info)))
			                         env
		  in
		      PrintManager.print ("\n BUG: Error occurred compiling method " ^ Id.toString name);
		      Debug.print "\n";
		      Debug.printDoc (MILPretty.pTAbstr((xs,e)));
		      prEnv "\nknown " (#known env) (MILTy.toString) ;
		      prEnv "\nblock " (#blocks env) (MILTy.toString o #1) ;
		      prEnv "\ntyenv "(#tyenv env) (MILTy.toString o #2);
 		      raise ex
(*		      {
		       name = name,
		       attributes = [],
		       stack = 0,
		       locals = [],
		       resty =  NONE,
		       args = [],
		       flags = Symbol.Set.empty,
		       code = [I.comment "BUG: this is a bogus method --- see log for details"]
		       } : RTResult.Method *)

		  end)
end

end