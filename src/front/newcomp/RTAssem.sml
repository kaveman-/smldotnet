(*======================================================================*)
(* Save a class as an assembler file to be processed by ILASM.		*)
(*======================================================================*)
structure RTAssem :> RTEMIT =
struct

local 
  open RTInstrs RTLabels
  structure I = RTInstrs
  structure RTO = RTAssemOps
in

fun quote file = "\""^file^"\""	

(*----------------------------------------------------------------------*)
(* Code generation controls						*)
(*----------------------------------------------------------------------*)
val [asmComments, showClasses, peverify, showilasm] = 
  map (Controls.add false) 
  ["codegen.asmComments", "codegen.showClasses", "codegen.verify", 
   "codegen.showilasm"]
val tailCalls = Controls.add true "codegen.tailCalls"

(*----------------------------------------------------------------------*)
(* Convert a type into the descriptor format used by ASSEM		*)
(*----------------------------------------------------------------------*)

local 
  val basemap =
  foldl (fn ((tn,s),m) => TyName.Map.insert(m,tn,s)) TyName.Map.empty
  [(TyNames.boolTyName, "bool"),
   (TyNames.int32TyName, "int32"),
   (TyNames.int8TyName, "int8"),
   (TyNames.int16TyName, "int16"),
   (TyNames.int64TyName, "int64"),
   (TyNames.charTyName, "char"),
   (TyNames.word32TyName, "unsigned int32"),
   (TyNames.word16TyName, "unsigned int16"),
   (TyNames.word8TyName, "unsigned int8"),
   (TyNames.word64TyName, "unsigned int64"),
   (TyNames.real64TyName, "float64"),
   (TyNames.real32TyName, "float32"),
   (TyNames.stringTyName, "string"),
   (TyNames.objectTyName, "object"),
   (TyNames.nativeIntTyName, "native int"),
   (TyNames.nativeWordTyName, "native unsigned int")
]
in
  fun classToString (longid,depth) =
      let val split = List.length longid - depth - 1
	  val dotted = List.take(longid,split)
	  val nested = List.drop(longid,split)
      in
	  (if List.null dotted then "" 
	   else Pretty.simpleVec "." RTO.idToString dotted ^ ".") ^
          Pretty.simpleVec "/" RTO.idToString nested
      end
  fun classTyToString (VMTy.Class c) =
    let
      val SOME (assembly, longid, depth) = TyName.fromExternal c     
      val s = if Id.equal(assembly, Id.fromString "") then "" else "[" ^ RTO.idToString assembly ^ "]" 
      (*@HACK translate syntax for nested classes, assumes "\\+" doesn't occur in string *)
    in
      s ^ classToString(longid,depth)
    end
    | classTyToString (VMTy.ValueClass c) =
    let
      val SOME (assembly, longid, depth) = TyName.fromExternal c     
      val s = if Id.equal(assembly, Id.fromString "") then "" else "[" ^ RTO.idToString assembly ^ "]" 
      (*@HACK translate syntax for nested classes, assumes "\\+" doesn't occur in string *)
    in
      s ^ classToString(longid,depth)
    end
  | classTyToString ty = tyToString ty

and tyToString ty =
case ty of
  VMTy.ValueClass c =>
  (case TyName.Map.find(basemap, c) of
    SOME s => s
  | NONE => 
    case ty of
      VMTy.ValueClass c => "valuetype " ^ classTyToString ty
    | VMTy.Class c => "class " ^ classTyToString ty)
| VMTy.Class c =>
  (case TyName.Map.find(basemap, c) of
    SOME s => s
  | NONE => 
    case ty of
      VMTy.ValueClass c => "valuetype " ^ classTyToString ty
    | VMTy.Class c => "class " ^ classTyToString ty)

| VMTy.Array ty =>
  tyToString ty ^ "[]"

| VMTy.App(ty,tys) =>
  tyToString ty ^ "<" ^ Pretty.simpleVec "," tyToString tys ^ ">"

| VMTy.Var i =>
  "!" ^ Int.toString i

| VMTy.Address ty =>
  tyToString ty ^ " &"

end

fun tyToTypeRef ty =
case ty of
  VMTy.Class c =>
  classTyToString ty
| VMTy.ValueClass c =>
  classTyToString ty

| VMTy.Array ty =>
  (*BUG: tyToTypeRef ? *)
  tyToString ty ^ "[]"

val repToString = tyToString
val repToTypeRef = tyToTypeRef

fun resTyToString NONE = "void"
  | resTyToString (SOME rep) = repToString rep

fun argTysToString argreps = "(" ^ Pretty.simpleVec "," repToString argreps ^ ")"

local
  val basemap =
  foldl (fn ((tn,s),m) => TyName.Map.insert(m,tn,s)) TyName.Map.empty
  [(TyNames.boolTyName, "i1"),
   (TyNames.int32TyName, "i4"),
   (TyNames.int8TyName, "i1"),
   (TyNames.int16TyName, "i2"),
   (TyNames.int64TyName, "i8"),
   (TyNames.charTyName, "u2"), (*@TODO: review *)
   (TyNames.word32TyName, "u4"),
   (TyNames.word16TyName, "u2"),
   (TyNames.word8TyName, "u1"),
   (TyNames.word64TyName, "u8"),
   (TyNames.real64TyName, "r8"),
   (TyNames.real32TyName, "r4")]
in
fun repToSignedSuffix ty = 
case ty of
  VMTy.ValueClass b =>
  valOf(TyName.Map.find(basemap, b))

| _ => "ref"
end

local
  val basemap =
  foldl (fn ((tn,s),m) => TyName.Map.insert(m,tn,s)) TyName.Map.empty
  [(TyNames.boolTyName, "i1"),
   (TyNames.int32TyName, "i4"),
   (TyNames.int8TyName, "i1"),
   (TyNames.int16TyName, "i2"),
   (TyNames.int64TyName, "i8"),
   (TyNames.charTyName, "i2"), (*@TODO: review *)
   (TyNames.word32TyName, "i4"),
   (TyNames.word8TyName, "i1"),
   (TyNames.word16TyName, "i2"),
   (TyNames.word64TyName, "i8"),
   (TyNames.real64TyName, "r8"),
   (TyNames.real32TyName, "r4")]
in
fun repToSuffix ty = 
case ty of
  VMTy.ValueClass b =>
  valOf(TyName.Map.find(basemap, b))

| _ => "ref"
end

(*----------------------------------------------------------------------*)
(* Convert a method signature into format understood by ASSEM		*)
(*----------------------------------------------------------------------*)
fun methodToString { name, classty, argtys, resty } =
  (if Id.equal(name,RuntimeNames.instanceConstructor) then "instance " else "") ^
  resTyToString resty ^ " " ^ 
  classTyToString classty ^ "::" ^
  RTO.idToString name ^ 
  argTysToString argtys  

(*----------------------------------------------------------------------*)
(* Convert a constructor signature into format understood by ASSEM	*)
(*----------------------------------------------------------------------*)
fun constructorToString { classty, argtys } =
  "instance void " ^ 
  classTyToString classty ^ "::" ^
  ".ctor" ^ 
  argTysToString argtys  

(*----------------------------------------------------------------------*)
(* Convert a field signature into format understood by ASSEM		*)
(*----------------------------------------------------------------------*)
fun fieldToString { name, classty, fldty } =
  repToString fldty ^ " " ^ 
  classTyToString classty ^ "::" ^
  RTO.idToString name

val smallints = [(* "-1", *) "0", "1", "2", "3", "4", "5", "6", "7", "8"]

val ilasmOptions = ref ([]:(string * string option) list)

fun mkOptions() = Pretty.simpleVec " " 
                (fn (option,SOME value) => "/"^option ^ "=" ^value
		  | (option,NONE) => "/"^option) (!ilasmOptions)

val _ = Commands.add "ilasm"
{
  act = fn root => (fn [] => (ilasmOptions := [];OS.Process.success)
                     | options => (ilasmOptions := (!ilasmOptions)@options;OS.Process.success)
		    ),
  query = fn () => "ilasm options: " ^ mkOptions(),
  syntax = "<option>[=<value>],...,<option>[=<value>]",
  help = "ilasm <option>[=<value>], ... ,<option>[=<value>]\n\
         \  pass on options to ilasm, eg.\n    ilasm KEY=keyFile.snk,CLOCK\n\
         \ilasm \n  Clear ilasm option\n\
         \ilasm? \n  Query ilasm options"
}

(*----------------------------------------------------------------------*)
(* Convert an instruction into the format understood by ASSEM		*)
(*----------------------------------------------------------------------*)
fun toString instr =
case instr of
  add _ => "add"
| add_ovf _ => "add.ovf"
| add_ovf_un _ => "add.ovf.un"
| And _ => "and"
| arglist => "arglist"
| beginscope bindings => 
  "{ .locals " ^ "(" ^ Pretty.simpleVec "," (fn (n,ty,sym) => "[" ^ Int.toString n ^ "] " ^ repToString ty ^ " " ^ RTO.idToString  sym)  bindings ^ ")"
| endscope => "}"
| bcmp (ne,_,i) => "bne.un " ^ labelToString i
| bcmp (t,_,i) => "b" ^ RTO.testToString t ^ " " ^ labelToString i
| bcmp_un (t,_,i) => "b" ^ RTO.testToString t ^ ".un " ^ labelToString i
| br i => "br " ^ labelToString i
| brfalse (_,i) => "brfalse " ^ labelToString i
| brtrue (_,i) => "brtrue " ^ labelToString i
| box (c as VMTy.ValueClass _) => "box " ^ classTyToString c
| call method => "call " ^ methodToString method
| callvirt method => 
  "callvirt instance " ^ methodToString method
| callinst method =>
  "call instance " ^ methodToString method
| conv (_, c as VMTy.Class _) => "castclass " ^ classTyToString c
| conv (_, c as VMTy.Array _) => "castclass " ^ tyToString c
| conv (_, ty) => "conv." ^ repToSignedSuffix ty
| conv_ovf (_, ty) => "conv.ovf." ^ repToSignedSuffix ty
| conv_ovf_un (_, ty) => "conv.ovf." ^ repToSignedSuffix ty ^ ".un"
| ckfinite => "ckfinite"
| cmp (t,_) => "c" ^ RTO.testToString t
| cmp_un (t,_) => "c" ^ RTO.testToString t ^ ".un"
| comment s => "// " ^ s
| cpblk => "cpblk"
| Div _ => "div"
| div_un _ => "div.un"
| dup _ => "dup"
| endcatch => "endcatch"
| endfilter => "endfilter"
| endfinally => "endfinally"
| entercrit => "entercrit"
| exn { startlabel, endlabel, handlerstart, handlerend, exnclass } =>
  ".try " ^ labelToString startlabel ^
  " to " ^ labelToString endlabel ^
  " catch " ^ classTyToString exnclass ^
  " handler " ^ labelToString handlerstart ^ 
  " to " ^ labelToString handlerend
| exitcrit => "exitcrit"
| initblk => "initblk"
| initobj r => "initobj " ^ classTyToString r
| isinst r => "isinst " ^ classTyToString r
| jmp m => "jmp " ^ methodToString m
| jmpi m => "jmpi " ^ methodToString m
| label i => labelToString i ^ ":"
| ldarg i => 
  "ldarg" ^ (if i < 4 then "." else if i < 256 then ".s " else " ") ^ Int.toString i
| ldarga i => "ldarga " ^ Int.toString i
(*@TODO: the treatment of constants seem to ignore signs --- this is probably wrong *)
| ldc (Constants.BOOLEAN i) => "ldc.i4 " ^ RTO.intToString i
| ldc (Constants.BYTE i) => "ldc.i4 " ^ RTO.intToString i
| ldc (Constants.SHORT i) => "ldc.i4 " ^ RTO.intToString i
| ldc (Constants.INT i) => 
  let
    val s = RTO.intToString i
  in
    "ldc.i4" ^ 
    (if RTInt.isji1 i 
     then if List.exists (fn s' => s=s') smallints then "." else ".s " 
     else " ") ^ s
  end
| ldc (Constants.LONG i) => "ldc.i8 " ^ RTO.longToString i
| ldc (Constants.CHAR i) => "ldc.i4 " ^ RTO.intToString i
| ldc (Constants.FLOAT f) => "ldc.r4 " ^ RTO.floatToString f
| ldc (Constants.DOUBLE d) => "ldc.r8 " ^ RTO.doubleToString d
| ldc (Constants.STRING s) => "ldstr " ^ RTO.UStringToQString s
| ldc c => Debug.fail "RTAssem.toString: invalid constant"
| ldelem r => "ldelem." ^ repToSignedSuffix r
| ldelema r => "ldelema " ^ classTyToString r
| ldfld field => "ldfld " ^ fieldToString field
| ldflda field => "ldflda " ^ fieldToString field
| ldftn m => "ldftn instance " ^ methodToString m
| ldvirtftn m => "ldvirtftn instance " ^ methodToString m
| ldind r => "ldind." ^ repToSignedSuffix r 
| ldlen => "ldlen"
| ldloc (i,ty) => 
  "ldloc" ^ (if i < 4 then "." else if i < 256 then ".s " else " ") ^ Int.toString i
| ldloca i => "ldloca " ^ Int.toString i
| ldnull => "ldnull"
| ldobj (c as VMTy.ValueClass _) => "ldobj " ^ classTyToString c
| ldsfld field => "ldsfld " ^ fieldToString field
| ldsflda field => "ldsflda " ^ fieldToString field
| ldvirtfn => "ldvirtfn"
| line {line=l,col,file} => 
  ".line " ^ (Int.toString l) ^ ":"
           ^ (Int.toString col) ^ " "
           ^ (RTO.stringToString file)
| linespan {line=(ll,rl),col=(lc,rc),file} => 
  if ll = rl 
  then
  ".line " ^ (Int.toString ll) ^ "," ^ (Int.toString rl) ^ ":"
           ^ (Int.toString lc) ^ "," ^ (Int.toString rc) ^ " "
           ^ (RTO.stringToString file)
  else
  (*@HACK: we truncate multi-line linespans to lines for better breakpoint setting in the editor *)
  ".line " ^ (Int.toString ll) ^ ":"
           ^ (Int.toString lc) ^ " "
           ^ (RTO.stringToString file)
| leave l => "leave " ^ labelToString l
| localloc => "localloc"
| mul _ => "mul"
| mul_ovf _ => "mul.ovf"
| mul_ovf_un _ => "mul.ovf.un"
| neg _ => "neg"
| newarr r => "newarr " ^ repToTypeRef r
| newobj constructor => 
  "newobj " ^ constructorToString constructor
| staticnewobj constructor =>   
  (* if staticnewobj has not been rewritten to a call, emit newobj *)
  "newobj " ^ constructorToString constructor
| nop => "nop"
| Not => "not"
| or _ => "or"
| pop => "pop"
| rem _ => "rem"
| rem_un _ => "rem.un"
| ret => "ret"
| rethrow => "rethrow"
| shl _ => "shl"
| shr _ => "shr"
| shr_un _ => "shr.un"
| starg i => "starg" ^ (if i < 256 then ".s " else " ") ^ Int.toString i
| stelem r => "stelem." ^ repToSuffix r
| stind r => "stind." ^ repToSuffix r
| stloc i => 
  "stloc" ^ (if i < 4 then "." else if i < 256 then ".s " else " ") ^ Int.toString i
| stfld field => "stfld " ^ fieldToString field
| stsfld field => "stsfld " ^ fieldToString field
| sub _ => "sub"
| sub_ovf _ => "sub.ovf"
| sub_ovf_un _ => "sub.ovf.un"
| switch labels => "switch (" ^ Pretty.simpleVec "," labelToString labels ^ ")"
| tailcall => if Controls.get tailCalls then "tail." else "//tail."
| throw => "throw"
| unaligned i => "unaligned " ^ Int.toString i
| unbox (c as VMTy.ValueClass _) => "unbox " ^ classTyToString c
| volatile => "volatile"
| xor _ => "xor"




fun emit (projname, emitter) =
let

  val ilasmCmd = RuntimeEnv.getIlasmFileName()
  val translateLines = RuntimeEnv.getVersion() < "2" andalso not (RuntimeEnv.getCompilerIlasm())
  val emitLineSpans = RuntimeEnv.getVersion() >= "2" orelse RuntimeEnv.getCompilerIlasm()


  val {translateInstr,startDbg,finishDbg} = mkDebugOps(projname,translateLines,emitLineSpans)
  val SOME { asm, out, assembly, module, kind } =
    TargetManager.getInfo ()

  (* don't emit entrypoint for non-exe *)
  val entrypointEmitted = ref (kind <> TargetManager.Exe)

  val asmfile = TextIO.openOut asm
  (*@todo: only emit those actually referenced *)
  val assemblyFiles = PackageManager.getReferences ()
  fun emit s = TextIO.output(asmfile, s) 
  fun emitLine s = emit (s ^ "\n")
  fun outputDeclExtern assemblyFile =
	let  fun outputRefDeclOpt(desc,opt) =
               (case opt of 
  	          SOME s => emitLine (desc ^ s)
                | _ => ())
	     val SOME {name,publickeytoken,version} = PackageManager.getPackageInfo assemblyFile
        in
 	    emitLine (".assembly extern " ^ name ^ " {");
	    outputRefDeclOpt("  .publickeytoken = ",publickeytoken);  
	    outputRefDeclOpt("  .ver ",version);
            emitLine "}"
	end	
  val options = case kind of TargetManager.Lib => "/dll " | _ => ""
  val options = if Controls.get RTInstrs.debug then " /DEBUG " ^ options else options
  val options = "/QUIET " ^ options
  val options = options ^ " " ^ mkOptions()
  val logname = asm ^ ".log"
  val ilasmCmdLine = ilasmCmd ^ " " ^ options ^ " " ^ quote asm ^ " /out=" ^ quote out
      (*@HACK: restore ^ " >" ^ quote logname *)

  fun runVerifier () =    
    if not(Controls.get CompileOps.verifiable)
    then (PrintManager.println "Warning: code may not be verifiable"; true)

    else
    case (Controls.get peverify, RuntimeEnv.getPeverifyFileName()) of
      (true, SOME name) =>
      (if OS.Process.system (name ^ " " ^ quote out (*@HACK: restore ^ " >" ^ quote logname *) 
	    )
	     = OS.Process.success
	then true
	else (PrintManager.println ("Verification failed" (*@HACK: restore  "find details in " ^ quote logname*)); 
              OS.FileSys.remove out; false))

    | _ => true

  fun save ({name = (longid,depth),attributes,flags, fields, methods, super, interfaces} : RTResult.Class) =
  let
  val isDelegate = case super of VMTy.Class tn => TyName.eq(tn,TyNames.delegateTyName) orelse TyName.eq(tn,TyNames.multicastDelegateTyName) | _ => false

  (* detect and remove the  augmentation psuedo flag *)  
  val isAugmentation = Symbol.Set.member(flags,CompileOps.augmentation)
  val flags = if isAugmentation then Symbol.Set.delete(flags,CompileOps.augmentation) else flags

  val asmComments = Controls.get asmComments
  fun flagToString flag =
(*    if Symbol.equal(flag, Id.finalSym)
    then "sealed" else *) Id.toString flag

  local
      fun byteToString byte =  
	  let fun i2hex i = 
	      if i < 10 
		  then Char.chr ((Char.ord #"0") + i) 
	      else Char.chr ((Char.ord #"A") + (i-10))
	      val i = Word8.toInt byte
	      val h1 = i div 16
	      val h2 = i mod 16
	  in
		implode [i2hex h1,i2hex h2,#" "]
	  end	
  in
  fun saveAttribute (argtys,resty,bytes) =
      (emitLine (".custom instance void " ^ classTyToString resty ^
	  "::.ctor(" ^ Pretty.simpleVec "," tyToString argtys ^ ")"); 
      (*@TODO:  possible bug: 
       classTyToString only works on *external* classes --- 
       may not work on ML declared attributes ... *)
      case Word8Vector.length(bytes) of 
	0 => emitLine "\n" 
      | _ => (emit "       = (";
	      Word8Vector.foldl 
	        (fn (byte,count) => 
		 (emit (byteToString byte);
		  if count = 15
		      then (emit "\n          ";
			    0)
		  else count+1))
		0 
		bytes;
	      emitLine ")"))
  end

  fun saveMethod { name, override, attributes, args, resty, flags, code:RTInstrs.Instrs, stack, locals } =
  let

    (* detect and remove the pseudo .entrypoint flag *)

    (*@HACK: we rely on the fact that globals are emitted *after* classes to emit a default entry point *)
    val markedEntry = Symbol.Set.member(flags,CompileOps.entrypoint)
    val flags = if markedEntry then Symbol.Set.delete(flags,CompileOps.entrypoint) else flags
    val isEntry = markedEntry andalso not (!entrypointEmitted)
    
    (* detect and remove the pseudo noinlining flag *)
    val noinlining = Symbol.Set.member(flags,CompileOps.noinliningSym)
    val flags = if noinlining then Symbol.Set.delete(flags,CompileOps.noinliningSym) else flags

    fun flagToString flag =
    if Symbol.equal(flag, Id.protectedSym)
    then "family"
    else Id.toString flag

    val implAttr = if isDelegate 
                   then "runtime" 
                   else if noinlining then  "managed noinlining" 
                   else "cil"

    val restystr =
      case resty of 
        NONE => "void"
      | SOME rep => tyToString rep

    val head = ".method " ^ Pretty.simpleVec " " flagToString (Symbol.Set.listItems flags)
    val head = 
      if Symbol.Set.member(flags, Id.staticSym)
      then head
      else
      if Id.equal(name, RuntimeNames.instanceConstructor)
      then head ^ " instance"      
      else head ^ " virtual instance"

    fun indent (RTInstrs.comment _) = false
      | indent (RTInstrs.label _) = false
      | indent (RTInstrs.exn _) = false
      | indent _ = true

    fun dumpInstr (RTInstrs.comment s) =
        if asmComments then emitLine("// " ^ s) else ()
      | dumpInstr instr =        
        emitLine ((if indent instr then "  " else "") ^ toString (translateInstr instr))

    local 
      fun varToString (x,[]) = Var.toString x
        | varToString (x,si) = RTO.stringToString (Longid.toString si)
	  fun saveLocal(n,(varlist,ty)) =
             emit ("["^Int.toString n^"] "^ repToString ty ^
                   " /*" ^ Pretty.simpleVec " " varToString varlist ^ "*/ ")

	      
	  fun saveLocals (_,[]) = ()
	     | saveLocals (n,[loc]) = saveLocal (n,loc)
	     | saveLocals (n,(loc::locs)) = (saveLocal(n,loc);
                                         emit ",\n         "; 
                                         saveLocals (n+1,locs))
    in 
	val saveLocals = fn locals => (emit "{.locals (";saveLocals (0,locals);emitLine ")}")
    end
  in
    emit (head ^ " " ^ restystr ^ " " ^ RTO.idToString name ^ "(" ^ Pretty.simpleVec "," (fn (id,ty) => tyToString ty ^ " " ^ RTO.idToString id) args ^ ") ");
    emit implAttr;
    emitLine " {";
    app saveAttribute attributes;
    (case override of NONE => () | SOME methodid => emitLine (".override " ^ classTyToString super ^ "::" ^ RTO.idToString methodid));
    if isDelegate orelse Symbol.Set.member(flags, Id.abstractSym)  then () 
    else(emitLine (".maxstack " ^ Int.toString stack);
    emitLine (".zeroinit"); 
    (* saveLocals locals;  *)
    if isEntry then (emitLine ".entrypoint";entrypointEmitted := true) else ();
	 RTInstrs.appInstrs dumpInstr (addNullLine code));
    (* we declare locals at the end,
       so they are dead on entry for the debugger *)
    saveLocals locals; 
    emitLine "}"
  end

  fun saveField { name, ty, flags, value } =
  let
    val head = 
      ".field " ^ (Pretty.simpleVec " " flagToString (Symbol.Set.listItems flags))
    val value = case value of NONE => "" | SOME i => " = int32(" ^Int.toString i^")"
  in
    emitLine (head ^ " " ^ repToString ty ^ " " ^ RTO.idToString name ^ value )
  end

  local
  val split = List.length longid - depth - 1
  val dotted = List.take(longid,split)
  val nested = List.drop(longid,split)
  fun namespacehead dotted = if List.null dotted then "" 
			     else "\n.namespace " ^ Pretty.simpleVec "." RTO.idToString dotted ^ " {"
  fun namespacetail dotted = if List.null dotted then "" 
			     else "\n}"
  fun nestedhead (_,[]) = ""
    | nestedhead (top,c::cs) = "\n.class " ^ 
                                (if top then " " else "nested ") ^
				(if null cs then 
				    Pretty.simpleVec " " flagToString (Symbol.Set.listItems flags) ^
				    " " ^
				    RTO.idToString c ^ 
				    (if Symbol.Set.member(flags,Id.interfaceSym) orelse isAugmentation then "" 
				     else  " extends " ^ classTyToString super) ^ 
				    (if null interfaces then "" 
				     else " implements " ^ Pretty.simpleVec "," classTyToString interfaces) ^
				    "{\n"
				 else RTO.idToString c ^ "{") ^
				nestedhead (false,cs)
  fun nestedtail [] = "" 
    | nestedtail (c::cs) = "\n}" ^ nestedtail cs
  in
      val classhead = namespacehead dotted ^ nestedhead (true,nested) 
      val classtail = namespacetail dotted ^ nestedtail nested
  end


  fun isStaticMethod ({ flags, ... }:RTResult.Method) = Symbol.Set.member(flags, Id.staticSym)
  fun isStaticField ({ flags, ... }:RTResult.Field) =  Symbol.Set.member(flags, Id.staticSym)

in
  if Controls.get showClasses
  then PrintManager.print ("[" ^ classToString (longid,depth) ^ "] ")
  else ();
  Stats.add ("static fields/class", length (List.filter isStaticField fields));
  Stats.add ("instance fields/class", length (List.filter (not o isStaticField) fields));
  Stats.add ("static methods/class", length (List.filter isStaticMethod methods));
  Stats.add ("instance methods/class", length (List.filter (not o isStaticMethod) methods));
  emitLine classhead;
  app saveAttribute attributes;
  app saveField fields;
  app saveMethod methods;
  emitLine classtail 
end



in
  (* Header stuff *)
  startDbg projname;

  (* we do this early so that ilasm /DEBUG picks up mscorlib correctly *)
  List.app outputDeclExtern assemblyFiles;  

  emitLine (".assembly '" ^ assembly ^"'"); 
  (*@TODO: any more required fields here? *)
  emitLine ("{ .hash algorithm 0x00008004 }"); 
  emitLine (".module '" ^ module ^"'"); 

  (* Actually emit the classes *)
  emitter save handle e => (finishDbg(); TextIO.closeOut asmfile; raise e);
  
  (* Footer stuff *)
  finishDbg();
  TextIO.closeOut asmfile;
  if (OS.Process.system 
       (if Controls.get showilasm then PrintManager.println (ilasmCmdLine)
        else ();
	ilasmCmdLine
	)
       = OS.Process.success)
  then runVerifier ()
  else (PrintManager.println ("Assembler failed, invoked with: " ^ ilasmCmdLine (*@HACK: restore : "find details in " ^ logname *)); 
	false)
end


end (* of local open *)

end (* of struct *)




