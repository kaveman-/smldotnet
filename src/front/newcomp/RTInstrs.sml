(*======================================================================*)
(* Datatypes and helper functions for the runtime instruction set.	*)
(*======================================================================*)
structure RTInstrs =
struct

local open RTLabels in

(*----------------------------------------------------------------------*)
(* Method signatures include the class, result and argument types	*)
(*----------------------------------------------------------------------*)
type Method = 
{ 
  name : Id.id,
  classty : VMTy.Type, 
  argtys : VMTy.Type list,
  resty : VMTy.Type option 
}

(*----------------------------------------------------------------------*)
(* Field signatures include the class and field type			*)
(*----------------------------------------------------------------------*)
type Field = 
{ 
  name : Id.id,
  fldty : VMTy.Type, 
  classty : VMTy.Type 
}

(*----------------------------------------------------------------------*)
(* Constructor signatures include the class and argument types		*)
(*----------------------------------------------------------------------*)
type Constructor = 
{ 
  classty : VMTy.Type, 
  argtys : VMTy.Type list 
}

(*----------------------------------------------------------------------*)
(* Exception handler specifications					*)
(*----------------------------------------------------------------------*)
type Exception =
{ 
  startlabel : Label, 
  endlabel : Label, 
  handlerstart : Label,
  handlerend : Label,
  exnclass : VMTy.Type 
}

datatype test=eq|ne|lt|ge|le|gt

(*------------------------------------------------------------------------*)
(* CLR instructions						  	  *)
(*------------------------------------------------------------------------*)
datatype Instr =				
  add		of VMTy.Type			
| add_ovf	of VMTy.Type			
| add_ovf_un	of VMTy.Type			
| And		of VMTy.Type			
| arglist					
| beginscope    of (int * VMTy.Type * Id.id ) list
| endscope     
| bcmp 		of test*VMTy.Type*Label		(* b??       *)
| bcmp_un 	of test*VMTy.Type*Label		(* b??.un    *)
| br 		of Label			
| brfalse 	of VMTy.Type*Label		
| brtrue 	of VMTy.Type*Label		
| box         	of VMTy.Type          		
| call 		of Method			
| callinst      of Method                       (* call instance *)
| callvirt 	of Method			
| ckfinite					
| cmp 		of test * VMTy.Type		(* c??	  *)
| cmp_un 	of test * VMTy.Type		(* c??.un	  *)
| comment 	of string			
| conv 		of VMTy.Type * VMTy.Type	(* conv/castclass *)
| conv_ovf 	of VMTy.Type * VMTy.Type	
| conv_ovf_un 	of VMTy.Type * VMTy.Type	
| cpblk
| Div		of VMTy.Type
| div_un	of VMTy.Type			
| dup		of VMTy.Type			
| endcatch
| endfilter
| endfinally
| entercrit
| exitcrit
| exn 		of Exception
| initblk			
| initobj 	of VMTy.Type
| isinst 	of VMTy.Type
| jmp 		of Method
| jmpi 		of Method
| label 	of Label
| ldarg 	of int
| ldarga 	of int			
| ldc 		of Constants.constant
| ldelem 	of VMTy.Type
| ldelema 	of VMTy.Type	
| ldfld 	of Field	
| ldflda 	of Field	
| ldftn 	of Method
| ldvirtftn     of Method
| ldind 	of VMTy.Type			
| ldlen				
| ldloc 	of int * VMTy.Type		(* Type required for shared-locals that need castclass *)          
| ldloca 	of int		
| ldnull			
| ldobj  	of VMTy.Type	
| ldsfld 	of Field	
| ldsflda 	of Field	
| ldvirtfn			
| leave 	of Label	
| linespan      of {line:(int * int),col:(int*int),file:string}
| line          of {line:int,col:int,file:string}
| localloc			
| mul		of VMTy.Type
| mul_ovf	of VMTy.Type
| mul_ovf_un	of VMTy.Type
| neg		of VMTy.Type
| newarr 	of VMTy.Type	
| newobj	of Constructor
| staticnewobj	of Constructor 
| nop
| Not						
| or		of VMTy.Type			
| pop						
| rem		of VMTy.Type			
| rem_un	of VMTy.Type			
| ret	 	                       		
| rethrow
| shl		of VMTy.Type			
| shr		of VMTy.Type			
| shr_un	of VMTy.Type			
| starg 	of int               		
| stelem 	of VMTy.Type			
| stind 	of VMTy.Type			
| stloc 	of int				
| stfld 	of Field			
| stsfld 	of Field			
| sub		of VMTy.Type			
| sub_ovf	of VMTy.Type			
| sub_ovf_un	of VMTy.Type			
| switch 	of Label list			
| tailcall					
| throw						
| unaligned 	of int				
| unbox 	of VMTy.Type			
| volatile					
| xor		of VMTy.Type			

datatype Instrs = Empty | Single of Instr | Join of Instrs * Instrs 

fun ++ (x, y) = Join(x,y)
fun $ x = Single x
fun $+ (x, y) = Join(Single x, y)
fun +$ (x, y) = Join(x, Single y)
val fromList = foldr (fn (x,y) => Join(Single x, y)) Empty
val flat = foldr Join Empty

fun countInstrs Empty = 0
  | countInstrs (Single _) = 1
  | countInstrs (Join(x,y)) = countInstrs x + countInstrs y

fun appInstrs f Empty = ()
  | appInstrs f (Single x) = f x
  | appInstrs f (Join(x,y)) = (appInstrs f x; appInstrs f y)

fun mapInstrs f Empty = Empty
  | mapInstrs f (Single x) = f x
  | mapInstrs f (Join(x,y)) = Join(mapInstrs f x, mapInstrs f y)

fun toList Empty = [] 
  | toList (Single x) = [x]
  | toList (Join(x,y)) = toList x @ toList y

fun getItem Empty = NONE
  | getItem (Single x) = SOME (x, Empty)
  | getItem (Join(x,y)) = 
    case getItem x of
      NONE => getItem y
    | SOME (h,t) => SOME (h, Join(t,y))
end

fun units rep = 1

fun classRepToString rep = 
case rep of
  VMTy.Class c =>
  TyName.toString c

val debug = Controls.add false "debug"

val nullLine = line{line=1,col=0,file= ""}

(*----------------------------------------------------------------------*)
(* If [translateLines] is true, translate line(span) instructions       *)
(* to  simple line instructions, producing                              *)
(* a single dummy source file that contains all the mentioned sources.  *)
(* If [emitLineSpans] is false, translate linespans to lines.           *)
(*----------------------------------------------------------------------*)

fun mkDebugOps (projname,translateLines,emitLineSpans) =
if translateLines andalso Controls.get debug 
then
(*@HACK: lazily concatenate referenced source files into a single source file, translating line info *)
let 
    val dbgLength = ref 0
    val dbgMap = ref (StringMap.empty:int StringMap.map)
    val SOME{asm,...} = TargetManager.getInfo()
    val {base,ext} = OS.Path.splitBaseExt asm
    val dbgname = OS.Path.joinBaseExt({base=base,ext=SOME "ml"})
    val dbgFile = TextIO.openOut dbgname

    fun startDbg projname = 
	(TextIO.output(dbgFile,"(***** This file contains debug information, it is NOT a source file *****)\n");
	 dbgLength := 1)

    fun finishDbg () = TextIO.closeOut dbgFile

    fun translateInstr (line{line=1,col=0,file= ""}) = 
	    line {line=1,col = 0, file= dbgname}
      | translateInstr(ln as linespan{line=(l,_),col=(col,_),file})
            =   (case StringMap.find(!dbgMap,file) of 
		      SOME offset => 
			  line {line=l + offset,col= col,file= dbgname}
		    | NONE => 
			  let val offset = !dbgLength
			      val is = TextIO.openIn file
			      fun loop len =
				  case TextIO.inputLine is of
                                    NONE => len
                                  | SOME s =>(TextIO.output(dbgFile,s); loop (len+1))
			      val len = loop 0
			  in
			      TextIO.closeIn(is);
			      dbgMap := StringMap.insert(!dbgMap,file,offset);
			      dbgLength := (!dbgLength)+len;
			      line{line=(l + offset),col=col,file=dbgname}
			  end)
      | translateInstr(ln as line{line=l,col=col,file})
	    =   (case StringMap.find(!dbgMap,file) of 
		      SOME offset => 
			  line {line=l + offset,col= col,file= dbgname}
		    | NONE => 
			  let val offset = !dbgLength
			      val is = TextIO.openIn file
			      fun loop len =
				  case TextIO.inputLine is of
 				    NONE => len
 				  | SOME s =>(TextIO.output(dbgFile,s);
					      loop (len+1))
			      val len = loop 0
			  in
			      TextIO.closeIn(is);
			      dbgMap := StringMap.insert(!dbgMap,file,offset);
			      dbgLength := (!dbgLength)+len;
			      line{line=(l + offset),col=col,file=dbgname}
			  end)
      | translateInstr instr = instr
in
    {startDbg = startDbg,
     translateInstr = translateInstr,
     finishDbg = finishDbg}
end
else
    let val nullFile = OS.Path.joinDirFile { dir = RuntimeEnv.getCompilerBinDir(), file = RuntimeNames.nullFile }
        fun translateInstr (line{line=1,col=0,file= ""}) = 
	    line {line=1,col=0,file=nullFile}
	  | translateInstr (instr as linespan{line=(l,_),col=(col,_),file}) =
              if emitLineSpans
              then instr 
              else 
 	      line {line=l,col=col,file=file}
          | translateInstr other = other
    in
       {translateInstr = translateInstr,
	startDbg = fn projname => (),
	finishDbg = fn () => ()}
    end
    

(* embed instructions in left and right line info from bound vars *)
(*@TODO: only produces correct results if at most one var in xs 
        (instrs should be a list of instr lists, one for each var) 
*)


fun addNullLine(instrs) = 
    if Controls.get(debug) then 
	Join(Single nullLine, instrs)
    else instrs

fun addLine ({left={line=ll,col=lc},
		 right={line=rl,col=rc},
		 file},instrs) =
  if Controls.get(debug) 
  then Join (Single (linespan {line=(ll,rl),col=(lc,rc),file=file}),
        instrs)
  else instrs

end


