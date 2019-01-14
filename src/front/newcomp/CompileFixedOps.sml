(*======================================================================*)
(* Auxiliary operations for compilation of fixed classes (E*, R*, etc.) *)
(*======================================================================*)
structure CompileFixedOps =
struct

local 
  open CompileEnv
  open CompileOps
  open ListOps  
  open RTInstrs
  infixr 5 ++ +$ $+
in

(*----------------------------------------------------------------------*)
(* Make a list of numbered field definitions                            *)
(*----------------------------------------------------------------------*)
fun makeProdFields flags tys =
  mapi (fn (i, ty) => 
  {
    name = RepNames.argLabel i, 
    flags = flags, 
    ty = ty,
    value = NONE
  }) tys

(*----------------------------------------------------------------------*)
(* Create a constructor which calls the superclass constructor and fills*)
(* in instance variables with the parameters it has been passed.        *)
(*----------------------------------------------------------------------*)
fun makeInitMethod (class, superclass, pairs, passon) =
  {
    name = RuntimeNames.instanceConstructor, 
    override = NONE,
    attributes = [], (*@TODO: consider non-empty attributes *)
    flags = assembly,
    resty = NONE,
    args = pairs @ passon,
    stack = Int.max(2, foldr op+ 1 (map (units o #2) (passon @ pairs))), 
    locals = [],
    code =
      (if RTOps.isRefType class
      then ldarg 0 $+ 
        fromList (mapi (fn (i,rep) => ldarg (length pairs + 1 + i)) passon) +$
        call 
        { name = RuntimeNames.instanceConstructor, classty = superclass, 
          argtys = map #2 passon, resty = NONE } else Empty) ++
      flat (mapi (fn (i,(name,rep)) => 
        fromList [ldarg 0, ldarg (i+1), stfld { classty = class, name = name, fldty = rep }]) pairs) +$
      ret
  }




(*----------------------------------------------------------------------------*)
(* Create diagnostic                                                          *)
(* ToString() and ToString(int depth) methods for product or sum type [class] *)
(*@TODO: add static depth field for run-time control?                         *)
(*@TODO: distinguish SOME/REF values somehow, perhaps by adding a tag         *)
(*----------------------------------------------------------------------------*)
fun makeToStringMethods(class,tagOpt:Field option, fields) = 
if not(Controls.get(debug)) then []
else
let 
    (* this is a gross name-based hack for recognising product and sum types *)
    fun isProdSumRep rep = case rep of
          VMTy.Class tn =>  
          case TyName.fromExternal tn of
              SOME (assem,[ns,id],_) => Id.equal(assem,Id.fromString "")
                                   andalso Id.equal(ns,RepNames.internalNamespace)
                                   andalso (String.isPrefix RepNames.tuplePrefix (Id.toString id) orelse
                                            String.isPrefix RepNames.conPrefix (Id.toString id) orelse
                                            Id.equal(RepNames.dataClass,id))
        | _ => false                            
    val depth = 1
    val toStringMethod = Id.fromString (RuntimeNames.toStringMethod)
    val ConcatMethod = Id.fromString "Concat"
    fun ldstr s = ldc (Constants.STRING (UString.fromString s))
    fun sconcat((inst1,stack1),(inst2,stack2)) = 
         (inst1 ++ inst2 +$
          call {name = ConcatMethod, classty = RTOps.string, 
               argtys = [RTOps.string,RTOps.string], resty = SOME RTOps.string },
          Int.max(stack1,stack2) + 1)
    fun fieldToString{name,flags,ty,value} = 
    case ty of 
       VMTy.Class tn =>
       let  val null = RTLabels.freshLabel()
            val cont = RTLabels.freshLabel()
       in
        (ldarg 0 $+       
         ldfld { classty = class, name = name, fldty = ty } $+
         dup ty $+
         brfalse (ty,null) $+
         ((if isProdSumRep ty 
           then ldarg depth $+ 
                $(callvirt { name = toStringMethod, classty = ty, 
                  argtys = [RTOps.int], resty = SOME RTOps.string })
           else
                $(callvirt { name = toStringMethod, classty = RTOps.object, 
                  argtys = [], resty = SOME RTOps.string })) ++
         br cont $+
         label null $+
         pop $+
         ldstr "null" $+ 
         $(label cont)),
         2)
        end
      | VMTy.ValueClass tn =>
         (ldarg 0 $+       
          ldfld { classty = class, name = name, fldty = ty } $+
          box ty $+
          $ (callvirt { name = toStringMethod, classty = RTOps.object, 
             argtys = [], resty = SOME RTOps.string }),
          1) 
      | VMTy.Array _ =>
         (ldarg 0 $+       
          ldfld { classty = class, name = name, fldty = ty } $+
          $ (callvirt { name = toStringMethod, classty = RTOps.object, 
             argtys = [], resty = SOME RTOps.string }),
          1) 
      | _ => ($ (ldstr "?"),1)
     val args  =
         case List.map fieldToString fields of
                  [field] => field
                | (field::fields) =>
                   let val fields = 
                      List.foldr (fn (field,rest) => (sconcat(sconcat(($(ldstr ","),1),field),rest)))
                                 ($ (ldstr ")"),1)
                      fields
                   in
                      sconcat(sconcat(($ (ldstr "("),1),field),fields)
                   end
                | [] => ($ (ldstr ""),1)
     val (str,maxstack) = case tagOpt of 
                  NONE => args
                | SOME (tagfield as {fldty,...}) => 
         let val tagstr = (ldarg 0 $+             
                           ldfld tagfield $+
                           box fldty $+
                           $ (callvirt { name = toStringMethod, classty = RTOps.object, 
                                         argtys = [], resty = SOME RTOps.string }),
                           1)
         in case fields of 
                 [field] => sconcat(sconcat(tagstr,($  (ldstr " "),1)),args)
               | (field::fields) => sconcat(tagstr,args)
               | [] => tagstr
         end

     val toodeep = RTLabels.freshLabel()
     val cont = RTLabels.freshLabel()
     val body = ldarg depth $+  ldc (Constants.INT (RTInt.fromInt 0)) $+ cmp (lt,RTOps.int) $+ brtrue (RTOps.bool,toodeep) $+ 
                  ldarg depth $+ ldc (Constants.INT (RTInt.fromInt 1)) $+ sub RTOps.int $+ starg depth $+
                  ((str +$ ret) ++ 
                   (label toodeep $+ ldstr "..." $+ $ ret))

in 
[ {
    name = Id.fromString RuntimeNames.toStringMethod, 
    override = NONE,
    attributes = [], 
    flags =  public,
    resty = SOME RTOps.string,
    args = [(Id.fromString "depth",RTOps.int)],
    stack = Int.max(2,maxstack),
    locals = [],
    code = body
  },
  {
    name = Id.fromString RuntimeNames.toStringMethod, 
    override = NONE,
    attributes = [], 
    flags = public,
    resty = SOME RTOps.string,
    args = [],
    stack = 2,
    locals = [],
    code = ldarg 0 $+ 
           ldc (Constants.INT (RTInt.fromInt 0)) $+ 
           callvirt { name = toStringMethod, classty = class, argtys = [RTOps.int], resty = SOME RTOps.string } $+ 
           $ ret
  }
]
end

fun init (instrs,maxstack) constructor =
  (instrs +$ newobj constructor, Int.max(maxstack,1))

end (* of local open *)

end (* of struct *)


