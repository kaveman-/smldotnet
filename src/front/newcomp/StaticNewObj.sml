(*----------------------------------------------------------------------*)
(*  Compilation of RTInstrs.staticnewobj   (*@HACK: crusso*)            *)
(*  Background: the CLR x86 JIT introduces a new temp for each call to  *)
(*  newobj which leads to large stack frames for methods that allocate  *) 
(*  heavily.                                                            *)
(*                                                                      *) 
(*  To avoid this, we selectively delegate staticnewobj instructions to *)
(*  a static wrapper method (called .sctor),                            *)
(*  The wrappers are created on demand when rewriting the newstaticobj  *)
(*  instructions to call to .sctor, depending                           *)
(*  on the number of slots and allocations in a method body.            *)
(*  StaticNewObj.finish(..) emits any required .sctor methods           *)
(*  using ilasm's class augmentation syntax.                            *)
(*  NB: assumes there is only one constructor per class!                *)
(*----------------------------------------------------------------------*)

structure StaticNewObj =
struct

local 
  open CompileEnv
  open CompileOps
  open ListOps  
  open RTInstrs
  infixr 5 ++ +$ $+

val genStaticnewobj = Controls.add false "codegen.staticnewobj" 

val staticConstructor = Id.fromString ".sctor"

val staticConstructors = ref (TyName.Map.empty: RTResult.Class TyName.Map.map)

fun addStaticConstructor {classty, argtys} =
    let val tn = case classty of 
                     VMTy.Class tn => tn
                   | VMTy.ValueClass tn => tn
                   | _ => Debug.fail "StaticNewObj.addStaticConstructor"
    in 
        case TyName.Map.find(!staticConstructors,tn) of 
            SOME class => ()
          | NONE => 
            let val args = map (fn rep => (Id.fromString "", rep)) argtys
                val sctor = {name = staticConstructor, 
                             override = NONE,
                             attributes = [], 
                             flags = assemblystaticnoinlining,
                             resty = SOME classty,
                             args = args,
                             stack = Int.max(1, foldr op+ 1 (map (units o (fn(_,ty) => ty)) args)),
                             locals = [],
                             code =
                             ((fromList  (mapi (fn (i,rep) => ldarg i) argtys)) +$
                                                                                RTInstrs.newobj { classty = classty,argtys = argtys}) +$
                                                                                                                                      ret
                                                                                                                                      }
                val class: RTResult.Class = {name = RTOps.classRepToLongid classty,
                                             attributes = [],
                                             super = RTOps.object, (* dummy *)
                                             interfaces = [],
                                             flags = Symbol.Set.singleton CompileOps.augmentation, (* hack *)
                                             fields = [],
                                             methods = [sctor]
                                             }
            in 
                staticConstructors := TyName.Map.insert(!staticConstructors,tn,class)
            end
    end

fun countnewobj acc Empty = acc
  | countnewobj acc (Join(x,y)) = countnewobj (countnewobj acc x) y 
  | countnewobj acc (Single(newobj _)) = acc + 1
  | countnewobj acc (Single(staticnewobj _)) = acc + 1
  | countnewobj acc (Single(_)) = acc

fun newstaticobjToCall (staticnewobj (constructor as {classty,argtys})) =
       (addStaticConstructor constructor;
        Single(call {name = staticConstructor, classty = classty, argtys= argtys,resty = SOME classty}))
  | newstaticobjToCall other = Single other

in

fun init (instrs,maxstack) constructor =
    (instrs +$ staticnewobj constructor, 
     Int.max(maxstack,1))

fun start() =  staticConstructors := TyName.Map.empty                                                   

fun finish(dosave,save) = 
    if dosave
    then
        TyName.Map.app save (!staticConstructors)
    else ()
    before
    staticConstructors := TyName.Map.empty


fun compile(numslots,bodyinstrs) =
    if Controls.get genStaticnewobj
       andalso (countnewobj 0 bodyinstrs + numslots) >= 32 
    then mapInstrs newstaticobjToCall bodyinstrs
    else bodyinstrs (* any remaining staticnewobj will get emitted as newobj *)
   
end (* of local open *)

end (* of struct *)


