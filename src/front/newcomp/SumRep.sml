structure SumRep :>
sig
val init : unit -> unit
val tagRep : unit -> VMTy.Type
val compileIndexFromTag : RTInstrs.Instrs * int -> RTInstrs.Instrs * int
val compileTagFromIndex : {con:Id.id,index:int} -> RTInstrs.Instr 
val finish: bool * (RTResult.Class -> unit) -> unit;
end

=
struct
local
     open CompileOps RTInstrs
     infixr 5 ++ $+
     infix 6 +$
in

val showConTags = Controls.add false "showConTags"

val tagSupply = ref (Symbol.Map.empty : {ord:int,index:int} list ref Symbol.Map.map);

val tagCount = ref 0;

fun newTag index =  let val tag = {ord = !tagCount, index = index}
                    in
                        tagCount := (!tagCount)+1;
                        tag
                    end

val genTags =  ref true;

val tagMask = 65535
fun tagToInt {ord,index} = ord*65536+index 

fun getConstructorTag {con=si,index=index} = 
     case Symbol.Map.find(!tagSupply,si) of 
        NONE => (* assign an initial tag to this constructor si *)
                let val tag = newTag index 
                in
                   tagSupply := Symbol.Map.insert(!tagSupply,si,ref [tag]);
                   tagToInt tag
                end
     |  SOME r =>
            let (* find an existing tag for this index, or create one *)
                fun find []:{ord:int,index:int} = 
                       (* assign an tag to this constructor and index *)
                       let val tag = newTag index
                       in
                           r := tag :: (!r);
                           tag
                       end
                  | find ((tag as {ord=ord,index=index'})::tags) =
                       if index = index' 
                       then tag
                       else (find tags)
            in
                  tagToInt (find (!r))
            end

fun tagRep() = VMTy.ValueClass(valOf(MILTy.fromTyname(MILTy.sumTagType())))

fun tagLiterals() =
   Symbol.Map.foldli (fn (si,ref tags,literals) =>
                case tags of
                  [tag] => 
                   {name = si,
                    value = tagToInt tag}::
                  literals
                | tags => 
                  ((List.map (fn (tag as {ord,index}) => 
                    {name = Id.fromString(Id.toString(si)^"."^Int.toString index),
                     value = tagToInt tag})
                   tags)
                  @ literals)) [] (!tagSupply)


val maskCon = Constants.INT (RTInt.fromInt tagMask)

fun compileIndexFromTag(instrs,stack) =
        if !genTags
        then (instrs ++ 
              (ldc maskCon $+ 
              $ (And RTOps.int)),
              stack+1)
        else (instrs,stack) 

fun compileTagFromIndex({con=si,index=index}) =         
        let val tag = if !genTags then getConstructorTag{con=si,index=index} else index
        in
            ldc (Constants.INT (RTInt.fromInt tag))
        end

fun init() = (genTags :=  not(VMTy.eq(tagRep (),RTOps.int));
              tagSupply := Symbol.Map.empty)

(*
fun makeToString(class, superclass, pairs, passon) =
  {
    name = RuntimeNames.instanceConstructor, 
    override = NONE,
    attributes = [], (*@TODO: consider non-empty attributes *)
    flags = assembly,
    resty = SOME RTOps.string,
    args = [],
    stack = Int.max(2, foldr op+ 1 (map (units o #2) (passon @ pairs))), 
    locals = [],
    code =
      (ldarg 0 $+ 
      call 
      { name = RuntimeNames.instanceConstructor, classty = superclass, 
        argtys = map #2 passon, resty = NONE } $+
      flat (mapi (fn (i,(name,rep)) => 
        fromList [ldarg 0, ldarg (i+1), stfld { classty = class, name = name, fldty = rep }]) pairs) +$
      ret
  }
*)

fun dumpConTags () = 
(Debug.print "\nConstructor Tags:";
 Symbol.Map.appi (fn (si,ref tags) =>
        List.app (fn (tag as {ord,index}) =>
         Debug.print ("\n" ^ Id.toString si ^ ": " ^ "(" ^ Int.toString index ^ ")" ^ Int.toString (tagToInt tag)))
        tags)
 (!tagSupply)
)


fun finish(dosave, save) = 
  (if !genTags andalso dosave
   then save (CompileProdSum.makeTagClass(tagRep(),tagLiterals()))
   else ();
   if Controls.get showConTags then dumpConTags () else ();
   tagSupply := Symbol.Map.empty)
end (*local *)
end
