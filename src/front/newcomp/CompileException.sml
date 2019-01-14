(*======================================================================*)
(* Compile an exception class       					*)
(*======================================================================*)
structure CompileException :> COMPILEEXCEPTION =
struct

local  
  open CompileEnv CompileFixedOps CompileOps
  structure I = RTInstrs
in

val exnLocs = Controls.add true "debug.exnLocs"

fun makeExnClass (superrep, classrep, reps, longid) =
let
  val holdsstring = 
  length reps = 1 andalso 
  (case reps of
    [VMTy.Class c] => TyName.eq(c, TyNames.stringTyName)
  | _ => false)
in
  {
    name = RTOps.classRepToLongid classrep,
    attributes = [],
    super = superrep,
    interfaces = [],
    flags = privatesealed,
    fields = makeProdFields assembly (if holdsstring then [] else reps),
    methods = 
    [
    {
      name = RuntimeNames.instanceConstructor, 
      override = NONE,
      attributes = [], (*@TODO: consider non-empty attributes *)
      flags = assembly,  
      resty = NONE,
      args = map (fn rep => (Id.fromString "", rep)) reps,
      stack = 3,
      locals = [],
      code =
        I.fromList (
        I.ldarg 0 :: 
        (if holdsstring then I.ldarg 1 else I.ldnull) ::
        I.call 
        { name = RuntimeNames.instanceConstructor, classty = superrep, 
          argtys = [RTOps.string], resty = NONE } ::
        (if holdsstring then [] else List.concat (ListOps.mapi (fn (i,rep) => 
        [I.ldarg 0, I.ldarg (i+1), 
         I.stfld { classty = classrep, name = RepNames.argLabel i, 
           fldty = rep }]) reps)) @ 
        [I.ret] )
     },
    {
       name = Id.fromString "ExnName",
       override = NONE,
       attributes = [], (*@TODO: consider non-empty attributes *)
       flags = public,
       resty = SOME RTOps.string,
       args = [],
       stack = 3,
       locals = [],
       code = I.fromList [I.ldc (Constants.STRING (UString.fromString longid)), I.ret]
     }]
  }
end


end (* of local open *)  
end
