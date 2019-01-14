(*======================================================================*)
(* Generate code for a special operation (language ext. or primitive)   *)
(*======================================================================*)
structure CompileSpecial :> COMPILESPECIAL = 
struct

local
  open CompileEnv 
  open RTInstrs
  infixr 5 ++ $+
  infix 5 +$
in

val primops =
foldr (fn ((x,i),m) => Symbol.Map.insert(m, Id.fromString x, i)) 
Symbol.Map.empty
[
  ("add",       add),
  ("And",       And),
  ("arraylength",fn r => ldlen),
  ("div",       Div),
  ("mul",       mul),
  ("neg",       neg),
  ("or",        or),
  ("rem",       rem),
  ("shl",       shl),
  ("shr",       shr),
  ("sub",       sub),
  ("ushr",      shr_un),
  ("xor",       xor),
  ("lt",        fn r => cmp (lt, r)),
  ("gt",        fn r => cmp (gt, r)),
  ("eq",        fn r => cmp (eq, r)),
  ("lt_un",     fn r => cmp_un (lt, r)),
  ("gt_un",     fn r => cmp_un (gt, r)),
  ("div_un",    div_un),
  ("rem_un",    rem_un),
  (* checked arithmetic *)
  ("add_ovf",   add_ovf),
  ("mul_ovf",   mul_ovf),
  ("sub_ovf",   sub_ovf),
  ("add_ovf_un",add_ovf_un),
  ("mul_ovf_un",mul_ovf_un),
  ("sub_ovf_un",sub_ovf_un)
]

(* See signature for details *)
fun compile (info as (optype, optrep, optname), argreps, argtys, resrep, maxstack) =
let val resstack = case resrep of NONE => 0 | SOME rep => units rep
in
case optype of
  Ext.Invoke =>
  let val SOME name = optname
  in
    case optrep of
      SOME classrep =>
      ($ (call { name = name,
                 classty = classrep,
                 argtys = argreps, 
                 resty = resrep}),  
        Int.max(maxstack, resstack) 
      )

    | NONE =>
      let
        val classty = hd argreps
        val instr = 
          case optype of 
            Ext.InvokeSuper => callinst
          | Ext.InvokeInterface => callvirt
          | _ => case classty of VMTy.ValueClass _ => callinst
                 | _ => callvirt
      in
        ($ (instr {name = name, 
                   classty = classty,
                   argtys = tl argreps, 
                  resty = resrep }),
         Int.max(maxstack, resstack))
      end
  end
| Ext.InvokeInterface =>
  let val SOME name = optname
  in
    case optrep of
      SOME classrep =>
      ($ (call { name = name,
                 classty = classrep,
                 argtys = argreps, 
                 resty = resrep}),  
        Int.max(maxstack, resstack) 
      )

    | NONE =>
      let
        val classty = hd argreps
        val instr = 
          case optype of 
            Ext.InvokeSuper => callinst
          | Ext.InvokeInterface => callvirt
          | _ => case classty of VMTy.ValueClass _ => callinst
                 | _ => callvirt
      in
        ($ (instr {name = name, 
                 classty = classty,
                 argtys = tl argreps, 
                 resty = resrep }),
         Int.max(maxstack, resstack))
      end
  end
| Ext.InvokeSuper =>
  let val SOME name = optname
  in
    case optrep of
      SOME classrep =>
      ( $(call { name = name,
                 classty = classrep,
                 argtys = argreps, 
                 resty = resrep}),  
        Int.max(maxstack, resstack) 
      )

    | NONE =>
      let
        val classty = hd argreps
        val instr = 
          case optype of 
            Ext.InvokeSuper => callinst
          | Ext.InvokeInterface => callvirt
          | _ => case classty of VMTy.ValueClass _ => callinst
                 | _ => callvirt
      in
        ($ (instr {name = name, 
                   classty = classty,
                   argtys = tl argreps, 
                   resty = resrep }),
         Int.max(maxstack, resstack))
      end
  end

| Ext.GetField =>
  let val SOME name = optname
  in
    case optrep of
      SOME classrep =>
      ($(ldsfld { name = name, classty = classrep, fldty = valOf resrep }), Int.max(maxstack,resstack))

    | NONE =>
      ($(ldfld { name = name, classty = hd argreps, fldty = valOf resrep }), Int.max(maxstack,resstack))
  end
      
| Ext.PutField =>
  let val SOME name = optname
  in
    case optrep of
      SOME classrep =>
      ($(stsfld { name = name, classty = classrep, fldty = hd argreps }), maxstack)

    | NONE =>
      ($(stfld { name = name, classty = hd argreps, fldty = hd (tl argreps) }), maxstack)
  end

| Ext.Cast =>
    let val argrep = hd argreps
        val resrep = valOf resrep
    in
        case (argrep,resrep) of
            (VMTy.ValueClass _,VMTy.Class _) => 
            (fromList [box argrep,conv (argrep(*type incorrect, but unused*),resrep)], maxstack) (* box - Beta2 *)
          | (VMTy.Class _,VMTy.ValueClass _) => 
            (fromList [unbox resrep,ldobj resrep], maxstack)
          | (VMTy.Class _, VMTy.Class _) => 
            ($(conv (argrep,resrep)), maxstack)
          | _ =>
            (* must be the identity *)
            (Empty,maxstack)
    end


| Ext.NopCast =>
  (Empty, maxstack)

| Ext.IsInst =>
  ($(isinst (valOf resrep)), 1)

| Ext.InstanceOf =>
  (*@TODO: optimize, here and in its use during translation *)
  (fromList [isinst (valOf optrep),
      ldnull,
      cmp(eq,RTOps.object),
      ldc(Constants.INT(RTInt.fromInt 0)),
      cmp(eq,RTOps.int)],
     Int.max(maxstack, 2))

| Ext.Prim p =>
  (case Id.toString p of
    "newarray" => 
    let val VMTy.Array ty = valOf resrep 
    in ($ (newarr ty), maxstack) end

  | "ref" =>
    StaticNewObj.init (RTInstrs.Empty, maxstack) { classty = valOf resrep, argtys = argreps }

  | "arrayload" => ($ (ldelem (valOf resrep)), maxstack)
  | "arraystore" => ($ (stelem (List.nth(argreps, 2))), maxstack)
  | "arraycopy" =>
    (fromList [call { name = Id.fromString "Copy", classty = RTOps.array,
       argtys = [RTOps.array, RTOps.int, RTOps.array, RTOps.int, RTOps.int], 
       resty = NONE }], maxstack)

  | "getLocMessage" =>
    ($ (ldsfld { name = RepNames.exnLocMessage, classty = TyRep.globs (),
                 fldty = RTOps.string }), maxstack)
  | "exn" =>
    let val countfield = 
    { name = RepNames.exnClassCount, fldty = RTOps.int, classty = TyRep.globs () }
    in
      (fromList [ldsfld countfield, dup RTOps.int, 
        ldc (Constants.INT (RTInt.fromInt 1)),
        add RTOps.int, stsfld countfield], 3)
    end

  | "localAlloc" =>
    let val {store,load,loada} = RTLocals.newTmp (hd argreps)
    in 
       (store ++ loada,maxstack)
    end

  | "!" =>
    let
      val rep = valOf resrep
      val refrep = hd argreps
      val refty = hd argtys
      val SOME (tys,refkind) = MILTy.fromRefty refty
      val maxstack = Int.max(maxstack, units rep)
    in 
      case MILTys.fromRefKind refkind of
            SOME MILTys.Heap => ($(ldfld { name = RepNames.argLabel 0, classty = refrep, fldty = rep }),maxstack)
          | SOME MILTys.Address => ($(ldind rep), maxstack)
          | SOME (MILTys.Field(classtn,field)) => 
                (let val classrep = TyRep.tyToRep CompileEnv.empty (MILTy.tyname classtn)
                 in case classrep of
                     VMTy.Class _ =>
                         ($(ldfld { name = field, classty = classrep, fldty = rep}),maxstack)
                   | VMTy.ValueClass _ => (*@TODO: avoid copying just to take address *) 
                         ($(ldfld {name = field, classty = classrep, fldty = rep}), maxstack)
                   | _ => 
                     (Debug.print ("\nWarning: CompileSpecial:Deref:1 encountered unknown reference type "^MILTy.toString refty);
                      let val {load,...} = RTLocals.newTmp rep
                      in (load,units rep)
                      end
                      )

                 end)
          | SOME (MILTys.Static(classtn,field)) => 
                (let val classrep = TyRep.tyToRep CompileEnv.empty (MILTy.tyname classtn)
                 in (* naive (cmp,instrs@[pop,ldsfld {name = field, classty = classrep, fldty = rep}],stack) *)
                    (pop $+ $ (ldsfld {name = field, classty = classrep, fldty = rep}),units rep)
                 end)
         | NONE => 
                (Debug.print ("\nWarning: CompileSpecial:Deref:2 encountered unknown reference type "^MILTy.toString refty);
                 (* This must be dead code, so simply pop the reference and load an appropriate default to satisfy the verifier *)
                 let val {load,...} = RTLocals.newTmp rep
                 in
                     (pop $+ load,units rep)
                 end)
    end

  | ":=" =>
  let
    val refty = hd argtys
    val refrep =  hd argreps
    val rep = List.nth(argreps, 1)
    val (_,refkind) = valOf (MILTy.fromRefty refty)
    val maxstack = Int.max(maxstack, units refrep)
  in
       case MILTys.fromRefKind refkind of
            SOME MILTys.Heap =>
                ($(stfld { name = RepNames.argLabel 0, fldty = rep, classty = refrep }), maxstack)
          | SOME MILTys.Address =>
                ($(stind rep), maxstack)
          | SOME(MILTys.Field(classtn,field)) => 
                (let val classrep = TyRep.tyToRep CompileEnv.empty (MILTy.tyname classtn)
                 in
                     (case classrep of
                          VMTy.Class _ => 
                              ($(stfld {name = field, classty = classrep, fldty = rep}), maxstack)
                        | VMTy.ValueClass _ => 
                              ($(stfld {name = field, classty = classrep, fldty = rep}), maxstack)
                        | _ =>  
                          ( Debug.print ("\nWarning: CompileSpecial:Assign:1 encountered unknown reference type "^MILTy.toString refty);
                           (Empty,0)))
                 end)
          | SOME(MILTys.Static(classtn,field)) => 
                (let val classrep = TyRep.tyToRep CompileEnv.empty (MILTy.tyname classtn)
                 in
                   ($(stsfld {name = field, classty = classrep, fldty = rep}) +$ pop, maxstack)
                 end)
          | NONE => ((* This must be dead code, so simply pop the reference and value to satisfy the verifier *)
                     Debug.print ("\nWarning: CompileSpecial:Assign:2 encountered unknown reference type "^MILTy.toString refty);
                     (pop $+ $pop,0))
  end

  (* conversions *)

  | "conv" => (fromList [conv (hd argreps,valOf resrep)],maxstack)
  | "conv_ovf" => (fromList [conv_ovf (hd argreps,valOf resrep)],maxstack)
  | "conv_ovf_un" => ($ (conv_ovf_un (hd argreps,valOf resrep)),maxstack)
  | "badapp" => 
    (case resrep of
      NONE => (fromList [ldnull, throw], 1)
    | SOME rep => 
      let val {load,...} = RTLocals.newTmp rep
      in   
        (fromList [ldnull, throw] ++ load, Int.max(maxstack,1))
      end
    )
  | s =>
    if Id.equal(Id.fromString s, RepNames.globalInitPrim)
    then ($(call { name = RepNames.globalInitPrim, classty = TyRep.globs(), argtys = [], resty = NONE}), maxstack)
    else (case Symbol.Map.find(primops, p) of
              NONE =>
                  Debug.fail ("CompileSpecial.compile: invalid primitive operation " ^ 
                              Id.toString p)
            | SOME f => ($ (f (hd argreps)), maxstack))
  
  )
| Ext.Line l => 
        (RTInstrs.addLine(l,Empty),maxstack)
| _ =>
  Debug.fail ("CompileSpecial.compile: " ^ ExtOps.toString optype)
end
      
end (* of local *)

end


