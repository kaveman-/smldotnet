(*======================================================================*)
(* Compile a continuation computation term.				*)
(*======================================================================*)
structure CompileCont :> COMPILECONT =
struct

local 
  open MILTerm RepNames CompileEnv TyRep ListOps
  open RTInstrs CompileValCmp
  infixr 5 ++ $+ 
  infix 6 +$
in

val genSwitch = Controls.add true "codegen.genSwitch"

(* Emit dummy calls to ensure GC safe points exist before backward branches,
   avoiding the creation of fully interruptiple methods with large GC encodings.
*)
val GCSafePoints = Controls.add false "codegen.GCSafePoints"

fun GCSafePoint() = 
    if Controls.get GCSafePoints
    then $ (call {name = CompileOps.GCSafePointMethod,
                  classty = globs(),
                  argtys = [],
                  resty = NONE})
    else Empty

(* number of cases needed to generate a switch *)
val switchThreshold = 3:Int32.int

(* min ratio of cases/span for which to generate switch *)
val switchRatio = {numerator=1:Int32.int,denumerator=2:Int32.int}

fun int i = Constants.INT (RTInt.fromInt i)

(*----------------------------------------------------------------------*)
(* Compile code for a continuation computation term.	              	*)
(* Arguments:                                                           *)
(*   whether or not we're inside an exception handler			*)
(*   the term e                                                         *)
(*   the compilation environment env                                    *)
(* Results:                                                             *)
(*   the MIL type of e                                                  *)
(*   a list of instructions -- the tail of a block                      *)
(*   a bound on the stack used by the instructions			*)
(*----------------------------------------------------------------------*)
fun compile (env : Env) e =
let
  fun lookupBlock (env : BlockEnv, Var x) = 
      (case Var.Map.find(env, x) of
        SOME (ty, isrec, instrs) => SOME (x, ty, isrec, instrs)
      | NONE => NONE)

    | lookupBlock (env, TApp(Var x, tys)) =
      (case Var.Map.find(env, x) of
        SOME (polyty, isrec, instrs) =>
        SOME (x, MILTy.app (MILTy.abs (valOf(MILTy.fromForall polyty)), tys), isrec, instrs)

      | NONE => NONE)
   
    | lookupBlock (env, _) = NONE

(*......................................................................*)
(* Compile a mutually-recursive set of local definitions.		*)
(* The body of the let is used to determine whether or not to allocate  *)
(* new locals for the arguments to the block.				*)
(*......................................................................*)
fun compileBlock (env : Env) (tyvars, RecFun defs) =
    let 

    (* The environment under which the definitions are compiled *)
    val defnenv = envPlusTyVars (clearStacked env) tyvars

    val (defnenv,bodyenv) = foldl (fn ((f,g,(xs,_),cty),(defnenv,bodyenv)) =>
      let val funty = MILTy.arrow(map #2 xs, cty)
          val (defnenv, bindinstrs) = envPlusTypedVars defnenv xs
      in
        (envPlusBlock defnenv (#1 g, (funty,true,bindinstrs)), envPlusBlock bodyenv (#1 f, (MILTy.forall(tyvars, funty),false,bindinstrs)))
      end) (defnenv,env) defs

    (*..................................................................*)
    (* Compile one of the definitions              		        *)
    (*..................................................................*)
    fun compileDefn ((f,_), (g,_), (xs, body), _) =
    let
      (* The environment under which the definitions are compiled *)
      val (bodyty, bodyinstrs, stack) = compile (defnenv) body
      val bodyinstrs = CompileValCmp.scopeTypedVars(defnenv,xs,bodyinstrs)
    in
      (label (RTLabels.varLabel f) $+ label (RTLabels.varLabel g) $+ bodyinstrs, Int.max(length xs,stack))
    end

    val (instrss, stacks) = ListPair.unzip (map compileDefn defs)
  in
    (flat instrss, bodyenv, max stacks)
  end
 
(*......................................................................*)
(* Compile a non-recursive local block.					                *)
(*......................................................................*)
  | compileBlock env (tyvars, Fun (fsi as (f,_), (xs, body))) =
    let
      (* The environment under which the definitions are compiled *)
      val (defnenv, bindinstrs) = envPlusTypedVars (clearStacked env) xs
      val (bodyty, bodyinstrs, stack) = compile (defnenv) body
      val bodyinstrs = CompileValCmp.scopeTypedVars(defnenv,xs,bodyinstrs)
      val funty = MILTy.arrow(map #2 xs, bodyty)
    in
      (label (RTLabels.varLabel f) $+ bodyinstrs,
      envPlusBlock env (f, (MILTy.forall(tyvars, funty), false, bindinstrs)), Int.max(length xs,stack))
    end           

  fun compileNonContTail () =
  case #handler env of
    NONE =>
    let val (cty, reps, instrs, stack) = CompileValCmp.compile ($tailcall) env e
    in
      (cty, instrs +$ ret, stack)
    end

  | SOME { cont, ... } =>
    let val (cty, reps, instrs, stack) = CompileValCmp.compile Empty env e
    in
      (cty, instrs ++ cont, stack)
    end


fun jconToInt jcon = RTInt.toInt32 (valOf (Constants.toInt jcon))

fun compileSwitch env (tagToInt,tagToJcon,tagRep) (cases,optdefault,cty) =
       if not(Controls.get genSwitch) then 
           NONE 
       else
       let 
           val len = Int32.fromInt (length cases)
           (* sort the cases *)
           val cases = QuickSort.sort 
                           (fn ((tag1,_),(tag2,_)) => tagToInt tag1 <= tagToInt tag2)                                                                                                 cases
           (* are they dense enough? *)
           val baseTag = #1 (hd cases)
           val isDense = 
               let 
                   val min = tagToInt (#1 (hd cases))
                   val max = tagToInt (#1 (List.last cases))
                   val span = max-min+1
                   val {numerator,denumerator} = switchRatio
               in  (len * denumerator) div span >= numerator
               end
           val baseJcon = tagToJcon (#1 (hd cases))
       in
           if isDense andalso len >= switchThreshold then 
           let 
               val defaultLabel = RTLabels.freshLabel ()
               val (defaultinstrs,defaultstack) = 
               case optdefault of SOME e => 
                                  let val (cty,defaultinstrs,defaultstack) = compile env e 
                                  in
                                     (label defaultLabel $+ defaultinstrs,defaultstack)
                                  end
                                | NONE => ($ (label defaultLabel), 0)
                                          
               fun branches (_,[]) = ([],Empty,0)
                 | branches (i,allcases as (tag, ([], e))::cases) =
                   if i < tagToInt tag 
                   then let val (labels, restinstrs, reststack) = branches (i+1,allcases)
                        in
                            (defaultLabel::labels,restinstrs,reststack)
                        end
                   else
                       let
                           val l = RTLabels.freshLabel ()
                           val (cty, instrs, stack) = compile env e
                           val (labels, restinstrs, reststack) = branches (i+1,cases)
                           val stack =  Int.max(stack, reststack)
                       in
                           (l::labels,
                            label l $+ instrs ++ restinstrs,
                            Int.max (stack, reststack))
                       end
                           
               val (labels,casesinstrs,casesstack) = branches (tagToInt baseTag,cases)
                                                     
               val (testinstr,teststack) = 
                   if jconToInt baseJcon = 0 
                   then (Empty,0) 
                   else (ldc baseJcon $+ $ (sub tagRep),1)
           in
               SOME (cty,testinstr ++ (switch(labels) $+ defaultinstrs ++ casesinstrs),Int.max(1+teststack,Int.max(defaultstack,casesstack)))
           end
           else NONE
       end               

in
case e of 
(*......................................................................*)
(* Value binding                           				*)
(*......................................................................*)
  LetVal(x, v, body) =>
  let
    val (env', instrs, maxstack) = CompileValCmp.compValBinding env (v, x)
    val (cty, instrs', maxstack') = compile env' body
    val instrs' = CompileValCmp.scopeBoundVars(env',[x],instrs')
  in
    (cty, instrs ++ instrs', Int.max(maxstack, maxstack'))
  end

(*......................................................................*)
(* Moggi-let							      	*)
(*......................................................................*)
| Let(defn, (xs, body)) =>
  let
    val (env', instrs, maxstack) = CompileValCmp.compCmpBinding env (defn, xs)
    val (bodyty, instrs', maxstack') = compile env' body
    val instrs' = CompileValCmp.scopeTypedVars(env',xs,instrs')
  in
    (
      bodyty,
      instrs ++ instrs',
      Int.max(maxstack, maxstack')
    )
  end

(*......................................................................*)
(* Moggi-let with exception handlers					*)
(*......................................................................*)
| TryLet(try, handlers, (xs,cont)) =>
  let
    val startlabel = RTLabels.freshLabel ()
    val endlabel = RTLabels.freshLabel ()

    val (tryty, reps, tryinstrs, trystack) = CompileValCmp.compile Empty env try
    val resstack = sum (map units reps)
    val (env', bindinstrs) = envPlusTypedVars env xs
    val (contty, continstrs, contstack) = compile env' cont
    val continstr = CompileValCmp.scopeTypedVars(env',xs,continstrs)
    val (_,contrestys) = MILTy.fromCmp contty
    val (leaveinstrs, retinstrs) = 
    case #handler env of 
      SOME { cont, ... } => (cont, Empty)
    | NONE =>
      let val retlabel = RTLabels.freshLabel ()
      in case contrestys of
        [] => ($(leave retlabel), fromList [label retlabel, ret])
      | [ty] =>
        let
          val {store,load,...} = RTLocals.newResTmp (tyToRep env ty)
        in
          (store +$ leave retlabel, label retlabel $+ load +$ ret)
        end
      end
 
    fun compileHandlers ([], instrs, stack, table) = (rev table, instrs, stack)
      | compileHandlers (([(y,ty)], e)::handlers, instrs, stack, table) =
        let
          val rep = tyToRep env ty
          val (env',store) = if Census.isDead (#1 y) then (env,$pop) else envPlusNewVar env (y, ty)
          val (cty, instrs', stack') = compile (envPlusHandler env' (leaveinstrs, #1 y)) e
	  val instrs' = CompileValCmp.scopeBoundVars(env',[y],instrs')

          val handlerstart = RTLabels.freshLabel ()
          val handlerend = RTLabels.freshLabel ()

          val entry = 
          { startlabel = startlabel, endlabel = endlabel,
            handlerstart = handlerstart, handlerend = handlerend, 
            exnclass = rep }
        in
          compileHandlers (handlers, 
            label handlerstart $+ store ++ instrs' +$ label handlerend,
            Int.max(stack, Int.max(1,stack')),
            entry::table)
        end 

    val (table, handlerinstrs, handlerstack) = compileHandlers (handlers, Empty, 0, [])
  in
    (
      contty,
        label startlabel $+ tryinstrs ++ bindinstrs +$ leave endlabel ++
        label endlabel $+ continstrs ++       
        handlerinstrs ++ retinstrs ++
        fromList (map exn table),
      Int.max(Int.max(trystack, contstack), handlerstack)
    )
  end

(*......................................................................*)
(* Local block definition						*)
(*......................................................................*)
| LetFun(tyvars, LocalFun, fundef, body) =>
  let
    val (instrs1, env', maxstack) = compileBlock env (tyvars, fundef)
    val (cty, instrs2, maxstack') = compile env' body
  in
    (cty, instrs2 ++ instrs1, Int.max(maxstack, maxstack'))
  end

(*......................................................................*)
(* Special constant case					        *)
(*......................................................................*)
| CaseSCon (v, cases, optdefault, cty) =>
  let
    val (vty, rep, vinstrs, vstack) = CompileValCmp.compileVal env v
    val env = clearStacked env
              
    val isIntegral = isSome (Constants.toInt (#1 (hd cases)))

    fun compileLinearTests() =                        
        let fun loop(first, cases) =
                case (cases, optdefault) of
                    ([], SOME e) => if first then raise Fail "CompileCont.CaseSCon: singleton" else 
                                    let val (cty, instrs, stack) = compile env e in (cty, instrs, stack, true) end
                  | ([(_, ([], e))], NONE) => if first then raise Fail "CompileCont.CaseSCon: singleton" else
                                              let val (cty, instrs, stack) = compile env e in (cty, instrs, stack, true) end
                 | ((jcon, ([], e))::cases, _) =>
                   let
                       val (cty, instrs, stack) = compile env e
                       val l = RTLabels.freshLabel ()
                       val (_, rest, reststack, last) = loop(false, cases)
                       val testinstrs = 
                           case rep of
                               VMTy.Class _ =>
                               fromList [callvirt 
                                             { name = RuntimeNames.equalsMethod,
                                               classty = rep, 
                                               argtys = [RTOps.object], 
                                               resty = SOME RTOps.bool }, 
                                             brfalse (RTOps.bool, l)]
                             | _ => $ (bcmp_un (ne, rep, l))
                       val teststack =
                           Int.max(stack, Int.max(vstack, units rep * 2 + 1))
                   in
                       (
                        cty, 
                        (if last then Empty else $(dup rep)) ++ ldc jcon $+ testinstrs ++
                                                             (if last then Empty else $pop) ++ instrs ++ (label l $+ rest),
                        Int.max (teststack, reststack),
                        false
                        )
                   end
            val (cty,instrs,stack,_) = loop(true, cases)
        in
            (cty,instrs,stack)
        end

    val (cty, instrs, stack) = 
        if isIntegral then 
            (* try to compile as switch *)
            case compileSwitch env (jconToInt,fn jcon => jcon,rep) (cases,optdefault,cty) of 
               SOME res => (Stats.add ("cases/CasesSCon(switch)", length cases);
                            res)
            |  NONE => (Stats.add ("cases/CasesSCon(linear integral)", length cases);
                        compileLinearTests())
        else (Stats.add ("cases/CasesSCon(linear)", length cases);
              compileLinearTests())
  in
    (cty, vinstrs ++ instrs, max [vstack, stack])
  end

(*......................................................................*)
(* Case on enumeration and 1+ types                                    	*)
(*......................................................................*)
| Case(v, cases, optdefault, cty) =>
  let 
    val (ty, rep, vinstrs, vstack) = CompileValCmp.compileVal env v
    val env = clearStacked env
  in
    case MILTyRep.sumKind ty of
  (*..................................................................*)
  (* 1+ types                                                         *)
  (*..................................................................*)
      SOME (MILTyRep.OnePlus ty') => raise Fail "SimplifyCont: unexpected case on 1+"

  (*..................................................................*)
  (* Must be enumeration types 					      *)
  (*..................................................................*)
    | SOME (MILTyRep.Enum n) =>
      let
        val (vinstrs,vstack) =  SumRep.compileIndexFromTag(vinstrs,vstack)

        fun compileLinearTest() = 
            let fun loop cases =
                    case (cases, optdefault) of
                        ([], SOME e) => let val (cty, instrs, stack) = compile env e in (cty, instrs, stack, true) end
                      | ([(i, ([],e))], NONE) =>  let val (cty, instrs, stack) = compile env e in (cty, instrs, stack, true) end
                      | ((i, ([],e))::cases, _) =>
                        let
                            val (cty, instrs, stack) = compile env e
                            val l = RTLabels.freshLabel ()
                            val (_, rest, reststack, last) = loop cases
                        in
                            (
                             cty, 
                             (if last then Empty else $(dup rep)) ++
                              ldc (int i) $+ bcmp_un (ne, RTOps.int,l) $+
                              (if last then Empty else $pop) ++ instrs ++ (label l $+ rest),
                             max [units rep * 2 + 1, stack, reststack],
                             false
                             )
                        end
                 val (cty,instrs,stack,_) = loop cases
            in
                (cty,instrs,stack)                
            end
                            
        val (cty, instrs, stack) = 
             case compileSwitch env (Int32.fromInt, int,rep) (cases,optdefault,cty) of
                SOME res => (Stats.add ("cases/Case(enum switch)", length cases);
                             res)
              | NONE =>  (Stats.add ("cases/Case(enum linear)", length cases);
                          compileLinearTest())
      in
          (cty, vinstrs ++ instrs, max [vstack,stack])
      end
  end
      
(*......................................................................*)
(* Type-case							      	*)
(*......................................................................*)
| TypeCase(v, cases, optdefault, cty) =>
  let
    val _ = Stats.add ("cases/TypeCase", length cases)
    val (ty, rep, vinstrs, vstack) = CompileValCmp.compileVal env v
    val {load,store,...} = RTLocals.newTmp rep
    val env = clearStacked env
    fun loop cases =
      case (cases, optdefault) of
        ([], SOME e) => compile env e
      | ([(castty, ([x], e))], NONE) => 
        let
          val castrep = tyToRep env castty
          val (env', storecastlocal) = envPlusNewVar env (x, castty)
          val (cty, instrs, stack) = compile env' e
        in
            (cty, 
               load +$ conv (rep, castrep) ++ storecastlocal ++ instrs,
             Int.max(vstack, stack))
        end

      | ((castty, ([x], e))::cases, _) =>
        let
          val castrep = tyToRep env castty
          val (env', storecastlocal) = envPlusNewVar env (x, castty)
          val (cty, instrs, stack') = compile env' e
          val l = RTLabels.freshLabel ()
          val (_, rest, stack'') = loop cases
        in
            (cty, 
               load ++ fromList [isinst castrep,
               brfalse (RTOps.int, l)] ++ 
                load +$ conv (rep, castrep) ++ storecastlocal ++
               instrs ++ (label l $+ rest),
             max [1, vstack, stack', stack''])
        end
    val (cty, instrs, stack) = loop cases
  in
    (
      cty,
      vinstrs ++ store ++ instrs,
      max [stack]
    )
  end
    
(*......................................................................*)
(* Throw an exception						      	*)
(*......................................................................*)
| Throw(v, cty, message) =>
  let
    val canrethrow = case (v, #handler env) of (Var x, SOME { exnvar, ...}) => Var.eq(x, exnvar) | _ => false
  in
    if canrethrow 
    then (cty, $rethrow, 0)
    else
    let
      val (exnty, _, instrs, stack) = CompileValCmp.compileVal env v
    in
    (
      cty,
      (if Controls.get CompileException.exnLocs andalso message<>"" 
      then 
      fromList [ldc (Constants.STRING (UString.fromString message)),
       stsfld { name = RepNames.exnLocMessage, classty = globs (),
                  fldty = RTOps.string }] else Empty) ++
      instrs +$ throw,
      stack
    )
    end
  end

(*......................................................................*)
(* Tail function call 						      	*)
(*......................................................................*)
| App(v, vs) =>
  (case lookupBlock (#blocks env, v) of
    SOME (x, funty, isrec, arginstrs) =>
    let
      val SOME (_, cty) = MILTy.fromArrow funty
      val (tys, reps, instrs, stack) = CompileValCmp.compileVals env vs
      val goto = if isSome (#handler env) then leave else br
    in
      (cty, (if isrec 
             then GCSafePoint()
             else Empty) ++
             instrs ++ arginstrs +$ goto (RTLabels.varLabel x), stack)
    end

  | NONE =>
    case #handler env of
      NONE => compileNonContTail ()
    | SOME { blocks, ... } =>
      case lookupBlock (blocks,v) of
        SOME (x, funty, isrec, arginstrs) =>
        let
          val SOME (_, cty) = MILTy.fromArrow funty
          val (tys, reps, instrs, stack) = CompileValCmp.compileVals env vs
        in
            (cty,(if isrec 
                  then GCSafePoint()
                  else Empty) ++
                  instrs ++ arginstrs +$ br (RTLabels.varLabel x), stack)
        end

      | NONE =>
        compileNonContTail()
  )

(*......................................................................*)
(* Not a continuation-only term						*)
(*......................................................................*)
| _ =>
  compileNonContTail()
        
end

end (* of local open *)  
end
