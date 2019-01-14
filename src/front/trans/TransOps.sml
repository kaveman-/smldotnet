(*======================================================================*)
(* Types and helper functions for SML->MIL translation                  *)
(*======================================================================*)
structure TransOps :> TRANSOPS =
struct

(*----------------------------------------------------------------------*)
(* A value environment maps an SML source symbol to:                    *)
(*   (1) a bound typed MIL variable                                     *)
(*   (3) a list of MIL types to which the variable should be applied    *)
(*----------------------------------------------------------------------*)
type ValEnv = ((Var.Var * MILTy.Type) * MILTy.Type list) Symbol.Map.map

(*----------------------------------------------------------------------*)
(* An exception environment maps an SML dynamically generative          *)
(* exception to a variable that holds the tag                           *)
(*----------------------------------------------------------------------*)
type ExEnv = Var.Var TyName.Map.map

type StrEnv = (Var.Var * MILTy.Type) Symbol.Map.map

(*----------------------------------------------------------------------*)
(* Mostly we're dealing with symbol maps and sets                       *)
(*----------------------------------------------------------------------*)
structure Map = Symbol.Map
structure Set = Symbol.Set

(*----------------------------------------------------------------------*)
(* Merge two environments, the second overriding the first.             *)
(*----------------------------------------------------------------------*)
fun merge (E1,E2)  = Map.unionWith #2 (E1,E2)
fun mergeSE (SE1,SE2)  = Map.unionWith #2 (SE1,SE2)
fun mergeEE (EE1, EE2) = TyName.Map.unionWith #2 (EE1, EE2)

(*----------------------------------------------------------------------*)
(* Accumulated errors                                                   *)
(*----------------------------------------------------------------------*)
val errors = ref ([] : Error.Error list)
fun addError err = errors := err :: !errors

(*----------------------------------------------------------------------*)
(* Term and type variable supplies                                      *)
(*----------------------------------------------------------------------*)
val vs = ref Var.initial
val tvs = ref Var.initial

fun freshVar () = 
  let val (vs', v) = Var.fresh (!vs)
  in
    vs := vs'; v
  end

fun freshAnonVar () = 
  let val (vs', v) = Var.fresh (!vs)
  in
    vs := vs'; ((v,MILTermOps.getAnonVarName()),MILTerm.Var v)
  end


fun freshBoundVar sym = 
  let val (vs', v) = Var.fresh (!vs)
  in
    vs := vs'; ((v,[sym]), MILTerm.Var v)
  end

fun freshBoundVarLongid longid = 
  let val (vs', v) = Var.fresh (!vs)
  in
    vs := vs'; ((v,longid), MILTerm.Var v)
  end


val debug = Controls.add true "debug.propsym" 
val symFromLoc = Controls.add false "debug.symFromLoc" 

fun getPatVar pat =
    case pat of
      SMLTerm.PatVar(v,smlty) =>
      SOME v
    | SMLTerm.PatLayer((v,smlty), pat) => 
      SOME v
    | SMLTerm.PatRef pat => 
      getPatVar pat
    | SMLTerm.PatCast (v,smlty) => 
      SOME v
    | _ => NONE

fun mkFreshVarFns (strid, sourcemap) = 
   if Controls.enabled(debug) 
   then let val currentSourcemap = ref sourcemap
            fun readSource sourcemap = let val is = TextIO.openIn(SourceMap.fileName sourcemap) in TextIO.inputAll is before TextIO.closeIn(is) end
            val currentSource = ref (readSource(sourcemap))
            fun sourceFromLoc {left,right} = 
                let val maxlen = 50
                    val halflen = maxlen div 2
                    val left = left - 3
                    val right = right - 3
                    val len = right -left
                    val size = size(!currentSource)
                in
                    if left>0 andalso left < size andalso right > left andalso right <= size
                    then
                      String.toString(
                        if len <= maxlen
                        then
                             String.substring(!currentSource,left,Int.min(right-left,maxlen))                      
                        else String.substring(!currentSource,left,halflen) ^ "..."  ^
                             String.substring(!currentSource,right-halflen,halflen)
                      )
                    else ""
                end
            fun decodeLoc  {left,right} =
                let val sourcemap = !currentSourcemap
                    val dl = SourceMap.decode(sourcemap,left)
                    val dr = SourceMap.decode(sourcemap,right)
                in  {left=dl,
                     right=dr,
                     file=SourceMap.fileName sourcemap}
                end     
        in
           {(* the freshXXXVar functions actually ignore locations... *)
            (*@TODO: remove locOpt args and clean up callers *)
            freshAnonVar = if Controls.get(symFromLoc) 
                           then  fn locOpt => 
                                    case locOpt of
                                        NONE => freshAnonVar ()
                                      | SOME loc => 
                                            freshBoundVarLongid([Id.fromString (sourceFromLoc(loc))])
                           else fn _ => freshAnonVar(),
            freshBoundVar = fn (locopt,sym) => freshBoundVarLongid [strid,sym],
            letLine = fn loc  => fn (cmp,cty) =>
                       let val line = Ext.Line (decodeLoc loc)
                           val lineEff = ExtOps.effectOf line
                           val letCty = MILTy.cmpTypePlus(cty,lineEff) 
                       in  
                             (MILTerm.Let(MILTerm.Special((line,NONE,NONE),[],MILTy.cmp(lineEff,[])), ([],cmp)), letCty)
                       end,
             withSource = 
               fn sourcemap => fn f => fn x => 
               let val savedSourcemap = !currentSourcemap
                   val savedSource = !currentSource
		   val _ = currentSourcemap := sourcemap
		   val _ = currentSource := readSource (sourcemap)
                   val res = f x
               in 
                   currentSourcemap := savedSourcemap;
                   currentSource := savedSource;
                   res
               end
           }
        end
   else {freshAnonVar = fn _ =>  freshAnonVar (),
         freshBoundVar = fn (_,sym)  => freshBoundVarLongid [strid,sym],
         letLine = fn _ => fn (cmp,cty) => (cmp,cty),
         withSource = fn _ => fn f => f}

fun freshTyVar () =
  let
    val (tvs', v) = Var.fresh (!tvs)  
  in
    tvs := tvs'; v
  end

(*----------------------------------------------------------------------*)
(* Translate an ML type variable into a MIL kind                        *)
(* i.e. sort Any becomes kind Any                                       *)
(*      sort Eq  becomes kind Eq                                        *)
(*      all others should not be present                                *)
(*----------------------------------------------------------------------*)
fun tyVarKind tyvar = 
  case TyVar.sort tyvar of
    TyVar.Normal s => if TySort.<=(s, TySort.eq) then MILTy.Eq else MILTy.Any
  | _ => MILTy.Any

fun freshTyVars (VE, []) = (VE, [])
  | freshTyVars (VE, tyvar::tyvars) = 
    case TyVar.sort tyvar of
      TyVar.Overloaded _ => freshTyVars (VE, tyvars)
    | _ =>
      let
        val v' = freshTyVar ()
        val (VE', vs') = freshTyVars (VE, tyvars)
      in
        (TyVar.Map.insert(VE', tyvar, MILTy.tyvar v'), 
          (v',tyVarKind tyvar)::vs')
      end

(*----------------------------------------------------------------------*)
(* Translate type variables and extend the TV environment appropriately *)
(*----------------------------------------------------------------------*)
fun freshDebTyVars depth tyvars =
let 
  val (TVE, kinds) =
  ListOps.foldri 
    (fn (d, tyvar, (TVE, kinds)) =>
      let 
        val K = tyVarKind tyvar
      in
        (TyVar.Map.insert(TVE, tyvar, MILTy.deb (depth + d)), K::kinds)
      end) 
    (TyVar.Map.empty, [])
  tyvars
in
  (TVE, kinds)
end

fun throwMessage (sourcemap, entity : Entity.Ref, loc) =
  " at " ^ Id.toString (#2 entity) ^ ":" ^ 
  PrintLoc.location2string (sourcemap, loc)

fun initialize supply =
  (vs := supply; tvs := Var.initial; errors := [])

fun getSupply () = !vs
fun getTyVarSupply () = !tvs
fun getErrors () = !errors

end


