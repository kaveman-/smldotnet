(*======================================================================*)
(* Elaboration of top-level structures, signatures and functors         *)
(*======================================================================*)
structure Elab :> ELAB =
struct

type Sig = 
  SMLTy.DatEnv * Env.Sig
type Str = 
  SMLTy.DatEnv * SMLTy.Realisation * Env.Env*SMLTerm.StrExp

structure T = SMLTerm

local open 
  Syntax ElabOps Env EnvOps SMLTy SMLPrimTy SMLSch SMLSchOps ElabTy ElabPat 
  ElabCore ElabState
in

val showElab = Controls.add false "showElab"
val showStructSharing = Controls.add false "showStructSharing"

structure Map = Symbol.Map

val tempstrid = Id.fromString "$temp"

fun addOpen(openedstructures,strexp as (loc,_)) = 
	if List.null openedstructures then strexp else
	(loc,Syntax.StrLet([(loc, Syntax.Open openedstructures)], strexp))	

fun funDerived (funid,funarg,siginfo,strexp as (loc,_)) =
let
  val strexp = 
      case siginfo of
        SigNone => strexp
      | SigAbstract sigexp => (loc,StrOpaque(strexp, sigexp))
      | SigConcrete sigexp => (loc,StrTransparent(strexp, sigexp))

  val triple = 
    case funarg of
      StructArg (strid,sigexp) => (strid,sigexp,strexp)
    | SpecArg spec => 
      (tempstrid,(loc,SigSpec spec),
        (loc,StrLet([(loc,Open [[tempstrid]])], strexp)))         
in
  triple
end

(*----------------------------------------------------------------------*)
(* Structure Expressions (p31 Defn)					*)
(*----------------------------------------------------------------------*)
fun infStrExp (topB, B : Basis) (loc, prestrexp) =
case prestrexp of
  
(* Rule 50 *)
  Struct strdec =>
  let
    val (d, E) = infStrDec (topB, B) strdec
  in
    (T.StrLet(d, 
    T.Struct (
      loc,
      Map.mapPartiali (fn (v,ValBind.VarSch _) => SOME v | _ =>NONE) (VEofE E),
      Map.mapi (fn (id, _) => T.Strid (id,[])) (SEofE E))), E)
  end

(* Rule 51 *)
| Strid longstrid =>
  let
    val (E, path) = EnvLookup.lookupStr (EofB B, loc, longstrid)
  in
    (T.Strid path, E)
  end

(* Rule 52 *)
| StrTransparent(strexp, sigexp) => 
  let
    val (e, E) = infStrExp (topB, B) strexp
    val sigma as (_, sigE) = infSigExp' (BnoPath B) sigexp
    val (psi, exmap) = Match.match1 loc (E, sigma)
    val E' = appRealisationE psi sigE
  in
(*    addRealisation psi; *)
    let
      val e' = Match.match2 (tempstrid,loc) (E, E')
    in
      (T.StrLet([T.Structure(tempstrid, e)], e'), renameE exmap E')
    end
  end
(* Rule 53 *)
| StrOpaque(strexp, sigexp) =>
  let
    val (e, E) = infStrExp (topB, B) strexp
    val sigma as (T, E') = infSigExp' (BnoPath B) sigexp
    val (r, T') = makeRenaming (pathofB B, T)
    val _ = appRenamingDE r
    val E' = renameE r E'
    val (psi, exmap) = Match.match1 loc (E, (T',E'))
    val E'' = appRealisationE psi E'
    val _  = if Controls.get showElab
             then Debug.print ("str:\n" ^ "E = " ^ EnvOps.EtoString E ^ 
               "\nrealised sig:\n" ^
               "E = " ^ EnvOps.EtoString E'' ^ "\n")
             else ()
  in
    addRealisation psi;
    let
      val e' = Match.match2 (tempstrid,loc) (E, E'')
      
    in
      (T.StrLet([T.Structure(tempstrid, e)], e'), renameE exmap E')
    end
  end
(* Rule 54 *)
(*@BUG: CRUSSO the FunApp case needs to be reviewed --- the textual expansion
   seems suspect since it can capture names (for instance, in the functor
   argument signature *)
 | FunApp(funid, strexp as (locStrExp,_)) =>
  let
    val E = infStrExp (topB, B) strexp
  in
  (*@TODO: it would be nicer if funid had its own location in the grammer;
     we currently reconstruct an approximate one for lookupFunId 
  *)
  case EnvLookup.lookupFunId(FofB B,{left= #left loc,right= #left locStrExp}, funid) of
    NONE => 
    (* error already reported in lookupFunId *)
    (T.Struct(loc,Map.empty, Map.empty), emptyE)
  | SOME (Phi, fileref, longids) =>    
    let
      val ParseManager.Success (topbind,sourcemap) = ParseManager.parse fileref
      val SOME topbind  = 
        SyntaxCheck.find (topbind, (Entity.Fun, funid))

      val (locFunBind,Syntax.Functor [a]) = topbind

      val (strid, sigexp as (locSigExp,_), strexp' as (locBody,_)) = funDerived a

      val (d, E1) = infStrDec (topB, B) 
        [(locStrExp, Structure [(tempstrid, strexp, SigNone)])]
      val bodyexp =  
	addOpen(longids,(locFunBind,StrLet([(locSigExp, Structure [(strid, (locSigExp,Strid [tempstrid]), SigConcrete sigexp)])],strexp')))
      val (e, E2) = infStrExp (topB, BplusE topB E1) bodyexp
    in
      (T.StrLet(d, T.StrInlined(sourcemap,e)), E2)
    end
  end

(* Rule 55 *)
| StrLet(strdec, strexp) =>
  let
    val (d, E1) = infStrDec (topB, B) strdec
    val (e, E2) = infStrExp (topB, BplusE B E1) strexp
  in
    (T.StrLet(d, e), E2)
  end

(*----------------------------------------------------------------------*)
(* Structure-level Declarations (p32 Defn)				*)
(*----------------------------------------------------------------------*)
and infStrDecItem (topB, B : Basis) (strdecitem as (loc, prestrdecitem)) =
case prestrdecitem of

(* Rule 57 *)
  Structure strbind =>
  let
    val (ds, SE) = infStrBind (topB, B) strbind
  in
    ([T.And ds], SEinE SE)
  end

(* Rule 58 *)
| Local(strdec1, strdec2) =>
  let
    val (d1, E1) = infStrDec (topB, B) strdec1
    val (d2, E2) = infStrDec (topB, BplusE B E1) strdec2
  in
    ([T.Local(d1, d2)], E2)
  end

(* Rule 56 *)
| _ =>
  infDecItem (CofB B) true strdecitem
  
and infStrDec (topB, B : Basis) dec =
case dec of

(* Rule 59 *)
  [] =>
  ([], emptyE)

(* Rule 60 *)
| strdecitem::strdec =>
  let
    val (d1, E1) = infStrDecItem (topB, B) strdecitem
    val (d2, E2) = infStrDec (topB, BplusE B E1) strdec
  in
    (d1 @ d2, EplusE E1 E2)
  end

(*----------------------------------------------------------------------*)
(* Structure Bindings (p32 Defn)					*)
(*----------------------------------------------------------------------*)
and infStrBind (topB, B : Basis) strbinds =
(* Rule 61 *)
case strbinds of
  [] =>
  ([], Map.empty)

| (strid,strexp as (loc,_),siginfo)::strbind =>
  let
    val strexp' = 
      case siginfo of
        SigNone => strexp
      | SigAbstract sigexp => (loc,StrOpaque(strexp, sigexp))
      | SigConcrete sigexp => (loc,StrTransparent(strexp, sigexp))
    val (e, E) = infStrExp (topB, BplusStr B strid) strexp'
    val (d, SE) = infStrBind (topB, B) strbind
  in
    (T.Structure(strid, e) :: d, Map.insert(SE, strid, E))
  end

(*----------------------------------------------------------------------*)
(* Functor Bindings (p36 Defn)						*)
(*----------------------------------------------------------------------*)
(* infFunBind returns a pair (F,es) of 
   - F, a functor environment 
   - es, the list of functor bodies to check for semantic restrictions 
    (ElabCheck.checkStrExp) after constraint solving 
*)
and infFunBind (topB, B : Basis) funbinds =
(* Rule 86 *)
case funbinds of
  [] =>
  (Map.empty,[])

| (funid,strid,sigexp,strexp)::funbind =>
  let
    val sigma as (T, E) = infSigExp' (BnoPath B) sigexp
    val stamp = getStamp ()
    val singletonSE = Map.insert(Map.empty, strid, E)
    val (e, E') = infStrExp (topB, BplusE B (SEinE singletonSE)) strexp
    val (F,es) = infFunBind (topB, B) funbind
    val T' = (TyName.Set.filter 
      (fn tn => not (TyName.earlier(tn, stamp))) (tynamesE E'))
    val Phi = (T, (E, (T',E')))
    val SOME (fileref, longids) = SourceManager.fileRefFor (getEntity ())
  in
    (Map.insert(F, funid, (Phi, fileref, longids)),e::es)
  end

(*----------------------------------------------------------------------*)
(* Signature Expressions (p32-33 Defn)					*)
(*----------------------------------------------------------------------*)
and infSigExp stamp (B : Basis) (loc,presigexp) =
case presigexp of
  
(* Rule 62 *)
  SigSpec spec =>
  infSpec stamp B spec

(* Rule 63: rename bound variables to satisfy side condition *)
| Sigid sigid =>
  (case EnvLookup.lookupSigId(GofB B, loc, sigid) of
    NONE => 
    (* error reported in lookupSigId *)
    emptyE
  | SOME (T,E) =>
    let
      val (r, T') = makeRenaming (pathofB B, T)
    in
      renameE r E
    end)

(* Rule 64 *)
| Where(sigexp, tyvarseq, longtycon, typ) =>
  let
    val E = infSigExp stamp B sigexp
    val ty = infTy (CofB B) typ
  in
    case EnvLookup.lookupTyCon (E, loc, longtycon) of
    NONE => 
    (error (Error.error(loc, 
      "type constructor not bound in signature: " ^ Longid.toString
      longtycon), []); emptyE)

  | SOME tystr =>
    if TyStr.arity tystr <> length tyvarseq
    then 
      (error (Error.error(loc, "type definition has wrong arity"),[]); emptyE) 
    else case TyStr.tyname tystr of
      NONE => 
      (error (Error.error(loc, 
      "type definition is not atomic: " ^ SMLTy.toString ty), []); emptyE)

    | SOME tyname =>
      let 
        (*@TODO: check that tyname and ty have same equality status *)
        val psi = TyName.Map.insert(TyName.Map.empty, tyname, 
          (map TyVar.explicit tyvarseq, ty))
        val E' = appRealisationE psi E
      in
        (*@TODO: check E' for well-formed-ness *)
        E'
      end
  end

and infSigExp' (B : Basis) sigexp =

  (* Rule 65 *)
  let
    val stamp = getStamp ()
    val E = infSigExp stamp B sigexp
  in
    (TyName.Set.filter 
    (fn tn => not (TyName.earlier(tn, stamp))) (tynamesE E), E)
  end

(*----------------------------------------------------------------------*)
(* Specifications (p33 Defn)						*)
(*----------------------------------------------------------------------*)
and infSpecItem stamp (B : Basis) ((loc,prespecitem) : SpecItem) = 
case prespecitem of

(* Rule 68 *)
  ValDesc valdesc => 
  let
    val VE = infValDesc loc (CofB B) valdesc
  in
    VEinE (Map.mapi (fn (v,ty) => ValBind.VarSch(polyType ty)) VE)
  end

(* Rule 69 *)
| TypeDesc typdesc =>
  TEinE (infTypeDesc (CofB B) typdesc)

(* Rule 70 *)
| EqTypeDesc typdesc =>
  TEinE (infEqTypeDesc (CofB B) typdesc)

(* Rule 71 *)
| DatatypeDesc (datdesc, typbindopt) => 
  let
    val (VE, TE, r) = infDatBind true (CofB B) (datdesc, typbindopt)
  in
    VETEinE (VE, TE)
  end

(* Rule 72 *)
| DatatypeDescCopy (tycon, longtycon) =>
  infDatCopy (CofB B) loc (tycon, longtycon)

(* Rule 73 *)
| ExceptionDesc exdesc =>
  let
    val EE = infExDesc (CofB B) loc exdesc
  in
    VEinE (Map.map ValBind.ExTy EE)
  end

(* Rule 73a: class declarations *)
(*@BUG: this case is incomplete *)
| ClassDesc (ClassTypeSpec 
  {tycon, modifiers, conty, inherits, methodspec }) =>
  let
    val C = CofB B

    (* Create a fresh type name for this class *)
    val tyname = freshClass (pathofC C @ [tycon])

    (* Also generate a tyvar type for it, to be unified with the defn later *)
    val classty = SMLTy.tyVarType (freshTyVar (TyVar.Normal TySort.class))

    (* Put it in a singleton type environment *)
    val TE = Map.insert(Map.empty, tycon, TyStr.makeConcrete([], classty))

    val C' = CplusTE C TE

    (* Type check the constructor type *)
    val conty = Option.map (infTy C') conty

    val VE = Map.insert(Map.empty, tycon, ValBind.Special (classty, NONE))

    (* Type the methods *)
    (*@TODO: method modifiers *)
    fun typeMeth (f, ty) =
    { flags = Symbol.Set.empty, name = f, ty = infTy C' ty }

    val methods = map typeMeth methodspec

    val interfaces = map (infTy C') inherits

    val superty = 
(*
    case super of
      NONE =>
*)
      SMLTy.baseType TyNames.objectTyName

(*
    | SOME tyexp =>
      infTy C' tyexp
*)

    val flags = Symbol.Set.addList(Symbol.Set.empty, modifiers)
    val mlclass =
    SMLTy.MLClass
    {
      tyname = tyname,
      flags = flags,
      super = superty,
      interfaces = interfaces,
      initargty = conty,
      methods = methods
    }

    val mlclassty = SMLTy.classType mlclass

    val _ = SMLTyUnify.unify ((SOME loc, "class type", classty),
                   (SOME loc, "class def", mlclassty))

    val TE = Map.insert(Map.empty, tycon, TyStr.makeClasstype mlclass)

  in
    VETEinE (VE,TE)
  end

(* Rule 74 *)
| StructureDesc strdesc =>
  SEinE (infStrDesc stamp B strdesc)

(* Rule 75 *)
| Include sigexp =>
  infSigExp stamp B sigexp

(* Rule 78 *)
| SharingType _ =>
  (Debug.fail "Elab:unexpected type sharing spec")
| Sharing _ =>
  (Debug.fail "Elab:unexpected structure sharing spec")

and lookupTyName E loc longtycon =
  case EnvLookup.lookupTyCon (E, loc, longtycon) of
    NONE => 
    (error (Error.error(loc, 
      "type constructor not bound in signature: " ^ Longid.toString
      longtycon), []); NONE)
  | SOME tystr =>
    case TyStr.fromDatatype tystr of
      SOME (tyvars, tyname, VE) =>
      SOME (tyvars, tyname)

    | NONE =>
      case TyStr.fromAbstract tystr of
        SOME (tyvars, tyname) =>
        SOME (tyvars, tyname)

      | NONE => 
        case TyStr.fromConcrete tystr of
          NONE =>
          (error (Error.error(loc, "type constructor not datatype or abstract: " 
          ^ Longid.toString longtycon), []); NONE)
        | SOME (tyvars, ty) =>
          case SMLTy.fromConsType ty of
            SOME (tys, tyname) => 
            (* is it eta-equivalent to a tyname *)
            if length tys = length tyvars
               andalso List.all SMLTy.eq (ListPair.zip(tys,List.map SMLTy.tyVarType tyvars))
            then SOME (tyvars, tyname)
            else (error (Error.error(loc, "type constructor not datatype or abstract: " 
                                          ^ Longid.toString longtycon), []); NONE)

          | NONE =>        
            (error (Error.error(loc, "type constructor not datatype or abstract: " 
          ^ Longid.toString longtycon), []); NONE)

and infSpec' stamp (B : Basis) Esofar (spec : Spec) =
case spec of
  [] => 
  Esofar
  (* type and structure sharing code adapted from Hamlet1.2 with kind permission from the author, Andreas Rossberg*)
  (* Rule 78 *)
| (loc,SharingType longids)::spec =>
  let
    val tyfuns = List.mapPartial (lookupTyName Esofar loc) longids
    (* partition into equality followed by nonequality tyfuns *)
    val (eqtyfuns,noneqtyfuns) = List.partition (fn (tyvars,tn) => TyName.equality tn = TyName.Eq) tyfuns
    val results = eqtyfuns@noneqtyfuns
  in
    (* are they all flexible? *)
    if List.exists(fn(tyvars,tn)=>TyName.earlier (tn,stamp)) results 
    then (error (Error.error(loc, "sharing type equation mentions free type names"), []);
          infSpec' stamp B Esofar spec)
    else
    case results of
      [] => 
      infSpec' stamp B Esofar spec

    | (tyvars,tn)::results' =>
      let val arity = length tyvars
      in
          (* do they all have the same arity? *)
          if List.all (fn (tyvars',_) => arity = List.length tyvars') results' then
              let
                  val psi = foldr (fn ((tvs, tn'), psi) => TyName.Map.insert(psi, tn', (tvs, SMLTy.consType(map SMLTy.tyVarType tvs, tn))))
                                  TyName.Map.empty results'
                  val E' = appRealisationE psi Esofar
              in
                  infSpec' stamp B E' spec
              end
          else (error (Error.error(loc, "types in sharing type spec have inconsistent arities"), []);
                infSpec' stamp B Esofar spec)
      end
  end
  (* Derived rule *)
| (loc,Sharing longids)::spec =>
	let
            fun fromFlexibleTyName tystr = 
                case TyStr.tyname tystr of 
                    SOME tn => 
                     if TyName.earlier (tn,stamp) 
                     then NONE
                     else SOME (TyStr.tyvars tystr,tn)
                  | NONE => NONE

	    fun shareFlexibleTyName(tycon,tyfun1,tyfun2, phi) =
		let
                    (* re-order tyfuns by equality *)
                    val ((tyvars1,t1),(tyvars2,t2)) = 
                        case (TyName.equality (#2 tyfun1), TyName.equality (#2 tyfun2)) of
                             (TyName.Eq,_) => (tyfun1,tyfun2)
                           | (_,TyName.Eq) => (tyfun2,tyfun1)
                           | (_,_) => (tyfun1,tyfun2)
                    val tyfun = (tyvars2, SMLTy.consType(map SMLTy.tyVarType tyvars2, t1))
                    val phi'' =  TyName.Map.insert(phi,t2,tyfun)
                    val phi' = TyName.Map.map (fn (tyvars,ty) => (tyvars,SMLTy.appRealisation phi'' ty)) phi
		in
                     TyName.Map.insert(phi',t2,tyfun)
		end

	    fun shareTE(TE1, TE2, phi) =
		Symbol.Map.foldli
		    (fn(tycon, tystr1, phi) =>
			case Symbol.Map.find(TE2, tycon)
			  of NONE => phi
			   | SOME tystr2 =>
                             let val (tystr1,tystr2) = (TyStr.appRealisation phi tystr1,TyStr.appRealisation phi tystr2)
                             in
                                 case (fromFlexibleTyName tystr1,fromFlexibleTyName tystr2)
                                  of (SOME tyfun1, SOME  tyfun2) =>
                                     if TyStr.arity tystr1 = TyStr.arity tystr2  
                                     then shareFlexibleTyName(tycon,tyfun1,tyfun2, phi)
                                     else (error (Error.error(loc, "types in sharing spec have inconsistent arities for constructor"^Id.toString tycon), []);
                                           phi)
                                   | (NONE,NONE) => 
                                     (* if they're both non-flexible, skip *)
                                     (* NB: this is a departure from the Definition, which says you should reject, but NJ seems to accept it.*)
                                     phi
                                   | _ => (* if one is flexible, but the other not, report an error *)
                                     (error(Error.error(loc, "structure contains both flexible and non-flexible type " ^Id.toString tycon),[]); 
                                      phi)
                             end)
		    phi TE1

	    fun shareSE(SE1, SE2, phi) =
		Symbol.Map.foldli
		    (fn(strid, E1, phi) =>
			case Symbol.Map.find(SE2, strid)
			  of NONE    => phi
			   | SOME E2 => shareE(E1, E2, phi)
		    )
		    phi SE1

	    and shareE(Env(SE1,TE1,VE1), Env(SE2,TE2,VE2), phi) =
		let
		    val phi'  = shareTE(TE1, TE2, phi)
		    val phi'' = shareSE(SE1, SE2, phi')
		in
		    phi''
		end

	    fun share1(E1,   [],   phi) = phi
	      | share1(E1, E2::Es, phi) =
		let
		    val phi' = shareE(E1, E2, phi)
		in
		    share1(E1, Es, phi')
		end

	    fun shareAll( [],   phi) = phi
	      | shareAll(E::Es, phi) =
		let
		    val phi' = share1(E, Es, phi)
		in
		    shareAll(Es, phi')
		end

	    val Es  = map (fn longid => #1 (EnvLookup.lookupStr (Esofar, loc, longid))) longids
	    val phi = shareAll(Es, TyName.Map.empty)
	in
            infSpec' stamp B (appRealisationE phi Esofar) spec
	end
  (* Rule 77 *)
| onespec::spec =>
  let
    val E1 = infSpecItem stamp (BplusE B Esofar) onespec
  in
    infSpec' stamp B (EplusE Esofar E1) spec
  end

and infSpec stamp (B : Basis) (spec : Spec) = infSpec' stamp B emptyE spec
 
(*----------------------------------------------------------------------*)
(* Value Descriptions (p34 Defn)					*)
(* Extended to deal with overloading.                                   *)
(*----------------------------------------------------------------------*)
(* Rule 79 *)
and infValDesc loc C valdesc =
let
  fun infValDesc' [] = 
      Map.empty

    | infValDesc' ((var,tyexp)::valdesc) =
      let
        val ty = infTy C tyexp
        val VE = infValDesc' valdesc
      in
        Map.insert(VE, var, ty)
      end

  val var = #1 (hd valdesc)
in
  if length valdesc > 1
  andalso List.all (fn (var',_) => Symbol.equal(var,var')) valdesc
  then
  let
    val tys = map (infTy C o #2) valdesc
    val SOME (S, ty) = SMLTyUnify.antiunifylist tys
  in
    case TyVar.Map.listItemsi S of
      [(tyvar, basetys)] =>
      let
        val tynameset =
            foldl 
	      (fn (basety, tynameset) =>
	       case SMLTy.fromConsType basety of
		   SOME([],tyname) => 
		     TyName.Set.add(tynameset, tyname) 
		 | _ => 
		     (error (Error.error(loc,"identifier overloaded at a type \                                                                                            \distinct from some type name: " ^ 
					 (SMLTy.toString basety)),[]);
		      tynameset))
	      TyName.Set.empty 
	      basetys
        val tyvar' = freshTyVar (TyVar.Overloaded tynameset)
        val ty = SMLTy.appSubst [(tyvar, SMLTy.tyVarType tyvar')] ty
      in
        Map.insert(Map.empty, var, ty)
      end

    | _ =>
      (error (Error.error 
	      (loc, 
	       "expected just one type variable in overloaded type"), 
	      []);
      Map.empty)
  end

  else infValDesc' valdesc
end
  

(*----------------------------------------------------------------------*)
(* Type Descriptions (p35 Defn)						*)
(*----------------------------------------------------------------------*)
(* Rule 80 *)
and infTypeDesc C [] = 
    Map.empty

  | infTypeDesc C ((tyvarseq,tycon,NONE)::typdesc) =
    let
      val TE = infTypeDesc C typdesc
      val tyname = freshTyName (pathofC C @ [tycon], TyName.NotEq)
    in
      Map.insert(TE, tycon, 
        TyStr.makeAbstract (map TyVar.explicit tyvarseq, tyname))
    end

  | infTypeDesc C ((tyvarseq,tycon,SOME typ)::typdesc) =
    let
      val ty = infTy C typ
      val TE = infTypeDesc C typdesc
    in
      Map.insert (TE, tycon, 
        TyStr.makeConcrete (map TyVar.explicit tyvarseq, ty))
    end

(*----------------------------------------------------------------------*)
(* Equality type Descriptions (p35 Defn)				*)
(*----------------------------------------------------------------------*)
(* Rule 80 *)
and infEqTypeDesc C [] = 
    Map.empty

  | infEqTypeDesc C ((tyvarseq,tycon)::typdesc) =
    let
       val TE = infEqTypeDesc C typdesc
       val tyname = freshTyName (pathofC C @ [tycon], TyName.Eq)
    in
      Map.insert(TE, tycon, 
        TyStr.makeAbstract(map TyVar.explicit tyvarseq, tyname))
    end

(* Rule 81/82: we reuse infDatBind *)

(*----------------------------------------------------------------------*)
(* Exception Descriptions (p35 Defn)					*)
(*----------------------------------------------------------------------*)
(* Rule 83 *)
and infExDesc C loc [] = 
    Map.empty

  | infExDesc C loc (((_,excon), tyopt)::exdesc) =
    let
      val EE = infExDesc C loc exdesc
    in
      case tyopt of
        NONE =>
        let
          val exname = freshTyName (pathofC C @ [excon], TyName.NotEq)
        in
          Map.insert (EE, excon, (exnType, exname))
        end

      | SOME typ =>
        let
          val ty = infTy C typ
          val exname = freshTyName (pathofC C @ [excon], TyName.NotEq)
        in
          Map.insert (EE, excon, 
            (SMLTy.funType (ty, exnType), exname))
        end
    end

(*----------------------------------------------------------------------*)
(* Structure Descriptions (p35 Defn)					*)
(*----------------------------------------------------------------------*)
(* Rule 84 *)
and infStrDesc stamp B strdesc =
case strdesc of
  [] =>
  Map.empty

| (strid,sigexp)::strdesc =>
  let 
    val E = infSigExp stamp B sigexp
    val SE = infStrDesc stamp B strdesc
  in
    Map.insert(SE, strid, E)
  end

fun BplusOpened(B,loc,strids) = 
  let val E' = List.foldl (fn (strid,E) => 
                           EplusE E (#1 (EnvLookup.lookupStr (EofB B, loc, strid))))
                           emptyE
	                   strids
  in
    BplusE B E'
  end

(*----------------------------------------------------------------------*)
(* Top-level signature							*)
(*----------------------------------------------------------------------*)
fun infTopSigExp B openedstructures (sigid,sigexp as (loc,_)) =
let
  val B = BplusOpened(B,loc,openedstructures)
  val (sigma, errors, DE, _) = 
    runelab (Entity.Sig, sigid) (fn () => infSigExp' B sigexp)
in
  if Controls.get showElab (* andalso List.all (not o Error.isSerious) errors *)
  then Debug.print ("results:\n" ^
    "E = " ^ EnvOps.EtoString (#2 sigma) ^ "\n")
  else (); 
  ((DE, sigma), errors)
end


(*----------------------------------------------------------------------*)
(* Top-level structure							*)
(*----------------------------------------------------------------------*)
fun infTopStrExp B openedstructures (strid,strexp as (loc,_),siginfo) =
let

  val strexp' = 
      case siginfo of
        SigNone => strexp
      | SigAbstract sigexp => (loc,StrOpaque(strexp, sigexp))
      | SigConcrete sigexp => (loc,StrTransparent(strexp, sigexp))

  val strexp' = addOpen(openedstructures,strexp')

  val ((e,E), errors, DE, psi) = 
    runelab (Entity.Str, strid) 
    (fn () => 
     let val _ = TyConstraint.init ()
         val result as (e,_) = infStrExp (B, BplusStr B strid) strexp'
     in if noSeriousErrors()
	then (TyConstraint.solve ();
	      if noSeriousErrors()
		  then ElabCheck.checkStrExp e
	      else ())
	else ();
	result 
     end)

in
  if Controls.get showElab (* andalso List.all (not o Error.isSerious) errors *)
  then Debug.print ("results:\n" ^
    "E = " ^ EnvOps.EtoString E ^ "\n" ^
    "DE = " ^ SMLTy.DEtoString DE ^ "\n" ^
    "psi = " ^ SMLTy.realisationToString psi ^ "\n")
  else ();
  ((DE, psi, E, e), errors)
end

(*----------------------------------------------------------------------*)
(* Top-level functor							*)
(*----------------------------------------------------------------------*)
fun infTopFunExp B openedstructures (arg as (funid,funarg,siginfo,strexp as (loc,_))) =
let
  val B = BplusOpened(B,loc,openedstructures) (* loc is bogus *)
  val (strid,sigexp,strexp) = funDerived arg
  val (F, errors, DE, psi) = 
    runelab (Entity.Fun, funid) 
    (fn () => 
         let val _ = TyConstraint.init ()
         val (F,es) = infFunBind (B, BplusStr B funid) [(funid,strid,sigexp,strexp)]
     in if noSeriousErrors()
	then (TyConstraint.solve ();
	      if noSeriousErrors()
		  then List.app ElabCheck.checkStrExp es
	      else ())
	else ();
	F 
     end)
in
  (valOf(Map.find(F, funid)), errors)
end


end (* of local open *)
end (* of struct *)



