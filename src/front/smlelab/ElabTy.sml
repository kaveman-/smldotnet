(*======================================================================*)
(* Elaboration of type expressions					*)
(*======================================================================*)
structure ElabTy :> ELABTY =
struct

local 
  open Env EnvOps ElabState
in

val seqWithtype = Controls.add false "seqWithtype"

structure Map = Symbol.Map

(*----------------------------------------------------------------------*)
(* Convert a constructor environment into a suitable value environment	*)
(*----------------------------------------------------------------------*)
fun CEtoVE (CE as (tyvars, tyname, m)) =
let
  val ty = SMLTy.consType(map SMLTy.tyVarType tyvars, tyname)
  fun toValBind tyopt =
    ValBind.ConSch(
      SMLSch.TypeScheme(tyvars, 
        case tyopt of 
          NONE => ty
        | SOME ty' => SMLTy.funType(ty', ty)),
      CE)
in
  Map.map toValBind m
end

(*----------------------------------------------------------------------*)
(* Type Expressions (p27 Defn)						*)
(*----------------------------------------------------------------------*)
fun infTy C (loc,ty) = 
case ty of

(* Rule 44 *)
  Syntax.TyVar tyvar => 
  SMLTy.tyVarType (TyVar.explicit tyvar)

(* Rule 45 *)
| Syntax.TyRecord tyrow => 
  SMLTy.recType (infTyRow C tyrow) 

(* Rule 46 *)
| Syntax.TyCon(tyseq, longtycon) =>
  let val tys = map (infTy C) tyseq
  in
    case EnvLookup.lookupTyCon (EofC C, loc, longtycon) of
      SOME tystr =>
        if TyStr.arity tystr <> length tys
        then
          (error (Error.error (loc, 
               "type constructor " ^ Longid.toString longtycon ^ 
               " is applied to " ^ Int.toString (length tys) ^
               " arguments, wants " ^ Int.toString (TyStr.arity tystr)), []);
          freshMono ())
        else 
          TyStr.apply (tystr, tys)

    | NONE => 
      (* lookupTyCon has already reported an error *)
      freshMono ()
  end

(* Rule 47 *)
| Syntax.TyFun(tyexp, tyexp') => 
  SMLTy.funType (infTy C tyexp, infTy C tyexp')

(* Extra rule for the tuple type derived form *)
| Syntax.TyTuple [ty] => 
  Debug.fail "Elab.infTy: singleton tuple type"

| Syntax.TyTuple tyseq =>
  SMLTy.tupleType (map (infTy C) tyseq)

(* Rule 48 elided: parentheses *)

(*----------------------------------------------------------------------*)
(* Type-expression Rows (p27 Defn)					*)
(*----------------------------------------------------------------------*)
(* Rule 49 *)
and infTyRow C tyrowexp = 
  map (fn (lab,tyexp) => (lab, infTy C tyexp)) tyrowexp

(*----------------------------------------------------------------------*)
(* Type Bindings (p25 Defn)						*)
(*----------------------------------------------------------------------*)
(* Rule 27 *)
fun infTypBind C [] = 
    Map.empty

  | infTypBind C ((tyvarseq,tycon,typ)::typbind) =
    Map.insert (infTypBind C typbind, 
      tycon, 
      TyStr.makeConcrete (map TyVar.explicit tyvarseq, infTy C typ))

(*----------------------------------------------------------------------*)
(* Sequential type bindings						*)
(*----------------------------------------------------------------------*)
fun infTypBindSeq C [] = 
    Map.empty

  | infTypBindSeq C ((tyvarseq,tycon,typ)::typbind) =
    let
      val tystr = TyStr.makeConcrete (map TyVar.explicit tyvarseq, infTy C typ)
      val TE = Map.insert(Map.empty, tycon, tystr)
    in
      Map.insert(infTypBindSeq (CplusTE C TE) typbind, tycon, tystr)
    end

(*----------------------------------------------------------------------*)
(* Data Type Bindings (p25 Defn)					*)
(* We want to elaborate a declaration					*)
(*     datatype datbind <withtype typbind>				*)
(* under a context C.							*)
(*									*)
(* This is one of the most complicated bits of the elaborator! Things   *)
(* proceed as follows:							*)
(*     1. Generate (equality-admitting) tynames for each of the type    *)
(*        constructors in the datatype bindings. Construct a type       *)
(*        environment TE1 that maps the type constructors to these      *)
(*        (abstract) names. Collect the names into a set N.		*)
(*     2. Under the environment E+TE1, compute the type environment     *)
(*        specified by the (optional) withtype clause. Call it TE2.     *)
(*     3. Under E+TE1+TE2 compute constructor environments for each of  *)
(*        the datatype bindings and at the same time determine the	*)
(*        defn-use graph of the datatype.				*)
(*     4. Calculate the strongly-connected components of the defn-use   *)
(*        graph of the datatype bindings.				*)
(*     5. Use these to determine the equality status of each type name. *)
(*     6. Construct the final type environment binding datatype         *)
(*        constructors to concrete datatype tystrs; also construct a    *)
(*        value environment for the value constructors.                 *) 
(*----------------------------------------------------------------------*)
(* Rule 28 *)
and infDatBind insig C (datbind : Syntax.DatBind, typbindopt) =
let 
  val tynames = TyName.temp 
    (map (fn (_,tycon,_) => pathofC C @ [tycon]) datbind, TyName.Eq)

  (* Step 1. Construct a type environment TE1 for the datatype bindings *)
  (* To save time later, also attach the arity and type names to the *)
  (* conbinds, and collect the tynames in a set N. *)
  fun makeTE ([], []) = 
      (Map.empty, [])

    | makeTE ((tyvarseq,tycon,conbind)::datbind, tyname::tynames) =
      let val (TE, binds) = makeTE (datbind, tynames)
      in
        (Map.insert (TE, tycon, 
           TyStr.makeAbstract (map TyVar.explicit tyvarseq, tyname)),
          (map TyVar.explicit tyvarseq, tycon, tyname, conbind)::binds)
      end

  val (TE1, binds) = makeTE (datbind, tynames)
  val N = TyName.Set.addList(TyName.Set.empty, tynames)

  (* Rule 29: Constructor bindings (p25 of Defn) *)
  fun infConBind C [] = 
      (Map.empty, TyName.Set.empty)

    | infConBind C (((_,con), NONE)::conbind) =
      let val (CE, N) = infConBind C conbind
      in
        (Map.insert (CE, con, NONE), N)
      end

    | infConBind C (((_,con), SOME (typ as (loc,_)))::conbind) =
      let 
        val ty' = infTy C typ
	(* check for byrefs *)
        val _ = ElabCheck.checkType (loc,"constructor type contains a byref") ty'
        val (CE,N) = infConBind C conbind
      in
        (Map.insert (CE, con, SOME ty'), 
        TyName.Set.union(N, SMLTy.tynames ty'))
      end

  fun makeDE (C, N) binds =
      case binds of
        [] => 
        []

      | (tvs, tycon, tyname, conbind)::binds =>
        let 
          val (CE, N') = infConBind C conbind
          val uses = TyName.Set.listItems (TyName.Set.intersection(N, N'))  
          val graph = makeDE (C, N) binds
        in
          ((tycon, (tvs, tyname, CE)), uses)::graph
        end

  (* Step 2. Construct a type environment TE2 for the withtype bindings *)
  (* In accordance with the Revised Defn, these are simultaneous defns. *)
  (* SML/NJ adopts a more useful sequential semantics; us too?          *)
  val TE2 = case typbindopt of 
          NONE => Map.empty
        | SOME typbind => 
          (if Controls.get seqWithtype
          then infTypBindSeq else infTypBind) (CplusTE C TE1) typbind

  (* Step 3. Build constructor environments for each datatype binding. *)
  (* At the same time build the defn-use graph *)
  val graph = makeDE (CplusTE (CplusTE C TE1) TE2, N) binds

  (* Step 4. Calculate the strongly-connected components of the graph *)
  (* These make up the datatype environment stored in the state *)
  val DE = Dep.scc 
           (graph, fn ((_,(_,tyname,_)), tyname') => 
             TyName.eq(tyname, tyname'))  

  (* Step 5. Build up a type name environment with the correct sorts *)

  (* List the types involved in a strongly-connected component of
     datatype bindings *)
  fun typesInDef (tycon, (tvs, tyname, CE)) =
      Map.foldr (fn (NONE, tys) => tys | (SOME ty, tys) => ty::tys) [] CE
  fun typesInSCC (Dep.NonRec d) = typesInDef d
    | typesInSCC (Dep.Rec ds) = List.concat (List.map typesInDef ds)

  (* Calculate the maximal equality status for the datatype bindings *)
  (* Return as a `renaming' for the the type names in DE *)
  fun maximiseEq [] result = result
    | maximiseEq (scc::DE) (r,alltynames,resultDE,VE,TE1) =
      let
        fun convTySort s = if TySort.<=(s, TySort.eq) then TyName.Eq else TyName.NotEq
        val (r, scc', VE, TE1) =
        case scc of
          Dep.NonRec (d as (tycon, (tyvars,tyname,CE))) => 
          let 
            val sort = convTySort (SMLTy.sortList true (typesInDef d))
            val tyname' = freshTyName (pathofC C @ [tycon],sort)
            val r = TyName.Map.insert(r, tyname, tyname')
            val defs = (tyvars, tyname', 
              Map.map (Option.map (SMLTy.renameType r)) CE)
          in
            (r, [defs], VEplusVE VE (CEtoVE defs),
              Map.insert(TE1, tycon, TyStr.makeDatatype defs))
          end

          (* INCORRECT! Should iterate until a fixed point is reached *)
        | Dep.Rec defs => 
          let
            val sorts = 
              map (convTySort o SMLTy.sortList true o typesInDef) defs
            val tynames = 
              map (fn (_, (_, tyname, _)) => tyname) defs
            val longids = 
              map (fn (tycon, _) => pathofC C @ [tycon]) defs
            val tynames' = freshRecTyNames (ListPair.zip(longids, sorts))
            val r = ListPair.foldr 
                (fn (t, t', r) => TyName.Map.insert(r, t, t')) r 
                (tynames,tynames')
            val (defs, VE, TE1) =
              foldr 
              (fn ((tycon,(tyvars, tyname, CE)), (scc',VE,TE1)) => 
              let 
                val defs = 
                (
                  tyvars, 
                  TyName.rename r tyname, 
                  Map.map (Option.map (SMLTy.renameType r)) CE
                )
              in
                (defs::scc', VEplusVE VE (CEtoVE defs), 
                Map.insert(TE1, tycon, TyStr.makeDatatype defs))
              end) ([], VE, TE1) defs
          in
            (r, defs, VE, TE1)
          end    
        in
          maximiseEq DE (r, tynames@alltynames, scc'::resultDE,VE,TE1)
        end         

  val (r, alltynames, DE, VE, TE1) = 
    maximiseEq (rev DE) (TyName.Map.empty, [], [], Map.empty, Map.empty)

in
  (* Add to the pervasive datatype environment *)
  addDE DE;

  (VE, TEplusTE TE1 (renameTE r TE2), alltynames)
end

(*----------------------------------------------------------------------*)
(* New to SML'97: datatype tycon = datatype longtycon			*)
(*----------------------------------------------------------------------*)
fun infDatCopy C loc (tycon,longtycon) =
  case EnvLookup.lookupTyCon (EofC C, loc, longtycon) of
      SOME tystr =>
       (case TyStr.fromDatatype tystr of
         NONE =>
         (error(Error.error (loc, 
           "type constructor does not define a datatype: " ^ 
           Longid.toString longtycon), []); emptyE)

       | SOME defs =>
         VETEinE (CEtoVE defs, Map.insert(Map.empty, tycon, tystr)))

    | NONE => 
      (error(Error.error(loc, "type constructor undefined: "
          ^ Longid.toString longtycon), []); emptyE)


end (* of local open *)
end (* of struct *)

