(*======================================================================*)
(* Unification, anti-unification, matching, tyvar resolution.		*)
(*======================================================================*)
structure SMLTyUnify :> SMLTYUNIFY =
struct

local 
  val unifyCount = ref 0
  val showUnify = Controls.add false "elab.showUnify"
  val defaultsWarning = Controls.add false "defaultsWarning"
  open SMLTy ElabState
in

fun tyNameSort tn = 
  TySort.glb (if TyName.isClass tn then TySort.class else TySort.any,
  if TyName.equality tn = TyName.Eq then TySort.glb(TySort.eq,TySort.mono) else TySort.mono)

(*----------------------------------------------------------------------*)
(* Force type ty to have a sort s' such that s' <= s			*)
(*----------------------------------------------------------------------*)
fun forceSort sortError s ty =
    case proj ty of
      Var (r as ref (TyVar tyvar)) =>
      (case TyVar.sort tyvar of
        TyVar.Normal s' => 
        if TySort.<=(s', s) then ()
        else r := TyVar (freshTyVar (TyVar.Normal (TySort.glb (s, s'))))

      | TyVar.Overloaded tynames =>
        let
          val tynames' = 
            TyName.Set.filter (fn tyname => TySort.<= (tyNameSort tyname, s))
            tynames
        in
          if TyName.Set.numItems tynames = TyName.Set.numItems tynames'
          then ()
          else if TyName.Set.isEmpty tynames'
          then sortError s
          else r := TyVar (freshTyVar (TyVar.Overloaded tynames'))
        end
      )

      (* This doesn't seem quite right! *)
    | Con(tyname, tys) =>
      if TySort.<= (tyNameSort tyname, s)
      then app (forceSort sortError s) tys
      else sortError s

    | Rec (ref r) =>
      forceSortRow sortError s r
(*
    | (Ref ty | Array ty) => 
      if TySort.<=(s, TySort.eq) 
      then ()	
      else forceSort sortError s ty
*)
(*@TODO: review *)
    | Ref (ty1,ty2) => 
      let val s2 = TySort.lub(s,TySort.glb(TySort.mono,TySort.class)) 
	  (* our choice of s2 means that the equality attributes of the kind, ty2, of
	     the ref are irrelevant --- all kinds of ref should admit equality *)
      in
	  if TySort.<=(s, TySort.eq)
	      then forceSort sortError s2 ty2 
	  else (forceSort sortError s ty1;forceSort sortError s2 ty2)
      end
    | Array ty => 
      if TySort.<=(s, TySort.eq) 
      then ()	
      else forceSort sortError s ty

    | Fun(ty1, ty2) =>
      if TySort.<=(s, TySort.eq) 
      then sortError s
      else (forceSort sortError s ty1; forceSort sortError s ty2)

    | Class _ =>
      if TySort.<=(s, TySort.eq)
      then sortError s
      else ()

    and forceSortRow sortError s (fixed, varopt) =
        (app (forceSort sortError s) (Symbol.Map.listItems fixed);
        case varopt of
          NONE => ()
        | SOME (r as ref (RowVar rowvar)) => 
          let
            fun force () = r := RowVar (freshTyVar (TyVar.Normal s))
          in
            case TyVar.sort rowvar of
              TyVar.Normal s' => 
              if TySort.<=(s', s) then () else force ()

            | _ => 
              force ()
          end
        )

(*----------------------------------------------------------------------*)
(* Unify two types imperatively and return the unified type.		*)
(* A type name environment is required so that sorts can be determined. *)
(* The location is used for error reporting.				*)
(*----------------------------------------------------------------------*)
fun unify ((loc1,tag1,ty1),(loc2,tag2,ty2)) = 
let
  (* Dots are printed during type inference *)
  val _ = 
    (unifyCount := !unifyCount + 1;
     if !unifyCount mod 200 = 0 then PrintManager.print "." else ())

  (* Optionally dump the arguments to unify *)
  val _ = 
    if Controls.get showUnify
    then Debug.print ("\nUnify " ^ toString ty1 ^ " with " ^ toString ty2)
    else ()

  fun unify' (inty1, inty2) =
  let 
    (* Type constructors don't match *)
    fun unifyError () = error (Error.error (valOf loc1, "type mismatch"), 
        [(tag1,ty1),(tag2,ty2),("clash between",inty1),("and",inty2)])

    (* Occurs check has failed *)
    fun circError () = error (Error.error (valOf loc1, "type circularity"), 
        [(tag1,ty1),(tag2,ty2),("circularity between",inty1),("and",inty2)])

    (* Sort doesn't fit *)
    fun sortError s = error 
      (Error.error (valOf loc1, 
        "type does not have sort " ^ TySort.toString s), [(tag1,inty1)])

    fun unifyRow ((fixed1, variable1), (fixed2, variable2)) =
        let

          (* Record shapes don't match *)
	  fun unifyRowError (desc,lab,fldty) = 
	      error (Error.error (valOf loc1, "type mismatch"), 
		     [(tag1,ty1),(tag2,ty2),("record type",inty1),("disagrees with",inty2),
		      (desc ^ Id.toString lab,fldty)])
	      

          fun notTop s =
              not (TySort.<=(TySort.any,s) andalso TySort.<=(s,TySort.any))

          fun unifyFixed (result1, result2) ([], []) = 
              (case (variable1, variable2) of
                (NONE,NONE) => 
                ()

(* SL: or *)
(*
              | ((SOME (r as ref(RowVar rowvar)), NONE) |
                 (NONE, SOME (r as ref(RowVar rowvar)))) => 
                let
                  val TyVar.Normal s = TyVar.sort rowvar
                in
                  if notTop s 
                  then app (forceSort sortError s) 
                    (Symbol.Map.listItems result1)
                  else ();
                  r := Row (result1, NONE)
                end
*)
              | (SOME (r as ref(RowVar rowvar)), NONE) => 
                let
                  val TyVar.Normal s = TyVar.sort rowvar
                in
                  if notTop s 
                  then app (forceSort sortError s) 
                    (Symbol.Map.listItems result1)
                  else ();
                  r := Row (result1, NONE)
                end
	      | (NONE, SOME (r as ref(RowVar rowvar))) => 
                let
                  val TyVar.Normal s = TyVar.sort rowvar
                in
                  if notTop s 
                  then app (forceSort sortError s) 
                    (Symbol.Map.listItems result1)
                  else ();
                  r := Row (result1, NONE)
                end

              | (SOME (r1 as ref(RowVar rowvar1)), 
                 SOME (r2 as ref(RowVar rowvar2))) =>
                let
                  val TyVar.Normal s1 = TyVar.sort rowvar1
                  val TyVar.Normal s2 = TyVar.sort rowvar2
                  val s = TySort.glb (s1,s2)
                in
                  if notTop s
                  then app (forceSort sortError s) 
                    (Symbol.Map.listItems result1 @ 
                     Symbol.Map.listItems result2)
                  else ();
                  let
                    val rowvar = freshTyVar (TyVar.Normal s)
                    val r = ref (RowVar rowvar)
                  in 
                    r1 := Row(result1, SOME r); 
                    r2 := Row(result2, SOME r)
                  end
                end
              )

            | unifyFixed (result1, result2) ((lab1,ty1)::fixed1, []) =
              if isSome variable2 
              then unifyFixed (result1,Symbol.Map.insert(result2,lab1,ty1)) 
                              (fixed1, [])
              else unifyRowError ("missing field: ",lab1,ty1)

            | unifyFixed (result1, result2) ([], (lab2,ty2)::fixed2) =
              if isSome variable1
              then unifyFixed (Symbol.Map.insert(result1, lab2, ty2),result2) 
			      ([], fixed2)
              else unifyRowError ("extra field: ",lab2,ty2)

            | unifyFixed (result1, result2) 
                         ((lab1,ty1)::fixed1, (lab2,ty2)::fixed2) =
              case Symbol.Key.compare (lab1, lab2) of
                EQUAL =>
                (unify' (ty1, ty2); 
                 unifyFixed (result1, result2) (fixed1, fixed2))

              | LESS =>
                if isSome variable2
                then unifyFixed (result1,Symbol.Map.insert(result2, lab1, ty1)) 
                                (fixed1, (lab2,ty2)::fixed2)
                else unifyRowError ("missing field: ",lab1,ty1)

              | GREATER =>
                if isSome variable1
                then unifyFixed (Symbol.Map.insert(result1, lab2, ty2), result2)
                                ((lab1,ty1)::fixed1, fixed2)
                else unifyRowError ("extra field: ",lab2,ty2)
        in
          unifyFixed (fixed1, fixed2) 
                     (Symbol.Map.listItemsi fixed1, 
                      Symbol.Map.listItemsi fixed2)
        end       
    
    val (ty1, ty2) = (proj inty1, proj inty2) 
	
  in 
    case (ty1, ty2) of
    (Var (r1 as ref (TyVar tyvar1)), Var (r2 as ref (TyVar tyvar2))) =>
    if r1 = r2 then ()
    else
      (case (TyVar.sort tyvar1, TyVar.sort tyvar2) of
        (TyVar.Normal s1, TyVar.Normal s2) =>
        if TySort.<= (s1, s2) then r2 := Type inty1
        else
        if TySort.<= (s2, s1) then r1 := Type inty2
        else 
          let 
            val ty = tyVarType (freshTyVar (TyVar.Normal (TySort.glb (s1, s2))))
          in
            r1 := Type ty; r2 := Type ty
          end

      | (TyVar.Overloaded tynames1, TyVar.Overloaded tynames2) =>
        if TyName.Set.isSubset (tynames1, tynames2) then r2 := Type inty1
        else
        if TyName.Set.isSubset (tynames2, tynames1) then r1 := Type inty2
        else
        let
          val tynames = TyName.Set.intersection (tynames1, tynames2)
        in
          if TyName.Set.isEmpty tynames
          then unifyError ()
          else 
            let 
              val ty = tyVarType (freshTyVar (TyVar.Overloaded tynames))
            in 
              r1 := Type ty; r2 := Type ty
            end
        end

      | (TyVar.Normal s, TyVar.Overloaded tynames) =>
        let
          val tynames' = 
            TyName.Set.filter (fn tyname => TySort.<= (tyNameSort tyname, s))
            tynames
        in
          if TyName.Set.numItems tynames = TyName.Set.numItems tynames'
          then r1 := Type inty2
          else if TyName.Set.isEmpty tynames'
          then unifyError ()
          else 
          let
            val ty = tyVarType (freshTyVar (TyVar.Overloaded tynames'))
          in
            r1 := Type ty; r2 := Type ty
          end
        end

      | (TyVar.Overloaded tynames, TyVar.Normal s) =>
        let
          val tynames' = 
            TyName.Set.filter (fn tyname => TySort.<= (tyNameSort tyname, s))
            tynames
        in
          if TyName.Set.numItems tynames = TyName.Set.numItems tynames'
          then r2 := Type inty1
          else if TyName.Set.isEmpty tynames'
          then unifyError ()
          else 
          let
            val ty = tyVarType (freshTyVar (TyVar.Overloaded tynames'))
          in
            r1 := Type ty; r2 := Type ty
          end
        end)

(* SL: or *)
(*
  | ((Var(r as ref(TyVar tyvar)), ty2) | (ty2, Var(r as ref(TyVar tyvar)))) =>
    if occurs r (inj ty2)
    then circError ()
    else 
    (case TyVar.sort tyvar of
      TyVar.Normal s =>
      if TySort.<= (sort false (inj ty2), s)
      then r := Type (inj ty2)
      else (forceSort sortError s (inj ty2); r := Type (inj ty2))
{*
      if not (TySort.<=(s, TySort.eq)) then unifyError ()
      else 
        (forceEquality ty2; r := Type ty2)
*}
    | TyVar.Overloaded tynames =>
      case ty2 of
        Con(tyname, []) =>
        if TyName.Set.member(tynames, tyname)
        then r := Type (inj ty2)
        else unifyError ()

      | _ => unifyError ()
    )
*)
  | (Var(r as ref(TyVar tyvar)), ty2) =>
    if occurs r (inj ty2)
    then circError ()
    else 
    (case TyVar.sort tyvar of
      TyVar.Normal s =>
      if TySort.<= (sort false (inj ty2), s)
      then r := Type (inj ty2)
      else (forceSort sortError s (inj ty2); r := Type (inj ty2))
(*
      if not (TySort.<=(s, TySort.eq)) then unifyError ()
      else 
        (forceEquality ty2; r := Type ty2)
*)
    | TyVar.Overloaded tynames =>
      case ty2 of
        Con(tyname, []) =>
        if TyName.Set.member(tynames, tyname)
        then r := Type (inj ty2)
        else unifyError ()

      | _ => unifyError ()
    )
  | (ty2, Var(r as ref(TyVar tyvar))) =>
    if occurs r (inj ty2)
    then circError ()
    else 
    (case TyVar.sort tyvar of
      TyVar.Normal s =>
      if TySort.<= (sort false (inj ty2), s)
      then r := Type (inj ty2)
      else (forceSort sortError s (inj ty2); r := Type (inj ty2))
(*
      if not (TySort.<=(s, TySort.eq)) then unifyError ()
      else 
        (forceEquality ty2; r := Type ty2)
*)
    | TyVar.Overloaded tynames =>
      case ty2 of
        Con(tyname, []) =>
        if TyName.Set.member(tynames, tyname)
        then r := Type (inj ty2)
        else unifyError ()

      | _ => unifyError ()
    )

  | (Con(tyname1, tys1), Con(tyname2, tys2)) =>
    if TyName.eq(tyname1, tyname2)
    then app unify' (ListPair.zip (tys1, tys2))
    else unifyError ()

  | (Rec (r1 as ref row1), Rec (r2 as ref row2)) => 
    if r1=r2 then ()
    else unifyRow (row1, row2)

  | (Ref(ty1, ty2), Ref(ty3, ty4)) =>
    (unify' (ty1,ty3); unify' (ty2,ty4))

  
  | (Array ty1, Array ty2) => unify' (ty1, ty2)

  | (Fun(ty1, ty2), Fun(ty3, ty4)) =>
    (unify' (ty1,ty3); unify' (ty2,ty4))

  | (Class(MLClass {tyname = t1, ... }), Class(MLClass {tyname = t2, ... })) =>
    if TyName.eq(t1, t2) then ()
    else unifyError ()

  | _ => 
    unifyError ()

  end
in
  unify' (ty1, ty2); 

  (* Optionally dump the result of unification *)
  if Controls.get showUnify
  then Debug.print (" = " ^ toString ty1)
  else ();

  ty1
end

(*----------------------------------------------------------------------*)
(*   Match two types against each other: given bool update, list of     *)
(*   flexible tyvars flexTyVars,                                        *)
(*   rigid tyvars rigidTyVars, and                                      *)
(*   two types ty1 and ty2                                              *)
(*   return a substitution S such that (S U S') (ty1) = ty2, for some S'*)
(*          where dom(S)<=tyvars where                                  *)
(*                intersect(flexTyVars,dom(S')) = 0,                    *)
(*                intersect(rigidTyVars,rng(S')) = 0)                   *)
(*   If update is set then destructively apply the matching             *)
(*   substitution S.  Always destructively unify free type variables    *)
(*   in dom(S')(these are typically generated from interop code         *)
(*   or non-gen type  variables.                                        *) 
(*   This code relies on a number of assumptions,                       *)
(*   including that every free variable in ty2 is rigid                 *)
(*   (i.e. the scheme in the signature is closed)                       *)
(*   and that the free variables of ty1 and ty2 are distinct.           *)
(*----------------------------------------------------------------------*)

(*@TODO: This code should be replaced by a call to unify, once           *)
(*       we modify unify to take account of tyvar and tyname scopes.     *)
(*@TODO: update seems unnecessary (always false in callers)             *)
(*@TODO: this code should perhaps be in SMLSchOps                       *)
exception SortError
exception ScopeError
fun match update flexTyVars rigidTyVars (ty1,ty2) = 
let 
  fun isFree tyvar = 
      not(List.exists (fn tyvar' => TyVar.eq (tyvar,tyvar')) flexTyVars)
  fun noEscape ty = 
      let val tyvars = (TyVar.Set.listItems (tyvars ty)) 
      in
	  List.all (fn tyvar => 
		    not(List.exists (fn tyvar' => TyVar.eq (tyvar,tyvar'))
		                    rigidTyVars))
	  tyvars 
      end
  fun matchList S ([], []) = SOME S

    | matchList S (ty1::tys1, ty2::tys2) = 
      (case matchList S (tys1, tys2) of
        NONE => NONE
      | SOME S1 => matchTy S1 (ty1, ty2))

    | matchList S _ = Debug.fail "SMLTyUnify.matchList: unequal lengths"

  and matchRow S ([], []) = SOME S

    | matchRow S ((lab1,ty1)::row1, (lab2,ty2)::row2) = 
      if Symbol.equal(lab1,lab2)
      then case matchRow S (row1, row2) of
        NONE => NONE
      | SOME S1 => matchTy S1 (ty1, ty2)
      else NONE

   | matchRow S _ = NONE

  and matchTy S (ty1, ty2) =
  case (proj ty1, proj ty2) of

    (Var (tyref as ref (TyVar tyvar)), _) =>
    (case TyVar.Map.find(S, tyvar) of
      SOME ty' => if eq(ty', ty2) then SOME S else NONE
    | NONE => 
      (case TyVar.sort tyvar of
        TyVar.Overloaded tynames => 
        (case proj ty2 of
          Con(tyname, []) =>
          if TyName.Set.member(tynames, tyname)
          then 
          (if update orelse isFree tyvar then tyref := Type ty2 else ();
           SOME (TyVar.Map.insert(S, tyvar, ty2)))
          else NONE
         | _ => NONE)

       | TyVar.Normal s => 
	  let val isfree = isFree tyvar 
	  in
	   (if (not isfree) orelse noEscape ty2 then ()
	    else raise ScopeError; 
	   (* NB: we check noEscape first, because forcesort
	      can alter the free vars of ty2 *)
	    if TySort.<=(sort false ty2, s) then ()
	    else forceSort (fn _ => raise SortError) s ty2;
	    if update orelse isfree then (* do the physical update *)
		tyref := Type ty2
	    else ();
	    if isfree then SOME S 
	    else SOME (TyVar.Map.insert(S, tyvar, ty2))) 
                 (* record the required instantiation of bound variables*)
	   handle SortError => NONE
	       |  ScopeError => NONE
          end)
    )

  | (Con(t1, tys1), Con(t2, tys2)) => 
    if TyName.eq(t1, t2) 
    then matchList S (tys1, tys2)
    else NONE

  | (Class(MLClass {tyname = t1, ... }), Class(MLClass {tyname = t2, ... })) =>
    if TyName.eq(t1, t2) 
    then SOME S
    else NONE    

  | (Fun(ty1, ty1'), Fun(ty2, ty2')) => 
    matchList S ([ty1,ty1'], [ty2,ty2'])

  | (Rec (ref (fixed1,_)), Rec (ref (fixed2,_))) =>
    matchRow S (Symbol.Map.listItemsi fixed1, 
      Symbol.Map.listItemsi fixed2)

  | (Ref(ty1, ty1'), Ref(ty2, ty2')) => 
    matchList S ([ty1,ty1'], [ty2,ty2'])

  | (Array ty1, Array ty2) => 
    matchTy S (ty1, ty2)
    
  | _ => 
    NONE
in
    matchTy TyVar.Map.empty (ty1, ty2)
end

(*----------------------------------------------------------------------*)
(* Anti-unify two types							*)
(* Currently limited to dealing with base type clashes only.            *)
(*----------------------------------------------------------------------*)
exception AntiUnify
fun antiunify (ty1, ty2) =
let
  fun replace (S, ty1, ty2) =  
  let
    fun loop [] =
        let
          val v = freshTyVar (TyVar.Normal TySort.any)
        in
          ((ty1, ty2, v)::S, tyVarType v)
        end

      | loop ((ty1', ty2', v)::S') =
        if eq(ty1, ty1') andalso eq(ty2, ty2')
        then (S, tyVarType v) else loop S'
  in
    loop S
  end
      
  fun au (S, ty1, ty2) =
    case (proj ty1, proj ty2) of
      (Fun(lty1, rty1), Fun (lty2, rty2)) => 
      let
        val (S, lty) = au (S, lty1, lty2)
        val (S, rty) = au (S, rty1, rty2)
      in
        (S, inj (Fun(lty, rty)))
      end

    | (Ref(lty1, rty1), Ref (lty2, rty2)) => 
      let
        val (S, lty) = au (S, lty1, lty2)
        val (S, rty) = au (S, rty1, rty2)
      in
        (S, inj (Ref(lty, rty)))
      end

    | (Array ty1, Array ty2) => 
      let
        val (S, ty) = au (S, ty1, ty2)
      in
        (S, inj (Array ty))
      end

    | (Con(tyname1, []), Con(tyname2, [])) =>
      if TyName.eq(tyname1, tyname2) 
      then (S, ty1)
      else replace (S, ty1, ty2)

(* SL: or *)
(*
    | ((Con(_, []), Var _) | (Var _, Con(_, []))) =>
      replace (S, ty1, ty2)
*)
    | (Con(_, []), Var _) =>
      replace (S, ty1, ty2)
    | (Var _, Con(_, [])) =>
      replace (S, ty1, ty2)
      
    | (Con(tyname1, tys1), Con(tyname2, tys2)) =>
      if TyName.eq(tyname1, tyname2)
      then
      let
        val (S, tys) = aus (S, tys1, tys2)
      in
        (S, inj (Con(tyname1, tys)))
      end else raise AntiUnify

    | (Rec (ref (fixed1, NONE)), Rec (ref (fixed2, NONE))) =>
      let
        val (labs1, tys1) = ListPair.unzip (Symbol.Map.listItemsi fixed1)
        val (labs2, tys2) = ListPair.unzip (Symbol.Map.listItemsi fixed2)
      in
        if Eq.list Symbol.equal (labs1, labs2)
        then 
        let
          val (S, tys) = aus (S, tys1, tys2)
          val fixed = ListPair.foldr (fn (lab, ty, f) =>
            Symbol.Map.insert(f, lab, ty)) Symbol.Map.empty (labs1, tys)
        in
          (S, inj (Rec (ref (fixed, NONE))))
        end       
        else raise AntiUnify
      end

    | (Var (ref (TyVar tyvar1)), Var (ref (TyVar tyvar2))) =>
      if TyVar.eq(tyvar1, tyvar2) then (S, ty1)
      else replace (S, ty1, ty2)
      
    | _ => 
      raise AntiUnify

  and aus (S, [], []) = 
      (S, [])

    | aus (S, ty1::tys1, ty2::tys2) =
      let
        val (S, ty) = au (S, ty1, ty2)
        val (S, tys) = aus (S, tys1, tys2)
      in
        (S, ty::tys)
      end
    | aus _ = raise AntiUnify
in
  let
    val (S, ty) = au ([], ty1, ty2)
  in
    SOME (foldr (fn ((ty1, ty2, v), m) => TyVar.Map.insert(m, v, (ty1,ty2)))
      TyVar.Map.empty S, ty)
  end handle AntiUnify => NONE
end

fun antiunifylist [] = NONE
  | antiunifylist [ty] = SOME (TyVar.Map.empty, ty)
  | antiunifylist (ty::tys) =
    case antiunifylist tys of
      NONE => NONE
    | SOME (S1, ty') =>
      case antiunify (ty, ty') of
        NONE => NONE
      | SOME (S2, ty') =>
        let
          val S3 = TyVar.Map.mapi
            (fn (tyvar, (ty1,ty2)) =>
              case proj ty2 of
                Var (ref (TyVar tyvar)) =>
                (case TyVar.Map.find(S1, tyvar) of
                  NONE => ty1 :: map (fn _ => ty2) tys
                | SOME tys => ty1 :: tys)
              | _ => ty1 :: map (fn _ => ty2) tys) S2              
        in
          SOME (S3, ty')
        end


(*----------------------------------------------------------------------*)
(* Resolve overloaded type variables in a type and check that no	*)
(* open record types remain.						*)
(* Return the free type variables remaining				*)
(*----------------------------------------------------------------------*)
fun resolve loc excludeset ty =
(
  case proj ty of
    Var(r as ref (TyVar tyvar)) =>
    (case TyVar.sort tyvar of
      TyVar.Normal s => 
      if TySort.<=(s, TySort.mono) 
      then TyVar.Set.empty 
      else TyVar.Set.singleton tyvar
    | TyVar.Overloaded tynames =>
      if TyVar.Set.member (excludeset, tyvar)
      then TyVar.Set.singleton tyvar

      else
      let
        val tyname = TyNames.default tynames
      in
        r := Type (inj (Con(tyname, []))); 
        (if TyName.Set.numItems tynames > 1 
        andalso Controls.get defaultsWarning
        then error (Error.warning (loc, "default type assumed: " ^ 
          TyName.toString tyname), [])
        else ());
        TyVar.Set.empty
      end
    )

  | Con(c, tys) => 
    resolveList loc excludeset tys

  | Class _ =>
    TyVar.Set.empty

  | Fun(ty1, ty2) => 
    TyVar.Set.union 
    (resolve loc excludeset ty1, resolve loc excludeset ty2)

  | Ref(ty1, ty2) => 
    TyVar.Set.union 
    (resolve loc excludeset ty1, resolve loc excludeset ty2)

  | Rec (ref row) => 
    resolveRow loc excludeset row

  | Array ty => 
    resolve loc excludeset ty
)

and resolveRow loc excludeset (fixed,NONE) = 
    resolveList loc excludeset (Symbol.Map.listItems fixed)

  | resolveRow loc excludeset (r as (fixed,SOME (ref (RowVar rowvar)))) =
    if TyVar.Set.member(excludeset, rowvar)
    then 
      TyVar.Set.add(resolveList loc excludeset 
        (Symbol.Map.listItems fixed), rowvar)
    else
      (error (Error.error (loc, "unresolved flex record type"),
        [("type", inj (Rec (ref r)))]);
      TyVar.Set.empty)

and resolveList loc excludeset [] = TyVar.Set.empty

  | resolveList loc excludeset (ty::tys) = 
    TyVar.Set.union 
    (resolve loc excludeset ty, resolveList loc excludeset tys)


end (* of local *)

end (* of struct *)





