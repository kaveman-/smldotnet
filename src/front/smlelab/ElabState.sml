structure ElabState :> ELABSTATE =
struct

local open SMLTy in

type ErrorArg = string * Type
type Errors   = (Error.Error * ErrorArg list) list

val entity = ref (Entity.Str, Id.dummySym)

(* Variable supplies *)
val tyvarsupply = ref TyVar.initial
val varsupply = ref 0
val tynamesupply = ref (TyName.initial (!entity))

val errors = ref ([] : Errors)
fun noSeriousErrors () = not (List.exists  (fn(e,_) => Error.isSerious e) (!errors))

val DE = ref ([] : DatEnv)
val psi = ref (TyName.Map.empty : Realisation)

type 'a ElabResult = 
  'a * Error.Error list * DatEnv * Realisation

(*----------------------------------------------------------------------*)
(* Generate a new type variable with the specified sort			*)
(*----------------------------------------------------------------------*)
fun freshTyVar sort = 
let
  val (tyvar, ts) = TyVar.fresh sort (!tyvarsupply)
in
  tyvarsupply := ts;
  tyvar
end

fun freshType () = tyVarType (freshTyVar (TyVar.Normal TySort.any))
fun freshMono () = tyVarType (freshTyVar (TyVar.Normal TySort.mono))

fun openRecType fields = 
let
  val rowvar = freshTyVar (TyVar.Normal TySort.any)
in
  SMLTy.inj (Rec (ref 
  (foldr (fn ((lab,ty),row) => 
    Symbol.Map.insert(row,lab,ty)) Symbol.Map.empty fields,
    SOME (ref (RowVar rowvar)))))
end

(*----------------------------------------------------------------------*)
(* Generate a new term variable						*)
(*----------------------------------------------------------------------*)
fun freshVar () = 
let
  val x = !varsupply
in
  varsupply := x+1;
  Symbol.symbol (UString.fromString ("$" ^ Int.toString x))
end

(*----------------------------------------------------------------------*)
(* Generate a new type name						*)
(*----------------------------------------------------------------------*)
fun freshTyName args =
let
  val (tyname, ts) = TyName.fresh args (!tynamesupply)
in
  tynamesupply := ts;
  tyname
end

(*----------------------------------------------------------------------*)
(* Generate a new type name						*)
(*----------------------------------------------------------------------*)
fun freshClass args =
let
  val (tyname, ts) = TyName.freshClass args (!tynamesupply)
in
  tynamesupply := ts;
  tyname
end

(*----------------------------------------------------------------------*)
(* Generate a set of new recursive type names				*)
(*----------------------------------------------------------------------*)
fun freshRecTyNames args =
let
  val (tynames, ts) = TyName.freshRec args (!tynamesupply)
in 
  tynamesupply := ts;
  tynames
end

(*----------------------------------------------------------------------*)
(* Generate new type names for each element of a set of type names      *)
(*----------------------------------------------------------------------*)
fun makeRenaming (longid, tynames) =
let  
  fun make [] = 
      (TyName.Map.empty, TyName.Set.empty)

    | make (tyname::tynames) =
      let 
        val (r,T) = make tynames
        val (tyname', ts) = 
          TyName.freshen longid (tyname,!tynamesupply) 
      in
        tynamesupply := ts;
        (TyName.Map.insert(r, tyname, tyname'), TyName.Set.add(T,tyname'))
      end
in
  make (TyName.Set.listItems tynames)
end

fun getStamp () = !tynamesupply

fun getEntity () = !entity

(*----------------------------------------------------------------------*)
(* Add an error message to the list					*)
(*----------------------------------------------------------------------*)
fun error e = 
  errors := e :: !errors

(*----------------------------------------------------------------------*)
(* Extend the datatype environment					*)
(*----------------------------------------------------------------------*)

(*@NOTE: CRUSSO the original definition,

fun addDE DE' =
  DE := DE' @ !DE

fails to accummulate the actual realisation.
*)
(*@TODO: review and test *)
(*@BUG: (potential) should we worry about capturing tvs or tn? 
        addDE assumes these aren't involved in psi *)
fun addDE DE' =
  let val psiDE' = map (map (fn (tvs,tn,m) =>
			(tvs,
			 tn,
			 Symbol.Map.map 
			   (Option.map (SMLTy.appRealisation (!psi)))
			   m)
			)) DE'
  in  DE := psiDE' @ !DE
  end

(*----------------------------------------------------------------------*)
(* Extend the realisation              					*)
(*----------------------------------------------------------------------*)
(*@NOTE: CRUSSO the original definition,

fun addRealisation psi' =
  psi := TyName.Map.unionWith #2 (!psi, psi')

fails to accummulate the actual realisation.
*)
(*@TODO: review and test *)
(*@BUG: CRUSSO (potential) should we worry about capturing tvs?
        addRealisation assumes these aren't involved in psi *)
fun addRealisation psi' =
  psi := TyName.Map.unionWith #2 
         (!psi, 
	  TyName.Map.map (fn (tvs,ty) => 
			  (tvs,SMLTy.appRealisation (!psi) ty))
	                  psi')


(*----------------------------------------------------------------------*)
(* Run an elaborator with the state initialised.			*)
(*----------------------------------------------------------------------*)
fun runelab newentity elaborator = 
  (tyvarsupply := TyVar.initial;
   tynamesupply := TyName.initial newentity;
   varsupply := 1;
   DE := [];
   psi := TyName.Map.empty;
   entity := newentity;
   errors := [];

  let 
    val v = elaborator ()

    val DE = !DE
    val errors = !errors
    val psi = !psi
   
    fun convertError (err, args) =
    let
      val tyvars = TyVar.Set.filter (not o TyVar.isExplicit) (foldl 
        (fn ((_,ty), tvs) => TyVar.Set.union(tyvars ty, tvs)) 
         TyVar.Set.empty args)
      val (S,_) = foldl (fn (tyvar, (S,supply)) =>
        let val (tyvar', supply') = TyVar.fresh (TyVar.sort tyvar) supply
        in
          ((tyvar, tyVarType tyvar')::S, supply')
        end) ([], TyVar.initial) (TyVar.Set.listItems tyvars)
    in
      Error.append(err, String.concat 
        (map (fn (tag, ty) => "\n    " ^ tag ^ ": " ^ openTypeToString
          (appSubst S ty)) args))
    end
 
  in 
    (v, map convertError errors, rev DE, psi)
  end)

fun appRenamingDE r =
  DE := map (fn dds =>
      (map (fn (tyvars, tyname, CE) =>
        (tyvars, TyName.rename r tyname, 
           Symbol.Map.map (Option.map (renameType r))
           CE)) dds)) (!DE)  
  

end

end