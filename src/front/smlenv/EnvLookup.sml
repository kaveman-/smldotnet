(*======================================================================*)
(* Lookup functions for SML environments                                *)
(* We must keep track of the actual position of an identifier within    *)
(* the domain of VE or SE so that translation into MIL tuple            *)
(* projections is straightforward.                                      *)
(*======================================================================*)
structure EnvLookup :> ENVLOOKUP = 
struct

local
  open SMLTy ValBind ElabState
in

(*----------------------------------------------------------------------*)
(* What position is atom s in map m wrt alphabetic ordering?            *)
(*@TODO: improve the efficiency of this.                                *)
(*----------------------------------------------------------------------*)
fun lookup (m,s) =
let
  fun find i [] = NONE
    | find i ((s',v)::rest) = 
      if Symbol.equal(s,s') then SOME (v,i) else find (i+1) rest
in
  find 0 (Id.fixMap m)
end

(*----------------------------------------------------------------------*)
(* Lookup an identifier, following path down substructures              *)
(*----------------------------------------------------------------------*)
fun lookupVid (E, loc, []) = 
    Debug.fail "Env.lookupVid: empty path"

  | lookupVid (E, loc, [var]) = 
    Symbol.Map.find(EnvOps.VEofE E, var)

  | lookupVid (E, loc, strid :: vid) =
    case Symbol.Map.find(EnvOps.SEofE E, strid) of
      NONE => 
      (error (Error.error (loc, "unbound structure: " ^ 
        Id.toString strid), []); NONE)

    | SOME E => 
      lookupVid (E, loc, vid)


(*----------------------------------------------------------------------*)
(* Look up a variable in a value environment                            *)
(*----------------------------------------------------------------------*)
fun lookupVarVE (VE, v) =
let
  fun lookup i [] = NONE
    | lookup i ((v',vb)::rest) =
      if Symbol.equal(v,v') then SOME (vb, i)
      else case vb of
        VarSch _ => lookup (i+1) rest
      | _ => lookup i rest
in
  lookup 0 (Id.fixMap VE)
end

(*----------------------------------------------------------------------*)
(* Lookup an identifier, following path down substructures              *)
(* Also return positions of identifiers within environments.            *)
(*----------------------------------------------------------------------*)
fun lookupVid' (E, loc, []) = 
    Debug.fail "Env.lookupVid': empty path"

  | lookupVid' (E, loc, [var]) = 
    (case Symbol.Map.find(EnvOps.VEofE E, var) of
      NONE => 
      NONE

    | SOME vbind =>
      (SOME (vbind, (var, []))))

  | lookupVid' (E, loc, strid::(op:: rest)) =
    let
      fun lookupSubStr (E, (var, [])) =
          (case lookupVarVE (EnvOps.VEofE E, var) of 
            NONE => 
            NONE
    
          | SOME (vb, i) =>
            (SOME (vb, [(var, 
              Symbol.Map.numItems (EnvOps.SEofE E) + i)])))

        | lookupSubStr (E, (strid, op:: rest)) =
          case lookup(EnvOps.SEofE E, strid) of
            NONE => 
            (error (Error.error(loc, "missing substructure: " ^ 
              Id.toString strid), []); NONE)

          | SOME (E, i) => 
            case lookupSubStr (E, rest) of
              NONE =>
              NONE

            | SOME (vbind, path) =>
              SOME (vbind, (strid,i)::path)
    in
      case Symbol.Map.find(EnvOps.SEofE E, strid) of
        NONE =>
(* should be error *)
        (error (Error.warning(loc, "missing structure: " ^ 
          Id.toString strid), []); NONE)

      | SOME E =>
        case lookupSubStr (E, rest) of
          NONE => 
          NONE

        | SOME (vbind, path) =>
          (SOME (vbind, (strid, path)))
    end

(*----------------------------------------------------------------------*)
(* Lookup a longtycon, following path down substructures.               *)
(*----------------------------------------------------------------------*)
fun lookupTyCon (E, loc, longtycon) =
let
  fun lookup (E, [tycon]) = 
      Symbol.Map.find (EnvOps.TEofE E, tycon)

    | lookup (E, strid :: longtycon) =
      case Symbol.Map.find(EnvOps.SEofE E, strid) of
        NONE => NONE
      | SOME E => lookup (E, longtycon)
in
  case lookup (E, longtycon) of
    SOME tystr => 
    SOME tystr

  | NONE =>
    (error (Error.error(loc, "unbound type constructor: " ^ 
    Longid.toString longtycon),[]); NONE)
end

(*----------------------------------------------------------------------*)
(* Lookup a structure, following path down substructures                *)
(*----------------------------------------------------------------------*)
fun lookupStr (E, loc, []) = 
    Debug.fail "EnvOps.lookupStr: empty path"

  | lookupStr (E, loc, strid :: longid) = 
    let
      fun lookupSubStr (E, []) = 
          (E, [])

        | lookupSubStr (E, strid :: longid) =
          case lookup(EnvOps.SEofE E, strid) of
            NONE => 
            (error (Error.error(loc, "missing substructure: " ^ 
              Id.toString strid), []); (EnvOps.emptyE, []))

          | SOME (E, i) => 
            let 
              val (E', longid') = lookupSubStr (E, longid)
            in
              (E', (strid,i)::longid')
            end
    in
      case Symbol.Map.find(EnvOps.SEofE E, strid) of
        NONE =>
        (error 
          (Error.error(loc, "missing structure: " ^ 
            Id.toString strid), []); (EnvOps.emptyE, (strid,[])))

      | SOME E =>
        let 
          val (E', longid') = lookupSubStr (E, longid)
        in
          (E', (strid, longid'))
        end
    end

(*----------------------------------------------------------------------*)
(* Lookup a functor identifier                                          *)
(*----------------------------------------------------------------------*)
fun lookupFunId (FE, loc, funid) = 
    case Symbol.Map.find(FE, funid) of
      NONE => 
      (error (Error.error (loc, "unbound functor: " ^ Id.toString funid), []);
       NONE)
    | SOME funinfo =>  SOME funinfo

(*----------------------------------------------------------------------*)
(* Lookup a signature identifier                                        *)
(*----------------------------------------------------------------------*)
fun lookupSigId(GE, loc, sigid) = 
    case Symbol.Map.find(GE, sigid) of
      NONE => 
      (error (Error.error (loc, "unbound signature: " ^ Id.toString sigid), []);
       NONE)
    | SOME sigma => SOME sigma

(*----------------------------------------------------------------------*)
(* Hooks to record the type of an object                                *)
(* These do nothing here, but are reimplemented for VS integration      *)
(*----------------------------------------------------------------------*)
fun instance(classty,loc) = () 
fun expression(loc,ty) = () 
fun pattern(loc,VE,ty) = () 
val cacheTypes = false
end

end



