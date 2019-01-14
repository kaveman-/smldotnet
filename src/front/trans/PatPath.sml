(*======================================================================*)
(* Access paths through patterns. See signature for details.		*)
(*======================================================================*)
structure PatPath :> PATPATH =
struct

structure T = MILTerm

datatype PathItem = 
  Proj of int*int | ConArg of int | Deref of MILTy.Type | Unfold | Cast of MILTy.Type | NopCast of MILTy.Type
type Path = PathItem list

(*----------------------------------------------------------------------*)
(* An environment maps paths to variables. Crude list rep for now.	*)
(*@FUTURE: tree rep for path environments.				*)
(*----------------------------------------------------------------------*)
type Env = (Path * Var.Var) list

fun splitPath n obj =
  let fun loop i oargs =
            if i < 0 then oargs else
            loop (i-1) ((Proj(i,n)::obj) :: oargs)
  in loop (n-1) [] end;

(*----------------------------------------------------------------------*)
(* The empty environment containing only a root variable.		*)
(*----------------------------------------------------------------------*)
fun emptyEnv x = [([], x)]    

(*----------------------------------------------------------------------*)
(* Insert a path into an environment					*)
(*----------------------------------------------------------------------*)
fun insert env pair = pair::env

(*----------------------------------------------------------------------*)
(* Free equality on paths						*)
(*----------------------------------------------------------------------*)
fun eqItem (Proj i1, Proj i2) = i1=i2
  | eqItem (ConArg i1, ConArg i2) = i1=i2
  | eqItem (Deref _, Deref _) = true
  | eqItem (Unfold, Unfold) = true
  | eqItem (Cast ty1, Cast ty2) = MILTy.eq(ty1,ty2)
  | eqItem (NopCast ty1, NopCast ty2) = MILTy.eq(ty1,ty2)
  | eqItem _ = false

val equal = Eq.list eqItem

(*----------------------------------------------------------------------*)
(* Pretty-printing of paths and envs for diagnostics			*)
(*----------------------------------------------------------------------*)
fun itemToString item =
case item of
  Proj (i,n) => "#" ^ Int.toString i
| ConArg i => "c" ^ Int.toString i
| Deref _ => "!"
| Unfold => "u"
| Cast ty => ":>" ^ MILTy.toString ty

val toString = 
  Pretty.simpleVec "." itemToString 
val envToString = 
  Pretty.simpleVec ";" 
  (fn (path,var) => toString path ^ "|->" ^ Var.toString var)

(*----------------------------------------------------------------------*)
(* Look up a path in an environment, returning the variable and the	*)
(* remainder of the path.						*)
(*----------------------------------------------------------------------*)
fun lookup (env : Env) (path : Path) =
let
  val n = length path
  fun find [] = 
      Debug.fail 
      ("PatPath.lookup. \npath = " ^ toString path ^ "\nenv = " ^ envToString env)

    | find ((path',x)::rest) =
      let
        val n' = length path'
        val diff = n-n'
      in
        if diff >= 0 andalso equal (List.drop(path, diff), path')
        then (x, List.take(path, diff))
        else find rest
      end
in
  find env
end


(*----------------------------------------------------------------------*)
(* Compile a path down to a context (appropriate projection bindings)	*)
(* and a value (a variable, possibly with mu-elimination).		*)
(*----------------------------------------------------------------------*)
fun compile (env : Env) (varOpt : Id.id option , path : Path) =
let
  val (x, rest) = lookup env path
  fun make freshVar [] = 
      (Gen.identity, T.Var x)

    | make freshVar (item::path') = 
      let
        val (C, v) = make (TransOps.freshAnonVar) path'
      in
        case item of
          Unfold => 
          (C, T.Unfold v)

        | Deref ty =>
          let
            val (x,xv) = freshVar ()
          in
            (fn e => C (T.Let(T.Special((Ext.Prim (Id.fromString "!"), NONE, NONE), [v], MILTy.cmp(Effect.reads, [ty])), ([(x,ty)], e))), xv)
          end

        | Proj (i,n) =>
          let
            val (x,xv) = freshVar()
          in
            (fn e => C (T.LetVal(x, T.Proj(i, n, v), e)), xv)
          end

        | Cast ty =>
          let
            val (x,xv) = freshVar ()
          in
            (fn e => C (T.Let(T.Special((Ext.Cast, NONE, NONE), [v], 
              MILTy.noeffect [ty]), ([(x,ty)], e))), xv)
          end

        | NopCast ty =>
          let
            val (x,xv) = freshVar ()
          in
            (fn e => C (T.LetVal(x,T.As(v,ty),e)), xv)
          end

	  (* If we're required to project on a constructor then we must do a trivial
             case on it first. Hopefully this will be optimized away later. *)
        | ConArg i =>
          let
            val (x,xv) = freshVar ()
          in
            (fn e => C (T.Case(v, [(i, ([x], e))], NONE, MILTy.noeffect [])), xv)
          end
      end
in
  make (case varOpt of 
            NONE => TransOps.freshAnonVar 
          | SOME var =>  fn () => TransOps.freshBoundVar var) rest
end

end





