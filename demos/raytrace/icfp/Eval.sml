structure Eval: Eval =
struct

open Lib
open Program

val rtd = 180.0 / Math.acos (~1.0)
val dtr = Math.acos (~1.0) / 180.0
fun deg x = rtd * x
fun rad x = dtr * x
val zero = VFloat 0.0
val one = VFloat 1.0


fun lookup (env, s) : int =
  case env of
     [] => failwith ("Unbound variable \"" ^ s ^ "\"")
   | s' :: env' =>
	if s = s'
	   then 0
	else 1 + (lookup(env', s))

fun conv_aux (absenv,acc,p) = 
   case p of
      [] => acc
    | Float x :: Float y :: Float z :: Prim Point :: r =>
	 conv_aux(absenv,Val' (VPoint (VFloat x, VFloat y, VFloat z))::acc,r)
    | t :: r =>
	 (case t of
	     Fun p' => conv_aux(absenv,Fun' (conv(absenv, p')) ::acc, r)
	   | Arr p' => conv_aux(absenv,Arr' (conv(absenv, p')) :: acc, r)
	   | Ident s => conv_aux(absenv,Ident' (lookup(absenv, s)) :: acc,r)
	   | Binder s => conv_aux(s :: absenv, Binder' :: acc,r)
	   | Int i =>  conv_aux (absenv,Val' (VInt i) :: acc, r)
	   | Float f => conv_aux(absenv,Val' (VFloat f) :: acc,r)
	   | Bool b => conv_aux(absenv,Val' (VBool b) :: acc, r)
	   | String s => conv_aux(absenv,Val' (VStr s)::acc,r)
	   | Prim k => conv_aux(absenv,Prim' k ::acc,r))
and conv (absenv, p) = List.rev(conv_aux(absenv,[],p));


fun inline (offset, env, p) =
   case p of
      [] => []
    | t :: r =>
	 let
	    fun normal() = t :: inline(offset, env, r)
	 in case t of
	    Fun' p' => Fun' (inline(offset, env, p')) :: inline(offset, env, r)
	  | Arr' p' => Arr' (inline(offset, env, p')) :: inline(offset, env, r)
	  | Ident' i =>
	       if i >= offset
		  then Val' (List.nth (env, i - offset)) :: inline(offset, env, r)
	       else normal()
	  | Binder' => Binder' :: inline (1 + offset, env, r)
	  | Prim' _ => normal()
	  | Val' _ => normal()
	 end

val inline_closure =
   fn (VClos (env, p)) => VClos ([], inline(0, env, p))
    | _ => failwith "a surface function was actually not a function"

val _ = Render.inline_closure := inline_closure

local 
fun eval(env, st, p,d) =
  case (st, p) of 
(* inlined value *)
    (_, Val' v :: r) => eval(env, (v :: st), r, d)
(* Rule 1 *)
(* Rule 2 *)
  | (v::st', Binder' :: r) => eval((v :: env), st', r, d)
(* Rule 3 *)
  | (_, Ident' i :: r) =>
      let val v = List.nth(env, i)
      in eval(env, (v :: st), r,d)
      end
(* Rule 4 *)
  | (_, Fun' f :: r) => eval(env, (VClos (env, f) :: st), r, d)
(* Rule 5 *)
(*  | (VClos (env', f) :: st', Prim' Apply :: r) =>
      eval(env, eval(env', st', f), r,d) *)
  | (VClos (env', f) :: st', Prim' Apply :: r) =>
       eval(env', st', f, (env,r)::d) 
(* Rule 6 *)
  | (_, Arr' a :: r) =>
      eval(env, (VArr (Array.fromList (List.rev (eval(env, [], a,[]))))) :: st, r,d) (*@TODO: this is not tail rec ! *)
(* Rules 7 and 8 *)
  | (VClos _ :: VClos (env', iftrue) :: VBool true :: st', Prim' If :: r) =>
     eval(env', st', iftrue,(env,r)::d)
  | (VClos (env', iffalse) :: VClos _ :: VBool false :: st', Prim' If :: r) =>
     eval(env', st', iffalse,(env,r)::d)
(* Operations on numbers *)
  | (VInt n2 :: VInt n1 :: st', Prim' Addi :: r) =>
       eval(env, (VInt (n1 + n2) :: st'), r,d)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Addf :: r) =>
      eval(env, (VFloat (f1 + f2) :: st'), r,d)
  | (VFloat f :: st', Prim' Acos :: r) =>
       eval(env, (VFloat (deg (Math.acos f)) :: st'), r,d)
  | (VFloat f :: st', Prim' Asin :: r) =>
       eval(env, (VFloat (deg (Math.asin f)) :: st'), r,d)
  | ((vf as VFloat f):: st', Prim' Clampf :: r) =>
      let val f' = if f < 0.0 then zero else if f > 1.0 then one else vf
      in eval(env, (f' :: st'), r,d)
      end
  | (VFloat f :: st', Prim' Cos :: r) =>
       eval(env, (VFloat (Math.cos (rad f)) :: st'), r,d)
  | (VInt n2 :: VInt n1 :: st', Prim' Divi :: r) =>
       eval(env, (VInt (n1 div n2) :: st'), r,d)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Divf :: r) =>
      eval(env, (VFloat (f1 / f2) :: st'), r,d)
  | (VInt n2 :: VInt n1 :: st', Prim' Eqi :: r) =>
       eval(env, (VBool (n1 = n2) :: st'), r,d)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Eqf :: r) =>
      eval(env, (VBool (Real.==(f1, f2)) :: st'), r,d)
  | (VFloat f :: st', Prim' Floor :: r) =>
      eval(env, (VInt (Real.floor f) :: st'), r,d)
  | (VFloat f :: st', Prim' Frac :: r) =>
       eval(env, (VFloat (Real.realMod f) :: st'), r,d)
  | (VInt n2 :: VInt n1 :: st', Prim' Lessi :: r) =>
      eval(env, (VBool (n1 < n2) :: st'), r,d)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Lessf :: r) =>
      eval(env, (VBool (f1 < f2) :: st'), r,d)
  | (VInt n2 :: VInt n1 :: st', Prim' Modi :: r) =>
      eval(env, (VInt (n1 mod n2) :: st'), r,d)
  | (VInt n2 :: VInt n1 :: st', Prim' Muli :: r) =>
       eval(env, (VInt (n1 * n2) :: st'), r,d)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Mulf :: r) =>
      eval(env, (VFloat (f1 * f2) :: st'), r,d)
  | (VInt n :: st', Prim' Negi :: r) => eval(env, (VInt (~ n) :: st'), r,d)
  | (VFloat f :: st', Prim' Negf :: r) => eval(env, (VFloat (~ f) :: st'), r,d)
  | (VInt n :: st', Prim' Real :: r) => eval(env, (VFloat (Real.fromInt n) :: st'), r,d)
  | (VFloat f :: st', Prim' Sin :: r) => eval(env, (VFloat (Math.sin (rad f)) :: st'), r,d)
  | (VFloat f :: st', Prim' Sqrt :: r) => eval(env, (VFloat (Math.sqrt f) :: st'), r,d)
  | (VInt n2 :: VInt n1 :: st', Prim' Subi :: r) => eval(env, (VInt (n1 - n2) :: st'), r,d)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Subf :: r) =>
      eval(env, (VFloat (f1 - f2) :: st'), r,d)
(* Operations on points *)
  | (VPoint (x, _, _) :: st', Prim' Getx :: r ) => eval(env, (x :: st'), r,d)
  | (VPoint (_, y, _) :: st', Prim' Gety :: r ) => eval(env, (y :: st'), r,d)
  | (VPoint (_, _, z) :: st', Prim' Getz :: r ) => eval(env, (z :: st'), r,d)
  | ((z as VFloat _) :: (y as VFloat _) :: (x as VFloat _) :: st',
     Prim' Point :: r) =>
      eval(env, (VPoint (x, y, z) :: st'), r,d)
  | (VInt i :: VArr a :: st', Prim' Get :: r) =>
      if i < 0 orelse i >= Array.length a
      then failwith "illegal access beyond array boundary"
      else eval(env, (Array.sub(a, i) :: st'), r,d)
  | (VArr a :: st', Prim' Length :: r) =>
      eval(env, (VInt (Array.length a) :: st'), r,d)
(* Geometric primitives *)
  | ((f as VClos _) :: st', Prim' Sphere :: r  ) =>
      eval(env, (VObj (OObj (OSphere, ref (Unopt f))) :: st'), r, d)
  | ((f as VClos _) :: st', Prim' Cube :: r    ) =>
      eval(env, (VObj (OObj (OCube, ref (Unopt f)))   :: st'), r, d)
  | ((f as VClos _) :: st', Prim' Cylinder :: r) =>
      eval(env, (VObj (OObj (OCylind, ref (Unopt f))) :: st'), r, d)
  | ((f as VClos _) :: st', Prim' Cone :: r    ) =>
      eval(env, (VObj (OObj (OCone, ref (Unopt f)))   :: st'), r, d)
  | ((f as VClos _) :: st', Prim' Plane :: r   ) =>
      eval(env, (VObj (OObj (OPlane, ref (Unopt f)))  :: st'), r, d)
(* Transformations *)
  | (VFloat z :: VFloat y :: VFloat x :: VObj ob :: st', Prim' Translate :: r) =>
      eval(env,
        (VObj (OTransform (ob,
                           Matrix.translate (x, y, z),
                           Matrix.translate (~ x, ~ y, ~ z),
                           1.0, true)) :: st'),
        r,d)
  | (VFloat z :: VFloat y :: VFloat x :: VObj ob :: st', Prim' Scale :: r) =>
       eval( env,
        (VObj (OTransform (ob,
                           Matrix.scale (x, y, z),
                           Matrix.unscale (x, y, z),
                           Real.max (Real.abs x,
				     (Real.max (Real.abs y, Real.abs z))),
                           false)) :: st'),
        r,
        d)
  | (VFloat s :: VObj ob :: st', Prim' Uscale :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.uscale s, Matrix.unuscale s,
                           Real.abs s, true)) :: st'),
        r,
        d)
  | (VFloat t :: VObj ob :: st', Prim' Rotatex :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.rotatex t, Matrix.rotatex (~ t),
                           1.0, true)) :: st'),
        r,
        d)
  | (VFloat t :: VObj ob :: st', Prim' Rotatey :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.rotatey t, Matrix.rotatey (~ t),
                           1.0, true)) :: st'),
        r,
        d)
  | (VFloat t :: VObj ob :: st', Prim' Rotatez :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.rotatez t, Matrix.rotatez (~ t),
                           1.0, true)) :: st'),
        r,
        d)
(* Lights *)
  | ((color as VPoint _) :: (dir as VPoint _) :: st', Prim' Light :: r) =>
      eval(env, (VLight (dir, color) :: st'), r,d)
  | ((color as VPoint _) :: (pos as VPoint _) :: st', Prim' Pointlight :: r) =>
      eval(env, (VPtLight (pos, color) :: st'), r, d)
  | ((expon as VFloat _) :: (cutoff as VFloat _) :: (color as VPoint _) ::
    (at as VPoint _) :: (pos as VPoint _) :: st', Prim' Spotlight :: r) =>
      eval(env, (VStLight (pos, at, color, cutoff, expon) :: st'), r, d)
(* Constructive geometry *)
  | ((VObj o2) :: (VObj o1) :: st', Prim' Union :: r) =>
      eval(env, (VObj (OUnion (o1, o2)) :: st'), r, d)
  | ((VObj o2) :: (VObj o1) :: st', Prim' Intersect :: r) =>
      eval(env, (VObj (OInter (o1, o2)) :: st'), r, d)
  | ((VObj o2) :: (VObj o1) :: st', Prim' Difference :: r) =>
      eval(env, (VObj (ODiff (o1, o2)) :: st'), r, d)
(* Rendering *)
  | (VStr file :: VInt ht :: VInt wid :: VFloat fov :: VInt depth ::
    VObj obj :: VArr lights :: VPoint (VFloat ax, VFloat ay, VFloat az) ::
    st', Prim' Render :: r) =>
(*
amb the intensity of ambient light (a point). 
lights is an array of lights used to illuminate the scene. 
obj is the scene to render. 
depth is an integer limit on the recursive depth of the ray tracing. 
fov is the horizontal field of view in degrees (a real number). 
wid is the width of the rendered image in pixels (an integer). 
ht is the height of the rendered image in pixels (an integer). 
file is a string specifying output file for the rendered image. 
*)
    (Render.f ((ax, ay, az), lights, obj, depth, fov, wid, ht, file)
     ; eval(env, st', r, d))
(* Termination or Return *)
  | (s, []) => 
    (case d of [] => st
        | (env',p')::d => eval(env',st,p',d))
(* Failure *)
  | _ =>
      raise (Stuck_computation (env, st, p))
in
val eval = fn (env,st,p) => eval(env,st,p,[])
end

fun apply (f, st) =
  case f of
    VClos (env, p) => eval(env, st, p)
  | _ => raise Fail "assert false"

val _ = Render.apply := apply

fun f p =
   let
      val st = eval([], [], (conv([], p)))
   in
(*@TODO: dice.gml terminates with a non-empty stack --- is this expected or a bug in eval?
      case st of
	 [] => ()
       | _ => failwith "error" *) 
   ()
   end handle Stuck_computation (env, st, p) => failwith "stuck"
      
end






