structure Render: Render =
struct

open Lib
infix 9 **
val op ** = Math.pow

open Program

(* Scene description *)
datatype kind = (* section 3.2 *)
    SSphere of Matrix.v (* Center *) * Real.real (* Square of the radius *)
  | SEllips
  | SCube of Matrix.v (* Normal x = 0 *) *
             Matrix.v (* Normal y = 0 *) *
             Matrix.v (* Normal z = 0 *)
  | SCylind of Matrix.v (* Normal *)
  | SCone of Matrix.v (* Normal *)
  | SPlane of Matrix.v (* Equation *) * Matrix.v (* Normal *)

datatype scene = (* section 3.7 *)
    SObj of kind * closure ref (* surface function *) * Matrix.t
  | SBound of scene * Matrix.v (* Center *) * Real.real (* Square of the radius *)
  | SUnion of scene * scene
  | SInter of scene * scene
  | SDiff of scene * scene

datatype light = (* section 3.5 *)
    Light of Matrix.v (* negated & normalized *) * (Real.real * Real.real * Real.real)
  | PtLight of Matrix.v * (Real.real * Real.real * Real.real)
  | StLight of Matrix.v * Matrix.v (* negated & normalized *) *
               (Real.real * Real.real * Real.real) * Real.real (* cos *) * Real.real

type desc =
  { amb : Real.real * Real.real * Real.real,
    lights : light array,
    scene : scene }

open MathLib
open Matrix

(**** Scene calculation ****)

(* Plane equation and normal in world coordinates *)
fun plane_eq(m, v) =
  let
     val n = vmul (transpose m, v )
  in
     (n, normalize(#1 n, #2 n, #3 n, 0.0))
  end

val origin = ( 0.0, 0.0, 0.0, 1.0 )
val cube_center = ( 0.5, 0.5, 0.5, 1.0 )
val cylinder_center = ( 0.0, 0.5, 0.0, 1.0 )
val cone_center = ( 0.0, 1.0, 0.0, 1.0 )

fun intern_obj(m, m1, scale, isom, ob) =
(* apply transformations *)
  case ob of
    OObj (OSphere, f) =>
       if isom
	  then
	     let
		val center = vmul (m1, origin)
		val radius = scale * scale
	     in
		SBound (SObj (SSphere (center, radius), f, m), center, radius)
	     end
       else
	  let
	     val center = vmul (m1, origin)
	     val radius = scale * scale
	  in
	     SBound (SObj (SEllips, f, m), center, radius)
	  end
  | OObj (OCube, f) =>
      let
	 val (nx, nx') = plane_eq(m, (1.0, 0.0, 0.0, 0.0))
	 val (ny, ny') = plane_eq(m, (0.0, 1.0, 0.0, 0.0))
	 val (nz, nz') = plane_eq(m, (0.0, 0.0, 1.0, 0.0))
	 val c = SObj (SCube (nx', ny', nz'), f, m)
      in
	 SBound (c, vmul (m1, cube_center), scale * scale * 0.75)
      end
  | OObj (OCylind, f) =>
      let
	 val (n, n') = plane_eq(m, (0.0, 1.0, 0.0, 0.0))
	 val c = SObj (SCylind n', f, m)
      in
	 SBound (c, vmul(m1, cylinder_center), scale * scale * 1.25)
      end
  | OObj (OCone, f) =>
      let
	 val (n, n') = plane_eq(m, (0.0, 1.0, 0.0, 0.0))
	 val c = SObj (SCone n', f, m)
      in
	 SBound (c, vmul(m1, cone_center), scale * scale)
      end
  | OObj (OPlane, f) =>
      let
	 val (n, n') = plane_eq(m, (0.0, 1.0, 0.0, 0.0))
      in
	 SObj (SPlane (n, n'), f, m)
      end
  | OTransform (o', m', m'1, scale', isom') =>
      intern_obj
        (Matrix.mul(m', m), Matrix.mul(m1, m'1),
	 scale * scale', isom andalso isom', o')
  | OUnion (o1, o2) =>
      SUnion (intern_obj(m, m1, scale, isom, o1),
	      intern_obj(m, m1, scale, isom, o2))
  | OInter (o1, o2) =>
      SInter (intern_obj(m, m1, scale, isom, o1),
	      intern_obj(m, m1, scale, isom, o2))
  | ODiff (ODiff (o1, o2), o3) =>
      (* Better to have unions that diffs for introducing bounds *)
      intern_obj(m, m1, scale, isom, (ODiff (o1, OUnion (o2, o3))))
  | ODiff (o1, o2) =>
      SDiff (intern_obj(m, m1, scale, isom, o1),
	     intern_obj(m, m1, scale, isom, o2))

fun intern_lights a =
    let fun map f a = Array.tabulate(Array.length a, fn i => f(Array.sub(a, i)))
    in
      map
      (fn VLight (VPoint (VFloat x, VFloat y, VFloat z),
                 VPoint (VFloat r, VFloat g, VFloat b)) =>
           Light (normalize (neg (x, y, z, 0.0)), (r, g, b))
       | VPtLight (VPoint (VFloat x, VFloat y, VFloat z),
                   VPoint (VFloat r, VFloat g, VFloat b)) =>
           PtLight ((x, y, z, 1.0), (r, g, b))
       | VStLight (VPoint (VFloat x, VFloat y, VFloat z),
                   VPoint (VFloat x', VFloat y', VFloat z'),
                   VPoint (VFloat r, VFloat g, VFloat b),
                   VFloat cutoff, VFloat exp) =>
           StLight ((x, y, z, 1.0),
                    normalize (x - x', y - y', z - z', 0.0),
                    (r, g, b), dcos cutoff, exp)
       | _ =>
           raise(Fail "assert false"))
      a
    end

(**** Scene optimization ****)

fun flatten_rec(sc, rem) =
  case sc of
    SUnion (sc1, sc2) => flatten_rec(sc1, flatten_rec(sc2, rem))
  | sc                => sc :: rem

fun flatten_union sc = flatten_rec(sc, [])

fun object_cost k : int =
  case k of
    SSphere _ => 1
  | SEllips   => 2
  | SCube _   => 4
  | SCylind _ => 2
  | SCone _   => 2
  | SPlane _  => 0 (* Planes do not have a bounding box anyway *)

fun add_bound (r0, (x, r, cost, sc)) =
  if r0 < 0.0
     then
	 if r < 0.0 orelse cost <= 1
	    then (cost, sc)
	 else
	    (1, SBound (sc, x, r))
  else
     (* Cost of bounds *)
     let
	val c0 = r0 + r * Real.fromInt cost 
	(* Cost ofout bounds *)
	val c1 = r0 * Real.fromInt cost
     in
	if c0 < c1 then
	   (1, SBound (sc, x, r))
	else
	   (cost, sc)
     end

fun union_bound (dsc1 as (x1, r1, cost1, sc1),
		 dsc2 as (x2, r2, cost2, sc2)) =
  if r1 < 0.0 then
    let
       val (cost2', sc2') = add_bound(r1, dsc2)
    in
       (x1, r1, cost1, SUnion (sc1, sc2'))
    end
  else if r2 < 0.0 then
    let
       val (cost1', sc1') = add_bound (r2, dsc1)
    in
       (x2, r2, cost2, SUnion (sc1', sc2))
    end
  else
    let
       val d = Math.sqrt (square (sub(x2, x1)))
       val r1' = Math.sqrt r1
       val r2' = Math.sqrt r2
    in
       if d + r2' <= r1' then
	  let
	     val (cost2', sc2') = add_bound (r1, dsc2)
	  in
	     (x1, r1, cost1 + cost2', SUnion (sc1, sc2'))
	  end
       else if d + r1' <= r2' then
	  let
	     val (cost1', sc1') = add_bound (r2, dsc1)
	  in
	     (x2, r2, cost1' + cost2, SUnion (sc1', sc2))
	  end
	    else
	       let
		  val r' = (r1' + r2' + d) * 0.5
		  val r = r' * r'
		  val x = add_scaled (x1, (r' - r1') / d, sub(x2, x1))
		  val (cost1', sc1') = add_bound (r, dsc1)
		  val (cost2', sc2') = add_bound (r, dsc2)
	       in
		  (x, r, cost1' + cost2', SUnion (sc1', sc2'))
	       end
    end
 
fun union_radius (dsc1 as (x1, r1, cost1, sc1),
		  dsc2 as (x2, r2, cost2, sc2)) =
    let
       val d = Math.sqrt (square (sub (x2, x1)))
       val r1' = Math.sqrt r1
       val r2' = Math.sqrt r2
    in
       if d + r2' <= r1' then r1 else
	  if d + r1' <= r2' then r2 else
	     let
		val r' = (r1' + r2' + d) * 0.5
	     in
		r' * r'
	     end
    end

fun merge2 l =
  case l of
    sc1 :: sc2 :: r => union_bound (sc1, sc2) :: merge2 r
  | _               => l

fun merge_union l =
  case l of
    []    => raise(Fail "assert false")
  | [sc1] => sc1
  | l     => merge_union (merge2 l)

fun opt_union l =
  case l of
    [] => l
  | [_] => l
  | [sc1, sc2] => [union_bound(sc1, sc2)]
  | _ =>
       let
	  val c = Array.fromList l
	  val n = Array.length c
	  val m = Array2.array(n, n, Real.posInf)
	  val _ =
	     for(0, n - 1, fn i =>
		 for(0, n - 1, fn j =>
		     if i <> j
			then Array2.update(m, i, j,
					   union_radius
					   (Array.sub(c, i), Array.sub(c, j)))
		     else ()))
	  val remain = Array.tabulate (n, fn i => i)
	  val _ =
	     forDown
	     (n - 1, 1, fn k =>
	      let
		 val gain = ref Real.posInf
		 val i0 = ref 0
		 val j0 = ref 0
		 val _ =
		    for(0, k, fn i =>
			for(0, k, fn j =>
			    let
			       val i' = Array.sub(remain, i)
			       val j' = Array.sub(remain, j)
			    in
			       if Array2.sub(m, i', j') < !gain
				  then 
				     (gain := Array2.sub(m, i', j')
				      ; i0 := i
				      ; j0 := j)
			       else ()
			    end))
		 val i = Array.sub(remain, !i0)
		 val j = Array.sub(remain, !j0)
	      in
		 Array.update(remain, !j0, Array.sub(remain, k));
		 Array.update(c, i,
			      union_bound (Array.sub(c, i), Array.sub(c, j)));
		 for(0, k - 1, fn j0 =>
		     let
			val j = Array.sub(remain, j0)
		     in
			if i <> j
			   then
			      (
			       Array2.update
			       (m, i, j,
				union_radius
				(Array.sub(c, i), Array.sub(c, j)));
			       Array2.update
			       (m, j, i,
				union_radius
				(Array.sub(c, i), Array.sub(c, j))))
			else ()
		     end)
	      end)
       in [Array.sub(c, Array.sub(remain, 0))]
       end

fun optimize_rec sc =
  case sc of
    SObj (kind, _, _) =>
      (origin, ~1.0, object_cost kind, sc)
  | SUnion _ =>
       let
	  val l = List.map optimize_rec (flatten_union sc)
	  val unbounded = List.filter (fn (_, r, _, _) => r < 0.0) l
	  val bounded = List.filter (fn (_, r, _, _) => r >= 0.0) l
       in
	  merge_union (opt_union bounded @ unbounded)
       end
  | SInter (sc1, sc2) =>
       let
	  val (x1, r1, cost1, sc1) = optimize_rec sc1
	  val (x2, r2, cost2, sc2) = optimize_rec sc2
       in
	  if r2 < 0.0 then
	     (x2, r2, cost2, SInter (sc1, sc2))
	  else if r1 < 0.0 then
	     (x1, r1, cost1, SInter (sc2, sc1))
	       else if r1 < r2 then
		  (x1, r1, cost1, SInter (sc1, sc2))
		    else
		       (x2, r2, cost1, SInter (sc2, sc1))
       end
  | SDiff (sc1, sc2) =>
       let
	  val (x1, r1, cost1, sc1) = optimize_rec sc1
	  val dsc2 as (x2, r2, cost2, sc2) = optimize_rec sc2
	  val (cost2', sc2') = add_bound (r1, dsc2)
       in
	  (x1, r1, cost1, SDiff (sc1, sc2'))
       end
  | SBound (sc1, x, r) =>
       let
	  val (_, _, cost1, sc1) = optimize_rec sc1
       in
	  (x, r, cost1, sc1)
       end

fun optimize sc = #2 (add_bound (~1.0, optimize_rec sc))

(**** Rendering ****)

(* operations for intervals *)
fun union (l1, l2) : (Real.real * scene * Real.real * scene) list = 
  case (l1, l2) of
    ([], _) => l2
  | (_, []) => l1
  | ((i1 as (t1, o1, t1', o1')) :: r1,
     (i2 as (t2, o2, t2', o2')) :: r2) =>
    if t1' < t2
       then i1 :: union(r1, l2)
    else if t2' < t1
	    then i2 :: union(l1, r2)
	 else
	    if t1 < t2 then
	       if t1' < t2' then
		  union(r1, (t1, o1, t2', o2')::r2)
	       else
		  union((t1, o1, t1', o1')::r1, r2)
	    else
	       if t1' < t2' then
		  union(r1, ((t2, o2, t2', o2')::r2))
	       else
		  union((t2, o2, t1', o1')::r1, r2)

fun inter (l1, l2) : (Real.real * scene * Real.real * scene) list = 
  case (l1, l2) of
    ([], _) => []
  | (_, []) => []
  | ((i1 as (t1, o1, t1', o1')) :: r1,
     (i2 as (t2, o2, t2', o2')) :: r2) =>
    if t1' <= t2
       then inter(r1, l2)
    else if t2' <= t1
	    then inter(l1, r2)
	 else
	    if t1 < t2 then
	       if t1' < t2' then
		  (t2, o2, t1', o1') :: inter(r1, l2)
	       else
		  i2 :: inter(l1, r2)
	    else
	       if t1' < t2' then
		  i1 :: inter(r1, l2)
	       else
		  (t1, o1, t2', o2') :: inter(l1, r2)

fun diff (l1, l2) : (Real.real * scene * Real.real * scene) list =
  case (l1, l2) of
     ([], _) => []
   | (_, []) => l1
  | ((i1 as (t1, o1, t1', o1')) :: r1,
     (i2 as (t2, o2, t2', o2')) :: r2) =>
    if t1' <= t2
       then i1 :: diff(r1, l2)
    else if t2' <= t1
	    then diff(l1, r2)
	 else
	    if t1 < t2 then
	       if t1' < t2' then
		  (t1, o1, t2, o2) :: diff(r1, l2)
	       else
		  (t1, o1, t2, o2) :: diff((t2', o2', t1', o1') :: r1, r2)
	    else
	       if t1' < t2' then
		  diff(r1, l2)
	       else
		  diff((t2', o2', t1', o1') :: r1, r2)

(* intersection of ray and object *)
fun plane (orig, dir, scene, eq) : (Real.real * scene * Real.real * scene) list =
   let
      val porig = prod (eq, orig)
      val pdir = prod (eq, dir)
      val t = ~ porig / pdir
   in
      if porig < 0.0 then
	 if t > 0.0 then
	    [(0.0, scene, t, scene)]
	 else
	    [(0.0, scene, Real.posInf, scene)]
      else
	 if t > 0.0 then
	    [(t, scene, Real.posInf, scene)]
	 else
	    []
   end

fun band (obj, x, v, i) : (Real.real * scene * Real.real * scene) list = 
   let
      val t1 = ~ (i x) / (i v)
      val t2 = (1.0 - (i x)) / (i v)
      val t2' = if t1 >= t2 then t1 else t2
   in
      if t2' < 0.0 then
	 []
      else
	 let val t1' = if t1 <= t2 then t1 else t2
	 in
	    if t1' < 0.0 then
	       [(0.0, obj, t2', obj)]
	    else
	       [(t1', obj, t2', obj)]
	 end
   end

fun cube (orig, dir, scene, m): (Real.real * scene * Real.real * scene) list =
   let
      val x = vmul (m, orig)
      val v = vmul (m, dir)
   in
      case band (scene, x, v, #1) of
	 [] => []
       | l0 =>
	    case inter (l0, band (scene, x, v, #2)) of
	       [] => []
	     | l1 => inter (l1, band (scene, x, v, #3))
   end

fun sphere (orig, dir, scene, x, r2): (Real.real * scene * Real.real * scene) list =
   let
      val v = sub (x, orig)
      (* Square of the distance between the origin and the center of the sphere *)
      val v2 = square v
      val dir2 = square dir
      val p = prod (v, dir)
      (* Square of the distance between the ray and the center *)
      val d2 = v2 - p * p / dir2
      val delta = r2 - d2
   in  if delta <= 0.0
	  then []
       else
	  let
	     val sq = Math.sqrt (delta / dir2)
	     val t1 = p / dir2 - sq
	     val t2 = p / dir2 + sq
	  in
	     if t2 < 0.0
		then []
	     else
		[(max_real (0.0, t1), scene, t2, scene)]
	  end
   end

fun ellipsoid (orig, dir, scene, m): (Real.real * scene * Real.real * scene) list =
   let
      val x = vmul (m, orig)
      val v = vmul (m, dir)
      val x2 = square x
      val v2 = square v
      val xv = prod (x, v)
      val delta = xv * xv - v2 * (x2 - 2.0)
   in
      if delta <= 0.0 then
	 []
      else
	 let
	    val sq = Math.sqrt delta
	    val t1 = (~ xv - sq) / v2
	    val t2 = (~ xv + sq) / v2
	 in    if t2 < 0.0 then
	    []
	       else
		  [(max_real (0.0, t1), scene, t2, scene)]
	 end
   end

fun cylinder (orig, dir, scene, m): (Real.real * scene * Real.real * scene) list =
   let
      val x = vmul (m, orig)
      val v = vmul (m, dir)
      val x2 = #1 x * #1 x + #3 x * #3 x - 1.0
      val v2 = #1 v * #1 v + #3 v * #3 v
      val xv = #1 x * #1 v + #3 x * #3 v
      val delta = xv * xv - v2 * x2
   in
      if delta <= 0.0 then
	 []
      else
	 let
	    val sq = Math.sqrt delta
	    val t1 = (~ xv - sq) / v2
	    val t2 = (~ xv + sq) / v2
	 in    if t2 < 0.0 then
	    []
	       else
		  inter
		  ([(max_real (0.0, t1), scene, t2, scene)],
		   band (scene, x, v, #2))
	 end
   end

fun cone (orig, dir, scene, m): (Real.real * scene * Real.real * scene) list = 
   let
      val x = vmul (m, orig)
      val v = vmul (m, dir)
      val x2 = #1 x * #1 x + #3 x * #3 x - #2 x * #2 x
      val v2 = #1 v * #1 v + #3 v * #3 v - #2 v * #2 v
      val xv = #1 x * #1 v + #3 x * #3 v - #2 x * #2 v
      val delta = xv * xv - v2 * x2
   in
      if delta <= 0.0 then
	 []
      else
	 let
	    val sq = Math.sqrt delta
	    val t1 = (~ xv - sq) / v2
	    val t2 = (~ xv + sq) / v2
	 in
	    if t1 <= t2 then
	       if t2 < 0.0 then
		  []
	       else
		  inter
		  ([(max_real(0.0, t1), scene, t2, scene)],
		   band (scene, x, v, #2))
	    else
	       inter
	       (if t1 <= 0.0 then
		   [(0.0, scene, Real.posInf, scene)]
		else if t2 <= 0.0 then
		   [(t1, scene, Real.posInf, scene)]
		     else
			[(0.0, scene, t2, scene), (t1, scene, Real.posInf, scene)],
	        band (scene, x, v, #2))
	 end
   end

fun intersect (orig, dir, x, r2) =
   let
      val (vx, vy, vz, vt) = sub (x, orig)
  (* Square of the distance between the origin and the center of the sphere *)
      val v2 = vx * vx + vy * vy + vz * vz + vt * vt
      val (dx, dy, dz, dt) = dir
      val dir2 = dx * dx + dy * dy + dz * dz + dt * dt
      val p = vx * dx + vy * dy + vz * dz + vt * dt
      (* Square of the distance between the ray and the center *)
      val d2 = v2 - p * p / dir2
   in r2 > d2
   end

fun find_all (orig, dir, scene) =
  case scene of
    SObj (SSphere (x, r2), _, m) =>
      sphere (orig, dir, scene, x, r2)
  | SObj (SEllips, _, m) =>
      ellipsoid (orig, dir, scene, m)
  | SObj (SCube _, _, m) =>
      cube (orig, dir, scene, m)
  | SObj (SCylind _, _, m) =>
      cylinder (orig, dir, scene, m)
  | SObj (SCone _, _, m) =>
      cone (orig, dir, scene, m)
  | SObj (SPlane (eq, _), _, m) =>
      plane (orig, dir, scene, eq)
  | SBound (sc, x, r2) =>
      if intersect (orig, dir, x, r2)
	 then find_all (orig, dir, sc)
      else []
  | SUnion (sc1, sc2) =>
      union (find_all (orig, dir, sc1), find_all (orig, dir, sc2))
  | SInter (sc1, sc2) =>
      let val l1 = find_all (orig, dir, sc1)
      in
	 case l1 of
	    [] => []
	  | _ => inter(l1, find_all (orig, dir, sc2))
      end
  | SDiff (sc1, sc2) =>
      let val l1 = find_all(orig, dir, sc1)
      in
	 case l1 of
	    [] => []
	  | _ => diff(l1, find_all(orig, dir, sc2))
      end

fun filter_inter_list l =
  case l of
    (t, _, _, _)::r =>
       if t < epsilon
	  then filter_inter_list r
       else l
  | _ => l

fun hit_from_inter bounded l0 =
  let val l = filter_inter_list l0
  in
     case l of
	[] => false
      | (t, _, _, _)::r => (not bounded orelse  t <= 1.0)
  end

fun hit(orig, dir, scene, bounded) =
  case scene of
    SObj (kind, _, m) =>
       (case
	   (case kind of
	       SSphere (x, r2) => sphere (orig, dir, scene, x, r2)
	     | SEllips         => ellipsoid (orig, dir, scene, m)
	     | SCube _         => cube (orig, dir, scene, m)
	     | SCylind _       => cylinder (orig, dir, scene, m)
	     | SCone _         => cone (orig, dir, scene, m)
	     | SPlane (eq, _)  => plane (orig, dir, scene, eq)) of
	       [] => false
	     | [(t, _, _, _)] =>
		  if bounded andalso t > 1.0
		     then false
		  else if t < epsilon
			  then false
		       else true
	     | _ => true)
  | SBound (sc, x, r2) =>
      intersect (orig, dir, x, r2)  andalso hit (orig, dir, sc, bounded)
  | SUnion (sc1, sc2) =>
      hit (orig, dir, sc1, bounded) orelse hit (orig, dir, sc2, bounded)
  | SInter (sc1, sc2) =>
      let val l1 = find_all (orig, dir, sc1)
      in
	 case l1 of
	    [] => false
	  | _ => hit_from_inter bounded (inter(l1, find_all (orig, dir, sc2)))
      end
  | SDiff (sc1, sc2) =>
      let
	 val l1 = find_all(orig, dir, sc1)
      in
	 case l1 of
	    [] => false
	  | _ => hit_from_inter bounded (diff(l1, find_all(orig, dir, sc2)))
      end

fun visible (desc: desc, orig, dir, bounded) =
  not (hit(orig, dir, #scene desc, bounded))

val black = (0.0, 0.0, 0.0)

val apply : ((Program.v * Program.v list) -> Program.v list) ref =
   ref (fn _ => raise(Fail "assert false"))
val inline_closure : (Program.v -> Program.v) ref =
   ref (fn _ => raise(Fail "assert false"))

(* Value between 0 and 1 from the sinus and cosinus *)
(* Actually, only the sign of the sinus is used *)
fun angle (si, co) =
  let
     val u = dacos co / 360.0
  in
     if si > 0.0 then u else 1.0 - u
  end

fun texture_coord (kind, x: v) = (* section 3.6 *) 
   let
      fun ellipsOrSphere() =
	 let
	    val y = #2 x
	    val v = (y + 1.0) * 0.5
	 in
	    if v < epsilon
	       then [VFloat v, VFloat 0.0, VInt 0]
	    else
	       let
		  val u = angle (#1 x, #3 x / Math.sqrt (1.0 - y * y))
	       in
		  [VFloat v, VFloat u, VInt 0]
	       end
	 end
   in  (* [v; u; face] *)
      case kind of
	 SEllips => ellipsOrSphere()
       | SSphere _ => ellipsOrSphere()
       | SCube _ =>
      if Real.abs (#3 x) < epsilon then
        [VFloat (#2 x), VFloat (#1 x), VInt 0]
      else if Real.abs ((#3 x) - 1.0) < epsilon then
        [VFloat (#2 x), VFloat (#1 x), VInt 1]
      else if Real.abs (#1 x) < epsilon then
        [VFloat (#2 x), VFloat (#3 x), VInt 2]
      else if Real.abs ((#1 x) - 1.0) < epsilon then
        [VFloat (#2 x), VFloat (#3 x), VInt 3]
      else if Real.abs ((#2 x) - 1.0) < epsilon then
        [VFloat (#3 x), VFloat (#1 x), VInt 4]
      else (* if Real.abs (#2 x) < epsilon then *)
        [VFloat (#3 x), VFloat (#1 x), VInt 5]
  | SCylind _ =>
      if Real.abs (#2 x) < epsilon then
        [VFloat (((#3 x) + 1.0) * 0.5), VFloat (((#1 x) + 1.0) * 0.5), VInt 2]
      else if Real.abs ((#2 x) - 1.0) < epsilon then
        [VFloat (((#3 x) + 1.0) * 0.5), VFloat (((#1 x) + 1.0) * 0.5), VInt 1]
      else
        let
	   val u = angle (#1 x, #3 x)
	in
	   [VFloat (#2 x), VFloat u, VInt 0]
	end
  | SCone _ =>
      let val v = (#2 x)
      in
	 if Real.abs v < epsilon then
	    [VFloat v, VFloat 0.0, VInt 0]
	 else
	    if Real.abs ((#2 x) - 1.0) < epsilon
	       then
		  [VFloat (((#3 x) + 1.0) * 0.5),
		   VFloat (((#1 x) + 1.0) * 0.5),
		   VInt 1]
	    else
	       let
		  val u = angle (#1 x, (#3 x) / v)
	       in
		  [VFloat v, VFloat u, VInt 0]
	       end
      end
  | SPlane _ =>
      [VFloat (#3 x), VFloat (#1 x), VInt 0]
   end

fun normal (kind, m, x', x) =
  case kind of
    SSphere (x0, _) =>
      normalize (sub (x, x0))
  | SEllips =>
      let val (n0, n1, n2, _) = vmul (transpose m, x')
      in
	 normalize(n0, n1, n2, 0.0)
      end
  | SCylind n =>
      if Real.abs (#2 x') < epsilon
	 orelse Real.abs (#2 x') - 1.0 < epsilon then
        n
      else
        let
	   val (n0, n1, n2, _) = vmul (transpose m, (#1 x', 0.0, #3 x', 0.0))
	in
	   normalize(n0, n1, n2, 0.0)
	end
  | SCone n =>
      if Real.abs (#2 x') - 1.0 < epsilon
	 then n
      else
        let
	   val (n0, n1, n2, _) =
	      vmul (transpose m, (#1 x', ~(#2 x'), #3 x', 0.0))
	in
	   normalize(n0, n1, n2, 0.0)
	end
  | SCube (nx, ny, nz) =>
      if Real.abs (#3 x') < epsilon
	 orelse Real.abs (#3 x') - 1.0 < epsilon
	 then nz
      else if Real.abs (#1 x') < epsilon
 	      orelse Real.abs (#1 x') - 1.0 < epsilon
	      then nx
	   else ny
  | SPlane (_, n) =>
      n

fun apply_surface_fun (f, v) =
  case !apply(f, v) of
    [VFloat n, VFloat ks, VFloat kd,
     VPoint (VFloat cr, VFloat cg, VFloat cb)] =>
       (n, ks, kd, cr, cg, cb)
  | _ =>
      failwith "A surface function returns some incorrect values"

fun trace (desc: desc, depth: int, orig, dir) =
   let
      val dir = normalize dir
   in
      case filter_inter_list (find_all(orig, dir, #scene desc)) of
	 [] => black
       | (t, ob, _, _) :: _ => trace_2(desc, depth, orig, dir, t, ob)
   end

and trace_2 (desc, depth: int, orig, dir, t, obj) =
   let
      val x = add_scaled (orig, t, dir)
   in
      case obj of
	 SObj (kind, f, m) =>
	    let
	       val x' = vmul (m, x)
	       val (n, ks, kd, cr, cg, cb) =
		  (case !f of
		      Unopt g =>
			 (* First we check whether the function would fail *)
			 let
			    val res = apply_surface_fun(g, texture_coord(kind, x'))
			    fun stuck() = f := Opt (!inline_closure g)
			 in
			    (* Then, we check whether it is a constant function *)
			    ((ignore (apply_surface_fun(g, 
				      [VInt 0, VInt 0, VFloat 0.0]))
			      ; f := Cst res)
			     handle Stuck_computation _ => stuck()
				  | Stuck_computation' => stuck())
			    ; res
			 end
		    | Opt g =>
			 apply_surface_fun (g, texture_coord (kind, x'))
		    | Cst res =>
			 res)
               val nm = normal (kind, m, x', x)
	       val p = prod (dir, nm)
	       val nm = if p > 0.0 then neg nm else nm
	       val p = ~(Real.abs p)
	       (* Ambient composant *)
	       val (ar, ag, ab) = #amb desc
	       val r = ref (kd * ar)
	       val g = ref (kd * ag)
	       val b = ref (kd * ab)
		  (* Lights *)
	       val lights = #lights desc
	       val _ =
		  for(0, Array.length lights - 1, fn i =>
		      case (Array.sub(lights, i)) of
			 Light (ldir, (lr, lg, lb)) =>
			    let
			       val p' = prod (ldir, nm)
			    in
			       if p' > 0.0 andalso visible (desc, x, ldir, false)
				  then
				     let
					val int =
					   if ks > epsilon then
					      kd * p' +
					      ks * prod (normalize
							 (sub (ldir, dir)),
							 nm) ** n
					   else
					      kd * p'
				     in
					r := !r + int * lr;
					g := !g + int * lg;
					b := !b + int * lb
				     end
			       else ()
			    end
		       | PtLight (src, (lr, lg, lb)) =>
			    let
			       val ldir = sub (src, x)
			       val ldir' = normalize ldir
			       val p' = prod (ldir', nm)
			    in
			       if p' > 0.0 andalso visible(desc, x, ldir, true)
				  then
				     let
					val int =
					   if ks > epsilon
					      then
						 kd * p' +
						 ks * prod (normalize (sub (ldir', dir)),
							    nm) ** n
					   else
					      kd * p'
					val int = 100.0 * int / (99.0 + square ldir)
				     in
					r := !r + int * lr;
					g := !g + int * lg;
					b := !b + int * lb
				     end
			       else ()
			    end
		       | StLight (src, maindir, (lr, lg, lb), cutoff, exp) =>
			    let
			       val ldir = sub (src, x)
			       val ldir' = normalize ldir
			       val p' = prod (ldir', nm)
			       val p'' = prod (ldir', maindir)
			    in
			       if p' > 0.0 andalso p'' > cutoff
				  andalso visible(desc, x, ldir, true)
				  then
				     let
					val int =
					   if ks > epsilon
					      then
						 kd * p' +
						 ks * prod (normalize (sub(ldir', dir)),
							    nm) ** n
					   else
					      kd * p'
					val int =
					   100.0 * int / (99.0 + square ldir) *
					   (p'' ** exp)
				     in
					r := !r + int * lr;
					g := !g + int * lg;
					b := !b + int * lb
				     end
			       else ()
			    end)
	       val _ =
		  (* Reflexion *)
		  if ks > epsilon  andalso depth > 0
		     then
			let
			   val dir' = add_scaled (dir, ~2.0 * p, nm)
			   val (r', g', b') = trace(desc, depth - 1, x, dir')
			in
			   r := !r + ks * r';
			   g := !g + ks * g';
			   b := !b + ks * b'
			end
		  else ()
	    in (!r * cr, !g * cg, !b * cb)
	    end
       | _ => raise(Fail "assert false")
   end

fun conv c : int =
   let
      val i = Real.trunc (c * 256.0)
   in
      if i < 0 then 0 else
	 if i >= 256 then 255 else
	    i
   end

fun f (amb, lights, obj, depth: int, fov, wid, ht, file) =
   let
      val scene = intern_obj(Matrix.identity, Matrix.identity, 1.0, true, obj)
      val scene = optimize scene
      val img = Image.init {name=file,width=wid,height=ht}
      val orig = ( 0.0, 0.0, ~1.0, 1.0 )
      val width = 2.0 * dtan (0.5 * fov)
      val delta = width / Real.fromInt wid
      val x0 = ~ width / 2.0
      val y0 = delta * Real.fromInt ht / 2.0
      val desc = { amb = amb, lights = intern_lights lights, scene = scene }
   in
      for(0, ht - 1, fn j =>
	  for(0, wid - 1, fn i =>
	      let
		 val dir =
		    (x0 + (Real.fromInt i + 0.5) * delta,
		     y0 - (Real.fromInt j + 0.5) * delta,
		     1.0,
		     0.0)
		 val (r, g, b) = trace(desc, depth, orig, dir)
	      in
		 Image.set (img, {x=i,y=j}, {red=conv r,green=conv g,blue=conv b})
	      end))
      ; Image.commit(img)
   end

end
