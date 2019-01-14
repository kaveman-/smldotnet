structure Matrix :>  Matrix =
struct

open Lib
open MathLib

type t = Real.real array
type v = Real.real * Real.real * Real.real * Real.real

(**** Basic matrices ****)

val identity =
   Array.fromList[1.0, 0.0, 0.0, 0.0,
		  0.0, 1.0, 0.0, 0.0,
		  0.0, 0.0, 1.0, 0.0,
		  0.0, 0.0, 0.0, 1.0]

fun translate(x, y, z) =
   Array.fromList[1.0, 0.0, 0.0, ~ x,
		  0.0, 1.0, 0.0, ~ y,
		  0.0, 0.0, 1.0, ~ z,
		  0.0, 0.0, 0.0, 1.0]

fun unscale(x, y, z) =
   Array.fromList[ x,  0.0, 0.0, 0.0,
		  0.0, y,  0.0, 0.0,
		  0.0, 0.0, z,  0.0,
		  0.0, 0.0, 0.0, 1.0]
  
fun scale(x, y, z) = unscale (1.0 / x, 1.0 / y, 1.0 / z)

fun unuscale s = unscale (s, s, s)

fun uscale s = scale (s, s, s)

fun rotatex t =
  let
     val co = dcos t
     val si = dsin t
  in
     Array.fromList[ 1.0,   0.0,  0.0, 0.0,
		    0.0,   co,  si, 0.0,
		    0.0, ~ si, co, 0.0,
		    0.0,   0.0,  0.0, 1.0 ]
  end

fun rotatey t =
  let
     val co = dcos t
     val si = dsin t
  in
     Array.fromList[ co, 0.0, ~ si, 0.0,
		    0.0, 1.0,   0.0,  0.0,
		    si, 0.0,   co,  0.0,
		    0.0, 0.0,   0.0,  1.0 ]
  end

fun rotatez t =
  let
     val co = dcos t
     val si = dsin t
  in
     Array.fromList[   co,  si, 0.0, 0.0,
		    ~ si, co, 0.0, 0.0,
		    0.0,  0.0, 1.0, 0.0,
		    0.0,  0.0, 0.0, 1.0 ]
  end

(*** Operations on matrices ***)

fun get (m : t, i, j) = Array.sub (m, i * 4 + j)
fun set (m : t, i, j, v) = Array.update (m, i * 4 + j, v)

fun mul (m, m') =
  let
     val m'' = Array.array (16, 0.0)
  in
     for(0, 3, fn i =>
	 for(0, 3, fn j => let
	    fun lp (4, s) = s
	      | lp (k, s) = lp (k+1, s + get(m, i, k) * get(m', k, j))
	    in
	      set(m'', i, j, lp(0, 0.0))
	    end))
     ; m''
  end

fun transpose m =
  let val m' = Array.array (16, 0.0)
  in for(0, 3, fn i =>
	 for(0, 3, fn j =>
	     set (m', i, j, get (m, j, i))))
     ; m'
  end

fun vmul (m, (x, y, z, t)) =
   (x * get(m, 0, 0) + y * get(m, 0, 1) + z * get(m, 0, 2) + t * get(m, 0, 3),
    x * get(m, 1, 0) + y * get(m, 1, 1) + z * get(m, 1, 2) + t * get(m, 1, 3),
    x * get(m, 2, 0) + y * get(m, 2, 1) + z * get(m, 2, 2) + t * get(m, 2, 3),
    x * get(m, 3, 0) + y * get(m, 3, 1) + z * get(m, 3, 2) + t * get(m, 3, 3))

fun add_scaled (x: v, t, v: v) : v =
   ( #1 x + t * #1 v,
     #2 x + t * #2 v,
     #3 x + t * #3 v,
     #4 x + t * #4 v )

fun add (x: v, y: v) : v =
   ( #1 x + #1 y,
     #2 x + #2 y,
     #3 x + #3 y,
     #4 x + #4 y )

fun sub (x: v, y: v) : v =
   (#1 x - #1 y,
    #2 x - #2 y,
    #3 x - #3 y,
    #4 x - #4 y)

fun prod (x: v, y: v) : real =
   #1 x * #1 y + #2 x * #2 y + #3 x * #3 y + #4 x * #4 y

fun square (vx, vy, vz, vt) : real =
   vx * vx + vy * vy + vz * vz + vt * vt

fun normalize (x: v): v =
  let
     val nx = Math.sqrt (prod (x, x))
  in
     (#1 x / nx,
      #2 x / nx,
      #3 x / nx,
      #4 x / nx)
  end

fun neg (x: v) : v =
   (~(#1 x),
    ~(#2 x),
    ~(#3 x),
    ~(#4 x))

end
