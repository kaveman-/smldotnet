structure MathLib =
struct
local
open Real
open Math
in
val epsilon = 1E~5

val dtr = acos (~1.0) / 180.0
val rtd = 180.0 / acos (~1.0)

fun dcos t = cos (t * dtr): Real.real 
fun dsin t = sin (t * dtr): Real.real
fun dtan t = tan (t * dtr): Real.real
fun dacos x = rtd * acos x: Real.real

fun max_real (x, y : Real.real) = if x >= y then x else y
end
end
