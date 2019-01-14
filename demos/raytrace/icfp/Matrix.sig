signature Matrix =
   sig
      

      (**** Matrix arithmetic ****)

      type t = Real.real array (* 4-dimension matrix *)
      type v = Real.real * Real.real * Real.real * Real.real (* 4-dimension vector *)

      (* Basic matrices *)
      val identity : t
      val translate : (*x:*)Real.real * (*y:*)Real.real * (*z:*)Real.real -> t
      val scale : (*x:*)Real.real * (*y:*)Real.real * (*z:*)Real.real -> t
      val uscale : Real.real -> t
      val unscale : (*x:*)Real.real * (*y:*)Real.real * (*z:*)Real.real -> t
      val unuscale : Real.real -> t
      val rotatex : Real.real -> t
      val rotatey : Real.real -> t
      val rotatez : Real.real -> t

      (* Operations on matrices *)
      val mul : t * t -> t
      val vmul : t * v -> v
      val transpose : t -> t

      val add_scaled : v * Real.real * v -> v
      val add : v * v -> v
      val sub : v * v -> v
      val prod : v * v -> Real.real
      val square : v -> Real.real
      val normalize : v -> v
      val neg : v -> v
   end
