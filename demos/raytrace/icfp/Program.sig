signature Program =
   sig

      (**** Basic types: programs, values, ... ****)

      datatype k =
	 Acos | Addi | Addf | Apply | Asin | Clampf | Cone | Cos | Cube
       | Cylinder | Difference | Divi | Divf | Eqi | Eqf | Floor | Frac
       | Get | Getx | Gety | Getz | If | Intersect | Length | Lessi | Lessf
       | Light | Modi | Muli | Mulf | Negi | Negf | Plane | Point
       | Pointlight | Real | Render | Rotatex | Rotatey | Rotatez | Scale
       | Sin | Sphere | Spotlight | Sqrt | Subi | Subf | Translate | Union
       | Uscale

      (* Program tokens *)
      datatype t =
	 Fun of t list
       | Arr of t list
       | Ident of string
       | Binder of string
       | Int of int
       | Float of Real.real
       | Bool of bool
       | String of string
       | Prim of k

      (* internal representation of program tokens *)
      datatype t' =
	 Fun' of t' list
       | Arr' of t' list
       | Ident' of int (* index to environment stack *)
       | Binder'
       (*
     | Int' of int
     | Float' of float
     | Bool' of bool
     | String' of string
	*)
       | Prim' of k
       | Val' of v (* inlined value *)

      (* Values *)
      and v =
	 VInt of int
	| VFloat of Real.real
	| VBool of bool
	| VStr of string
	| VClos of v list * t' list
	| VFun of (v list -> v list) 
	| VArr of v array
	| VPoint of v * v * v 
	| VObj of obj
	| VLight of v * v
	| VPtLight of v * v
	| VStLight of v * v * v * v * v

      and obj =
	 OObj of kind * closure ref
	| OTransform of
	  obj *
	  Matrix.t *     (* World to object *)
	  Matrix.t *     (* Object to world *)
	  Real.real *        (* Scale factor *)
	  bool           (* Isometry? *)
	| OUnion of obj * obj
	| OInter of obj * obj
	| ODiff of obj * obj

      and kind =
	 OSphere
	| OCube
	| OCylind
	| OCone
	| OPlane

      and closure =
 	 Unopt of v (* Unoptimized function *)
	| Opt of v
	| Cst of (Real.real * Real.real * Real.real * Real.real * Real.real * Real.real)

      (* Translation of an identifier *)
      val translate : string -> t


      exception Stuck_computation of v list * v list * t' list
      exception Stuck_computation' (* for compiler *)

      val read: TextIO.instream -> t list
   end
