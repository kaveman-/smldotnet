structure Program : Program =
struct

open Lib

datatype k =
    Acos | Addi | Addf | Apply | Asin | Clampf | Cone | Cos | Cube
  | Cylinder | Difference | Divi | Divf | Eqi | Eqf | Floor | Frac
  | Get | Getx | Gety | Getz | If | Intersect | Length | Lessi | Lessf
  | Light | Modi | Muli | Mulf | Negi | Negf | Plane | Point
  | Pointlight | Real | Render | Rotatex | Rotatey | Rotatez | Scale
  | Sin | Sphere | Spotlight | Sqrt | Subi | Subf | Translate | Union
  | Uscale

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

datatype t' =
    Fun' of t' list
  | Arr' of t' list
  | Ident' of int (* index to environment stack *)
  | Binder'
  | Prim' of k
  | Val' of v (* inlined value *)

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
    Unopt of v
  | Opt of v
  | Cst of (Real.real * Real.real * Real.real * Real.real * Real.real * Real.real)

fun create_hashtables size init =
   let
      val tbl: (string, t) Hashtbl.t = Hashtbl.create size 
   in
      List.app (fn (key, data) => Hashtbl.add tbl key data) init;
      tbl 
   end

val keywords =
  create_hashtables 101
(* Booleans are either the literal true or the literal false. *)
    [ ("true", Bool true),
      ("false", Bool false),
(* Operators (see appendix) *)
      ("acos", Prim Acos),
      ("addi", Prim Addi),
      ("addf", Prim Addf),
      ("apply", Prim Apply),
      ("asin", Prim Asin),
      ("clampf", Prim Clampf),
      ("cone", Prim Cone),
      ("cos", Prim Cos),
      ("cube", Prim Cube),
      ("cylinder", Prim Cylinder),
      ("difference", Prim Difference),
      ("divi", Prim Divi),
      ("divf", Prim Divf),
      ("eqi", Prim Eqi),
      ("eqf", Prim Eqf),
      ("floor", Prim Floor),
      ("frac", Prim Frac),
      ("get", Prim Get),
      ("getx", Prim Getx),
      ("gety", Prim Gety),
      ("getz", Prim Getz),
      ("if", Prim If),
      ("intersect", Prim Intersect),
      ("length", Prim Length),
      ("lessi", Prim Lessi),
      ("lessf", Prim Lessf),
      ("light", Prim Light),
      ("modi", Prim Modi),
      ("muli", Prim Muli),
      ("mulf", Prim Mulf),
      ("negi", Prim Negi),
      ("negf", Prim Negf),
      ("plane", Prim Plane),
      ("point", Prim Point),
      ("pointlight", Prim Pointlight),
      ("real", Prim Real),
      ("render", Prim Render),
      ("rotatex", Prim Rotatex),
      ("rotatey", Prim Rotatey),
      ("rotatez", Prim Rotatez),
      ("scale", Prim Scale),
      ("sin", Prim Sin),
      ("sphere", Prim Sphere),
      ("spotlight", Prim Spotlight),
      ("sqrt", Prim Sqrt),
      ("subi", Prim Subi),
      ("subf", Prim Subf),
      ("translate", Prim Translate),
      ("union", Prim Union),
      ("uscale", Prim Uscale)]

fun translate i =
   Hashtbl.find keywords i
   handle Not_found => Ident i

exception Stuck_computation of v list * v list * t' list
exception Stuck_computation' (* for compiler *)

structure LexToken = LexToken()
structure Lex = Lex(structure Token = LexToken)

fun read(ins: TextIO.instream): t list =
   let
      val lex: unit -> LexToken.t =
	 Lex.makeLexer(fn n => TextIO.inputN(ins, n))()
      local
	 val next: LexToken.t option ref = ref NONE
      in
	 fun token(): LexToken.t =
	    case !next of
	       NONE => lex()
	     | SOME t => (next := NONE; t)
	 fun save(t: LexToken.t): unit =
	    next := SOME t
      end
      fun bad() = failwith "invalid input"
      fun many(done: LexToken.t -> bool): t list =
	 let
	    fun loop(ac: t list) =
	       case one() of
		  NONE => if done(token())
			     then rev ac
			  else bad()
		| SOME t => loop(t :: ac)
	 in loop []
	 end
      and one(): t option =
	 let fun tok t = SOME t
	 in case token() of
	    LexToken.Binder x => tok(Binder x)
	  | LexToken.Bool b => tok(Bool b)
	  | LexToken.Identifier x => tok(translate x)
	  | LexToken.Int i => tok(Int i)
	  | LexToken.Lbrace =>
	       SOME(Fun(many(fn LexToken.Rbrace => true | _ => false)))
	  | LexToken.Lbracket =>
	       SOME(Arr(many(fn LexToken.Rbracket => true | _ =>false)))
	  | LexToken.Real r => tok(Float r)
	  | LexToken.String s => tok(String s)
	  | t => (save t; NONE)
	 end
   in many(fn LexToken.Eof => true | _ => false)
   end

end
