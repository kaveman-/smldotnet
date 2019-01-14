structure TreeDraw :> TreeDraw =
struct

open System.Drawing

(* The font for tree labels *)
val font = Font("Lucida Sans Unicode", 10.0 : Real32.real)
val fontheight = font.#get_Height()
val quarterheight = fontheight div 4

val i2f = Real32.fromInt
val f2d = Real32.toLarge
val gap = i2f fontheight / 3.0
val genwidth = fontheight * 6

datatype Orientation = Portrait | Landscape

(*----------------------------------------------------------------------*)
(* From a graphics object determine appropriate functions for     	*)
(* drawing lines and labels, and necessary dimensions.                  *)
(*----------------------------------------------------------------------*)
fun makeInterfaceL (c : System.Windows.Forms.UserControl, rect : Rectangle) =
{ 
  label = 
  fn (s:string,(x,y)) => 
  let
    val SOME g = c.#CreateGraphics()
    val sizef = g.#MeasureString(s, font)
    val brush = SolidBrush(Color.get_Black())
    val rw = sizef.#get_Width()
    val halfwidth = rw / 2.0
  in          
    g.#DrawString(s, font, brush, i2f x - halfwidth, i2f y);
    g.#Dispose(); 
    brush.#Dispose();
    { left = ~(f2d halfwidth), 
      right = f2d halfwidth, 
      height = fontheight }
  end,

  line = 
  fn ((x1,y1),(x2,y2)) => 
  let val SOME g = c.#CreateGraphics()
      val pen = Pen(Color.get_Maroon()) 
  in 
      g.#DrawLine(pen,i2f x1,i2f y1,i2f x2,i2f y2);
      g.#Dispose();
      pen.#Dispose()
  end,

  dropabove = quarterheight,
  dropbelow = quarterheight
}

fun makeExtentL (c:System.Windows.Forms.UserControl) (s:string) = 
let 
  val SOME g = c.#CreateGraphics()
  val sizef = g.#MeasureString(s, font)
  val width = sizef.#get_Width() + gap
  val halfwidth = f2d width/2.0
in
  g.#Dispose(); 
  { left = ~halfwidth, right = halfwidth, 
    height = fontheight + quarterheight*2  }
end

fun makeInterfaceP (c : System.Windows.Forms.UserControl, rect : Rectangle) =
{ 
  label = 
  fn (s:string,(x,y)) => 
  let
    val SOME g = c.#CreateGraphics()
    val sizef = g.#MeasureString(s, font, genwidth)
    val brush = SolidBrush(Color.get_Black())
    val height = sizef.#get_Height()
    val halfheight = height / 2.0
    val srect = RectangleF(i2f y, i2f(x-quarterheight*2), i2f genwidth, height)
  in          
    g.#DrawString(s, font, brush, srect);
    g.#Dispose(); 
    brush.#Dispose();
    { left = ~(f2d halfheight),
      right = f2d halfheight,
      height = genwidth }
  end,

  line = 
  fn ((x1,y1),(x2,y2)) => 
  let val SOME g = c.#CreateGraphics()
      val pen = Pen(Color.get_Maroon()) 
  in  g.#DrawLine(pen,i2f y1,i2f x1,i2f y2,i2f x2);
      g.#Dispose();
      pen.#Dispose()
  end,

  dropabove = quarterheight,
  dropbelow = quarterheight
}

fun makeExtentP (c:System.Windows.Forms.UserControl) (s:string) = 
let 
  val SOME g = c.#CreateGraphics()
  val sizef = g.#MeasureString(s, font, genwidth)
  val height = sizef.#get_Height()
  val halfheight = height/2.0
in  g.#Dispose(); 
  { left = ~(f2d halfheight),
    right = f2d halfheight,
    height = genwidth + quarterheight*2 }
end

fun makeInterface Landscape = makeInterfaceL
  | makeInterface Portrait = makeInterfaceP

fun makeExtent Landscape = makeExtentL
  | makeExtent Portrait = makeExtentP

fun extentToSize Portrait { left, right, height } =
    Size(height + quarterheight*2+genwidth, Real.round(right-left))
  | extentToSize Landscape { left, right, height } =
    Size(Real.round(right-left), height+quarterheight*2+genwidth)

end







