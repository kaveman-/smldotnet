(* Coloured points in SML.NET *)
structure CPDemo =
struct
  _classtype Point(xinit, yinit) 
  with local
    val x = ref xinit
    val y = ref yinit
  in
        getX () = !x
    and getY () = !y 
    and move (xinc,yinc) = (x := !x+xinc; y := !y+yinc)
    and moveHoriz xinc = this.#move (xinc, 0)
    and moveVert yinc = this.#move (0, yinc)
  end

  _classtype ColouredPoint(x, y, c) : Point(x, y)
  with
        getColour () = c : System.Drawing.Color
    and move (xinc, yinc) = this.##move (xinc*2, yinc*2)
  end
end
