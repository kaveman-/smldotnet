(*======================================================================*)
(* Turn a tree layout into a sequence of draw/label commands		*)
(*======================================================================*)
structure Draw :> Draw =
struct

local open Tree in

type 'a Interface =
{
  line : ((int*int) * (int*int)) -> unit,
  label : 'a * (int*int) -> Tree.Extent,
  dropbelow : int,    (* Line drop from below text *)
  dropabove : int     (* Line drop from above text *)
}

fun drawLine 
  ({line, label, dropbelow, dropabove} : 'a Interface) ((x1,y1), (x2,y2)) =
  line ((Real.floor x1, y1), (Real.floor x2, y2))

fun drawLabel 
  ({line, label, dropbelow, dropabove} : 'a Interface) (s, (x,y)) =
  let
    val { left, right, height } = label (s, (Real.floor x, y))
  in
    ((Real.floor (x+left), y), (Real.ceil(right-left), height))
  end

(*----------------------------------------------------------------------*)
(* Extract positions of subtrees					*)
(*----------------------------------------------------------------------*)
fun pickx (Node((label,x),subtrees)) = x
fun eldestx   subtrees = pickx (hd subtrees)
fun youngestx subtrees = pickx (hd (rev subtrees))

(*----------------------------------------------------------------------*)
(* Generate the commands for drawing a tree, starting at level y	*)
(*----------------------------------------------------------------------*)
(* Line is drawn directly from parent to child *)
fun draw1 (interface as { line, label, dropbelow, dropabove }) 
          (xabs:real,y, {left,right,height}::e) (Node((s,xrel), subtrees)) =
  let 
    val x = xabs + xrel     
    val box = drawLabel interface (s, (x,y))
    val y' = y + height + dropbelow + dropabove
  in
    app 
    (fn t => drawLine interface ((x,y+height), (pickx t + x, y'))) subtrees;

    (s,box) :: List.concat (map (draw1 interface (x, y', e)) subtrees)
  end
  
(* Family-tree style with horizontals and verticals *)
fun draw2 (interface as { line, label, dropbelow, dropabove }) 
          (xabs:real,y, {left,right,height}::e) (Node((s,xrel), subtrees)) = 
  let 
    val x = xabs + xrel
    val box = drawLabel interface (s, (x,y))
    val y' = y + height + dropbelow
  in
    (s, box) ::
    (if null subtrees then []
    else
      (drawLine interface ((x + eldestx subtrees, y'),
                           (x + youngestx subtrees, y'));
      drawLine interface ((x, y + height), (x, y'));
      app (fn t => 
        let val x' = x + pickx t
        in drawLine interface ((x', y'), (x', y' + dropabove))
        end) subtrees;

      List.concat (map (draw2 interface (x, y'+dropabove, e)) subtrees)))
  end

end

end

