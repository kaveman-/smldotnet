signature Image =
   sig

      type image

      val init : {name:string, width:int, height: int} -> image
      val commit : image -> unit

      val width : image -> int
      val height : image -> int

      val set : image * {x:int,y:int} * {red:int,green:int,blue:int} -> unit
   end
