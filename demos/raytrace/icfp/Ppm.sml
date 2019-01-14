structure Ppm : Image =
struct
   
structure Array = Word8Array
structure Word = Word8
   
type image = (string * Array.array * int)

fun set ((_,img, width),{x,y}, {red,green, blue}) =
  let val i = ((width * y) + x) * 3
  in   Array.update(img, i, Word.fromInt red)
     ; Array.update(img, i + 1, Word.fromInt green)
     ; Array.update(img, i + 2, Word.fromInt blue)
  end

fun init {name,width,height} =
   (name,Array.array(height * width * 3, 0w0), width)

fun width (_,_,width) = width
fun height (_,img,width) = (Array.length img) div width div 3

(* produce an ASCII format .ppm file *)
fun commit ((file,img,width)) =
  let
     val _ = UTC.pause_clock()
     val sz = Array.length img
     val height = sz div 3 div width
     val path = 
         case !Config.ppmDir of 
	    NONE => file 
          | SOME  dir => OS.Path.joinDirFile {dir = dir, file = file}  
     val f = TextIO.openOut(path)
     
  in  
      TextIO.output (f, "P3\n# PL Club - ported to SML\n")
    ; TextIO.output (f, concat[Int.toString width, " ",
			       Int.toString height, "\n255\n"])
    ; Array.foldl (fn (w,i) => (TextIO.output(f,Int.toString (Word.toInt w));
				TextIO.output(f, " ");
				if i=16 then (* linewrap *)
                                   (TextIO.output(f,"\n");0) 
				else i+1)) 0 img
    ; TextIO.closeOut f;
    UTC.resume_clock()
  end;
end

