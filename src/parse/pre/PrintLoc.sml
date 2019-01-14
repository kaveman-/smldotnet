(* PrintLoc:>PRINTLOC pretty-prints syntax positions and locations
   to strings 
   *)
structure PrintLoc:>PRINTLOC=
struct
   val SOME im=Int.maxInt (* notional position of EOF *)
   
   datatype position=
      LC of {line:int,col:int}
   |  EOF (* end of file *)
   
   fun i2pos(s,i)=
      if i=im 
      then
         EOF
      else
         LC(SourceMap.decode(s,i))

   fun pos2string(LC{line,col})=
      (String.concat[
         Int.toString line,
         ".",
         Int.toString col
         ])
   |   pos2string EOF= "EOF"
   
   fun simplepospos2string(x,y)=
      String.concat[
         pos2string x,
         "-",
         pos2string y
         ]
   
   fun pospos2string(x:position,y:position)=
     (case (x,y) of
         (LC{line=line1,col=col1},LC{line=line2,col=col2})=>
            if line1=line2 
            then
               String.concat[
                  Int.toString line1,
                  ".",
                  Int.toString col1,
                  "-",
                  Int.toString col2
                  ]
            else
               simplepospos2string(x,y)
      |  _ => simplepospos2string(x,y)
      )

   fun position2string(s,p)=pos2string(i2pos(s,p))

   fun location2string(s,{left,right})=
      if left>=right
      then
         pos2string(i2pos(s,left))
      else
         pospos2string(i2pos(s,left),i2pos(s,right))
end
