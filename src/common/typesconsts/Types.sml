(* The Types structure defines the possible Java Types. *)
structure Types:>TYPES=
struct
   datatype base_type=BOOLEAN|BYTE|CHAR|SHORT|INT|LONG|FLOAT|DOUBLE|
		      CLASS of ClassHandle.Handle
   datatype java_type=F of int*base_type
   (* F(0,t) is a t.  F(n,t) for n>=1 is an n-dimensional array of t's. *)
   type java_void_type=java_type option
   (* NONE means VOID, otherwise the same as java_type. *)
   fun base_type_equal(bt1,bt2)=
      (case (bt1,bt2) of
         (BOOLEAN,BOOLEAN)=>true
      |  (BYTE,BYTE)=>true
      |  (CHAR,CHAR)=>true
      |  (SHORT,SHORT)=>true
      |  (INT,INT)=>true
      |  (LONG,LONG)=>true
      |  (FLOAT,FLOAT)=>true
      |  (DOUBLE,DOUBLE)=>true
      |  (CLASS c1,CLASS c2)=>ClassHandle.equal(c1,c2)
      |  (_,_)=>false
      )
   fun java_type_equal(F(n1,bt1),F(n2,bt2))=
      n1=n2 andalso base_type_equal(bt1,bt2)

   fun number_base_type x=
   (* give a number to the base type; CLASSes all get 0. *)
      (case x of
         CLASS _ => 0
      |  BOOLEAN => 1
      |  BYTE => 2
      |  CHAR => 3
      |  SHORT => 4
      |  INT => 5
      |  LONG => 6
      |  FLOAT => 7
      |  DOUBLE => 8
      )

   fun base_type_compare(x,y)=
   let
      val xn=number_base_type x
      val yn=number_base_type y
   in
      (case Int.compare(xn,yn) of
         LESS => LESS
      |  GREATER => GREATER
      |  EQUAL =>
            if xn=0
            then
            (case (x,y) of
               (CLASS xclass,CLASS yclass) =>
                  UString.compare
                    (ClassHandle.name xclass,ClassHandle.name yclass)
               )
            else EQUAL
      )
   end

   structure BaseKey=
   struct
      type ord_key=base_type
      val compare=base_type_compare
   end

   structure Key=
   struct
      type ord_key=java_type

      fun compare(F(nx,btx),F(ny,bty))=
      (case base_type_compare(btx,bty) of
         LESS => LESS
      |  GREATER => GREATER
      |  EQUAL => Int.compare(nx,ny)
      )
   end

   fun java_type_size jt=
   (* java_type_size returns the number of words required to store an item
      of this type in locals or on the stack. *)
      (case jt of
         F(0,LONG) => 2
      |  F(0,DOUBLE) => 2
      |  _ => 1
      )

   fun java_void_type_size jvt=
      (case jvt of
          NONE => 0
      |   SOME jt => java_type_size jt
      )

   fun drop_dim(F(n,bt))=
      if n<=0
      then 
         raise Fail "drop_dim on non-array type!"
      else
         F(n-1,bt)   

(*
   fun s2base_type s=
   (case s of
      "bool" => SOME BOOLEAN
   |  "byte" => SOME BYTE
   |  "char" => SOME CHAR
   |  "short" => SOME SHORT
   |  "int" => SOME INT
   |  "long" => SOME LONG
   |  "float" => SOME FLOAT
   |  "double" => SOME DOUBLE
   |  classname => SOME(CLASS(ClassHandle.unknown(
         UString.fromString classname)))
   )
*)

   fun base_type2s bt=
   (case bt of
      BOOLEAN => "bool"
   |  BYTE => "byte"
   |  CHAR => "char"
   |  SHORT => "short"
   |  INT => "int"
   |  LONG => "long"
   |  FLOAT => "float"
   |  DOUBLE => "double"
   |  CLASS h => UString.toMLString(ClassHandle.name h)
   )

   (* the XXX_toString functions are intended for debugging purposes only and
      should not be used in production code or in any other way relied on. *)

   fun base_type_toString(bt)=
      (case bt of
         BOOLEAN => "bool"
      |  BYTE => "byte"
      |  CHAR => "char"
      |  SHORT => "short"
      |  INT => "int"
      |  LONG => "long"
      |  FLOAT => "float"
      |  DOUBLE => "double"
      |  CLASS h=>
            UString.toMLString(ClassHandle.name h)
      )

   fun java_type_toString(F(n,bt))=
      String.concat(base_type_toString(bt)::List.tabulate(n,fn i=>"[]"))

   fun java_void_type_toString(jvt)=
      (case jvt of
         SOME jt=>java_type_toString jt
      |  NONE=>"VOID"
      )


   (* Here are some classification functions *)
   fun isnum(F(0,INT))=true
   |   isnum(F(0,LONG))=true
   |   isnum(F(0,FLOAT))=true
   |   isnum(F(0,DOUBLE))=true
   |   isnum(_)=false
   

   fun bt_isint INT=true
   |   bt_isint _ = false

   fun isint(F(0,INT))=true
   |   isint _=false
   
   fun islong(F(0,LONG))=true
   |   islong _=false

   fun bt_issmall bt=
   (case bt of 
      BOOLEAN => true
   |  BYTE => true
   |  CHAR => true
   |  SHORT => true
   |  _ => false
   )

   fun issmall(F(0,bt))=bt_issmall bt
   |   issmall _ = false
   
   fun iscmp(F(0,bt))=
      (case bt of
         LONG => true
      |  FLOAT => true
      |  DOUBLE => true
      |  _ => false
      )
   |  iscmp _ = false

   fun isref(F(0,CLASS _))=true
   |   isref(F(n,_))=n>0
   
   fun islogical(F(0,INT))=true
   |   islogical(F(0,LONG))=true
   |   islogical(_)=false
   
   fun isloadable(F(0,INT))=true
   |   isloadable(F(0,LONG))=true
   |   isloadable(F(0,FLOAT))=true
   |   isloadable(F(0,DOUBLE))=true
   |   isloadable(F(0,CLASS _))=true
   |   isloadable(F(n,_))=n>=1

   fun isreal(F(0,FLOAT))=true
   |   isreal(F(0,DOUBLE))=true
   |   isreal _=false

   fun is2(F(0,LONG))=true
   |   is2(F(0,DOUBLE))=true
   |   is2 _ = false
      
   fun widen jt=
      if issmall jt then F(0,INT) else jt
end



