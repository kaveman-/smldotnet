structure DynIntArray:>DARRAY=
struct
   type array=int array ref
  
   val default= 0

   fun array n = ref(Array.array(n,default))

   fun clear a = a := Array.array(Array.length (!a),default)

   fun sub(ref arr,i)=
      if i<0
      then raise Subscript
      else if i<Array.length arr 
      then Array.sub(arr,i) 
      else default

   fun update(aref as ref arr,i,value) =
       if i<0 then raise Subscript
       else if i<Array.length arr 
       then 
	   Array.update(arr,i,value)
       else	 
           let
               val oldlen=Array.length arr
               val newlen=Int.max(i+1,2*oldlen)
               val newarr=Array.array(newlen,default)
           in
               Array.copy{dst=newarr, src=arr, di=0};
               aref:=newarr;
               Array.update(newarr,i,value)
           end
	       
  fun bound a = Array.length (!a) - 1
end

