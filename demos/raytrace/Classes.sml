structure Classes = struct

    (* ImageClass defines an object-oriented interface to functional Ppm Images *)
    (* ImageClass is sub-classed, and its methods  are overriden, by the C# class DisplayImageClass *)

    _classtype ImageClass (name:string option,w:int,h:int) 
    with 
    local
	val img = Ppm.init{name=valOf name,width=w,height=h}
    in
	Width () = Ppm.width img
	and Height () = Ppm.height img
	    
	and Set(x:int, y:int, r:int, g:int, b:int) = Ppm.set(img,{x=x,y=y},{red=r,green=r,blue=b})
	    
	and Commit () = Ppm.commit img
    end

    (* A factory object can encapsulate an ImageClass constructor, but
       objects of its derived classes can encapsulate constructors of ImageClass and any of its sub-classes.

       The C# client determines the actual FactoryObject used by the Server by updating the 
       SML value Image.current_factory.
    *)

    _classtype FactoryClass () 
    with
	MakeImage(nameopt,x:int,y:int) = SOME (ImageClass(nameopt,x,y))
    end;

end








