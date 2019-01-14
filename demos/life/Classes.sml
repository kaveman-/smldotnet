structure Classes = struct

    (* ImageClass defines an object-oriented interface to a bitmap *) 

    (* ImageClass is sub-classed, and its methods are
       overriden, by the C# class DisplayImageClass *)

    _classtype ImageClass () 
    with 
	    Set(x:int, y:int) = ()
	and Clear(x:int, y:int) = ()
	and Size() = 0
    end

    (* A factory object can encapsulate an ImageClass constructor, but
       objects of its derived classes can encapsulate constructors of
       ImageClass and any of its sub-classes.

       The C# client determines the actual FactoryObject used by the
       Server by updating the SML value Image.current_factory.  *)

    _classtype FactoryClass () 
    with
	MakeImage() = SOME (ImageClass())
    end;

end








