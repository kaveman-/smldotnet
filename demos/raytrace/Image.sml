(* Image defines a functional interface to the class Classes.ImageClass *)
structure Image =
struct

    val current_factory = ref (SOME (Classes.FactoryClass()))

    type image = Classes.ImageClass
    
    fun init {name,width,height} = 
	let val SOME fo = !current_factory
	in
	    valOf(fo.#MakeImage(SOME name,width,height))
	end	

    fun width (i:image) = i.#Width()
	
    fun height (i:image) = i.#Height()
    
    fun set (i:image, {x,y}, {red,green,blue}) = i.#Set(x,y,red,green,blue)

    fun commit (i:image) = i.#Commit()
	
end

































































































