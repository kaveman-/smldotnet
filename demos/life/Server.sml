(* Game of life *)
(* Based on code from Chris Reade's book:
   "Elements of Functions Programming", Addison-Wesley Publishing, 1989.
   Reproduced with kind permission of the author.
*)
structure Server =
struct
fun life (SOME (factory:Classes.FactoryClass)) = 
let
    val SOME image = factory.#MakeImage();

    val size = image.#Size();

    fun accumulate f = let
          fun foldf a [] = a
            | foldf a (b::x) = foldf (f a b) x
          in
            foldf
          end

    fun filter p = let
          fun consifp x a = if p a then a::x else x
          in
            rev o accumulate consifp []
          end

    fun exists p = let fun existsp [] = false
                     | existsp (a::x) = if p a then true else existsp x
                in existsp end

    fun equal a b = (a  = b)

    fun member x a = exists (equal a) x

    fun C f x y = f y x

    fun cons a x = a::x

    fun revonto x = accumulate (C cons) x

    fun length x = let fun count n a = n+1 in accumulate count 0 x end

    local 
      fun lexordset [] = []
        | lexordset (a::x) = lexordset (filter (lexless a) x) @ [a] @
                             lexordset (filter (lexgreater a) x)
      and lexless(a1:int,b1:int)(a2,b2) = 
           if a2<a1 then true else if a2=a1 then b2<b1 else false
      and lexgreater pr1 pr2 = lexless pr2 pr1
      fun collect f list =
             let fun accumf sofar [] = sofar
                   | accumf sofar (a::x) = accumf (revonto sofar (f a)) x
              in accumf [] list
             end
      fun occurs3 x = 
          (* finds coords which occur exactly 3 times in coordlist x *)
          let fun f xover x3 x2 x1 [] = diff x3 xover
                | f xover x3 x2 x1 (a::x) = 
                   if member xover a then f xover x3 x2 x1 x else
                   if member x3 a then f (a::xover) x3 x2 x1 x else
                   if member x2 a then f xover (a::x3) x2 x1 x else
                   if member x1 a then f xover x3 (a::x2) x1 x else
                                       f xover x3 x2 (a::x1) x
              and diff x y = filter (not o member y) x
           in f [] [] [] [] x end
     in 
      abstype generation = GEN of (int*int) list
        with 
          fun alive (GEN livecoords) = livecoords
          and unsafegen c = GEN c
          and mkgen coordlist = GEN (lexordset coordlist)
          and mk_nextgen_fn neighbours gen =
              let val living = alive gen
                  val isalive = member living
                  val liveneighbours = length o filter isalive o neighbours
                  fun twoorthree n = n=2 orelse n=3
                  val survivors = filter (twoorthree o liveneighbours) living
                  val newnbrlist = collect (filter (not o isalive) o neighbours) living
                  val newborn = occurs3 newnbrlist
               in mkgen (survivors @ newborn) end
     end
    end

     fun random_gen () = 
     let
        val gen = Rand.newgen() 
        val density = 0.05 + (Rand.random gen * 0.1)
        val acc = ref ([]:(int*int) list);
        fun random_row 0 = ()
          | random_row i = (random_column (i,size);random_row (i-1))
        and random_column (_,0) = ()
          | random_column (i,j)  = (if Rand.random gen < density
                                         then acc:=(i,j)::(!acc)
                                    else ();
                                    random_column (i,(j-1)))
    in  random_row size;
        unsafegen(!acc)
    end

    local fun i - j = let val k = Int.-(i,j) in if k < 0 then size else k end
          fun i + j = let val k = Int.+(i,j) in if k > size then 0 else k end
    in    
        fun neighbours (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),
                                (i,j-1),(i,j+1),
                                (i+1,j-1),(i+1,j),(i+1,j+1)]
    end
    
    fun invisible (x,y) = x<0 orelse x>size orelse y<0 orelse y>size

    fun animate p = 
        let val n = (mk_nextgen_fn neighbours p)
            val continue = ref false
            val () = app (fn c as (x,y) => 
                          if member (alive n) c orelse invisible c 
                              then () 
                          else (continue:=true;image.#Clear(x,y)))
                         (alive p)
            val () = app (fn c as (x,y) => 
                          if member (alive p) c orelse invisible c
                              then () 
                          else (continue:=true;image.#Set(x,y)))
                         (alive n)
        in
            if !continue then animate n else ()
        end

    val startgen = random_gen()
in
    animate startgen
end

end (* Life *)



