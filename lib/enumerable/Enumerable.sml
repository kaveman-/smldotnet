structure Enumerable :> ENUMERABLE =
struct
type enumerable = System.Collections.IEnumerable
type oo = System.Object option

fun app f (e:enumerable) =
  let val enumerator = valOf(e.#GetEnumerator())
      fun loop () = if enumerator.#MoveNext() then
                       (f (enumerator.#get_Current()); loop ())
                    else ()
  in loop ()
  end

fun map f (e:enumerable) =
  let val enumerator = valOf(e.#GetEnumerator())
      fun loop () = if enumerator.#MoveNext() then
                       (f (enumerator.#get_Current()) :: loop ())
                    else []
  in loop ()
  end

fun mapPartial f (e:enumerable) =
  let val enumerator = valOf(e.#GetEnumerator())
      fun loop () = if enumerator.#MoveNext() then
                       case f (enumerator.#get_Current())
                        of NONE => loop ()
                         | SOME v => v :: loop ()
                    else []
  in loop ()
  end

val toList = map (fn x=>x)

fun foldl f b (e:enumerable) =
  let val enumerator = valOf(e.#GetEnumerator())
      fun loop r = if enumerator.#MoveNext() then
                       loop (f (enumerator.#get_Current(), r))
                    else r
  in loop b
  end

fun foldr f b (e:enumerable) =
  let val enumerator = valOf(e.#GetEnumerator())
      fun loop () = if enumerator.#MoveNext() then
                       f (enumerator.#get_Current(), loop ())
                    else b
  in loop ()
  end

val length = foldl (fn (oo,n)=>n+1) 0

fun null (e:enumerable) =
  let val enumerator = valOf(e.#GetEnumerator())
  in not (enumerator.#MoveNext())
  end

fun hd  (e:enumerable) =
  let val enumerator = valOf(e.#GetEnumerator())
  in if enumerator.#MoveNext()
     then enumerator.#get_Current()
     else raise Empty
  end

val tl = List.tl o toList

fun last (e:enumerable) =
 let val enumerator = valOf(e.#GetEnumerator())
     val x = if enumerator.#MoveNext()
             then enumerator.#get_Current()
             else raise Empty
     fun loop y = if enumerator.#MoveNext()
                  then loop (enumerator.#get_Current())
                  else y
 in loop x
 end

fun find f (e:enumerable) =
  let val enumerator = valOf(e.#GetEnumerator())
      fun loop () = if enumerator.#MoveNext() then
                       let val oo = enumerator.#get_Current()
                       in if f oo
                          then SOME oo
                          else loop ()
                       end
                    else NONE
  in loop ()
  end

fun exists f e = Option.isSome (find f e)

fun all f = not o (exists (not o f))

end