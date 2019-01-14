functor Wrapper(type T) =
struct
  _classtype W(x : T)
  with 
    get() = x 
  end
  fun wrap   (x : T) = W(x)
  fun unwrap (w : W) = w.#get()
end
