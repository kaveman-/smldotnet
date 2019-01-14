structure StringSet = SetFn(
  struct
    type ord_key = string
    val compare = String.compare
  end)



