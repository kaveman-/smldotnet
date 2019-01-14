structure Stats :> STATS =
struct

val stats = ref (StringMap.empty : int ref IntMap.map ref StringMap.map)
val counts = ref (StringMap.empty : int ref StringMap.map)

fun clear () = (stats := StringMap.empty; counts := StringMap.empty)

(* Up to but not including n *)
val groups = [1,2,3,4,5,
              10,15,20,25,30,40,50,60,70,80,90,
              100,150,200,250,300,400,500,600,700,800,900,
              1000,2000,3000,4000,5000,6000,7000,8000,9000,
              10000, valOf Int.maxInt]

fun printRange (start, limit, tally) = 
  if tally = 0 
  then ()
  else
  if start = limit-1
  then 
    Debug.print ("  " ^ Int.toString(start) ^ ": " ^ Int.toString(tally) ^ "\n")
  else
  if limit = valOf Int.maxInt
  then 
    Debug.print ("  " ^ Int.toString(start) ^ "- :" ^ Int.toString(tally) ^ "\n")
  else
    Debug.print ("  " ^ Int.toString(start) ^ "-" ^ Int.toString(limit-1) ^ ": " ^ Int.toString(tally) ^ "\n")

fun collate (data, ref tally, (currenttally, start, groups as limit::_)) = 
    if data < limit
    then (currenttally+tally, start, groups)
    else 
    let
      fun find (start', limit'::rest) = if data<limit' then (start', limit'::rest) else find (limit',rest)
      val (start', groups') = find (start,groups)
    in
      printRange (start, limit, currenttally);
      (tally, start', groups')
    end

fun calctotal (data, ref tally, (total, count)) = (data*tally+total, count+tally)

fun dumpInfo (s, ref m) = 
let
  val (total, count) = IntMap.foldri calctotal (0,0) m
in
  Debug.print ("\nAverage " ^ s ^ " is " ^ Int.toString (total div count) ^ " (count = " ^ Int.toString count ^ ") with distribution as follows:\n");
  let
    val (tally, start, limit::_) = IntMap.foldli collate (0,0,groups) m
    in printRange (start,limit,tally) end
end

fun dumpCountInfo (s, ref c) = Debug.print ("\nTotal number of " ^ s ^ " is " ^ Int.toString c)

fun dump () = (StringMap.appi dumpInfo (!stats); StringMap.appi dumpCountInfo (!counts); Debug.print "\n")

fun add (key, value) =
case StringMap.find(!stats, key) of
  NONE =>
  stats := StringMap.insert(!stats, key, ref (IntMap.insert(IntMap.empty, value, ref 1)))

| SOME m =>
  case IntMap.find(!m, value) of
    NONE =>
    m := IntMap.insert(!m, value, ref 1)

  | SOME count =>
    count := !count + 1


fun inc key =
case StringMap.find(!counts, key) of
  NONE =>
  counts := StringMap.insert(!counts,key,ref 1)

| SOME count =>
  count := !count + 1

end