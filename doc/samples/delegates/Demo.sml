(* using delegates in SML.NET *)
structure Demo = struct 
fun main ()= 
    let
	open Classes

	(* creating a Task delegate, from a function *)
	val task = Task(fn () => print "\ninvoked!")
	val _ = task.#Invoke()
	

	(* creating a bunch of tasks *)
        val tasks = List.map (fn s => Task (fn () => (print "\n";print s))) ["a","b","c"]
 
        (* adding them to the TaskList *)
	val _ = List.app TaskList.AddTask tasks


        (* do the same from sml.net *)
	val SOME (mltasks:>Task) = 
	    List.foldl (fn(s,task) => 
			System.Delegate.Combine(task,
						Task(fn () => (print "\n";print s))))
	    NONE 
	    ["d","e","f"]

	(* declaring a delegate type *)

	_classtype BinaryOp of (int * int) -> int

	val plus = BinaryOp (op +)  
	val times = BinaryOp (op * ) 

        fun fold(delegate:BinaryOp,id,list) = List.foldr (delegate.#Invoke) id list

        val 10 = fold(plus,0,[1,2,3,4])
	val 24 = fold(times,1,[1,2,3,4])

    in  (* invoking the multicast delegates *)
	TaskList.DoTasks();
        (* invoking the original delegate *)
	task.#Invoke();
	mltasks.#Invoke();
	print "\ndone!" 
    end 

end










