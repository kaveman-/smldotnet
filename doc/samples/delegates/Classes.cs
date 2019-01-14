using System; 

namespace Classes {

   public delegate void Task();

   public class TaskList:object {

   private static Task Tasks = null;

   public static void AddTask(Task t){
	Tasks = (Task)System.Delegate.Combine(Tasks,t);
   }
 
   public static void DoTasks(){Tasks();}
   }
};








