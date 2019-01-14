using System;
using System.Collections;
using System.IO;
using System.Reflection;
// using Microsoft.Win32;

public class getsysdir {

// Attempt to determine FrameworkDir and FrameworkVersion by reflecting on the assembly of System.Object
// NB: the result may depend on the config file getsysdir.exe.config
public static int Run() {
   Uri uri = new Uri (typeof(object).Assembly.CodeBase);
   if (!uri.IsFile)
     return 1;
   String path = Directory.GetParent(uri.LocalPath).ToString();
   Console.Out.WriteLine(Path.GetDirectoryName(path));
   Console.Out.WriteLine(Path.GetFileName(path));
   return 0; 
}

static void Usage()
{
  Console.Out.Write("Usage: getsysdir [out] \n\n");
}

public static int Main(String[] a)
{
  String[] args = System.Environment.GetCommandLineArgs();  
  if (args.Length < 1 || args.Length > 2){Usage();return -1;};
  if (args.Length > 1) 	{  
      TextWriter tmp = Console.Out;
      FileStream fs1 = new FileStream(args[1], FileMode.Create);
      StreamWriter sw1 = new StreamWriter(fs1);
      Console.SetOut(sw1);	
      int result = Run();
      sw1.Close();
      Console.SetOut(tmp);
      return result;
  }
  else {
      return Run();
  };
}

}
