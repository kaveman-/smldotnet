using System.Reflection;
using System;
using System.IO;

class clslist
{

static String fullName(Type t){
	return t.ToString();
}

static void print(String s)
{
  Console.Out.Write(s);
}

static void print(string field, string s)
{
    print(field);
    print("\n");
    if (s==null) {
	print("null");
    }
    else {
	print(s);
    };
    print("\n");
}

static void print(string field,byte[] b)
{
    print(field);
    print("\n");
    if (b==null) {
	print("null");
    }
    else {
	print("(");
	for (int i = 0; i < b.Length; i++) {
	    print(BitConverter.ToString(b,i,1));
	    print(" ");
	};
	print(")");
    };
    
    print("\n");
}

static void print(string field,Version v)
{
    print(field);
    print("\n");
    if (v == null) {
	print("null");
    }
    else {print(v.Major.ToString());
  	  print(":");
	  print(v.Minor.ToString());
  	  print(":");
	  print(v.Build.ToString());
 	  print(":");                   
	  print(v.Revision.ToString()); 
    };
    print("\n");
}

static bool IsGeneric(Type c)
{
#if WHIDBEY
  return (c.IsGenericTypeDefinition);
#else
  return false;
#endif
}

static bool Importable(Type c)
{
  if (!c.IsPublic && !c.IsNestedPublic)
    return false;
  if (Environment.Version.Major >= 2 && IsGeneric(c))
    return false;
  return true;
}

static int Run(String AssemblyFile,String AssemblyStamp)
{
  Assembly a = null;

  print("clslist 9\n");
  if (AssemblyFile == null) 
  {
    a = Assembly.Load("mscorlib.dll");
  }
  else
  { try { a = Assembly.LoadFrom(AssemblyFile);}
    catch (Exception e) { Console.WriteLine("Error: " + e.ToString()); };
  }

  if (a == null) 
  {
    print("ERROR: Assembly ");
    print(AssemblyFile);
    print(" not found\n");
    return -1;
  }
  else
  {
    AssemblyName an = a.GetName();
    print("assemblyFile",AssemblyFile);
    print("stamp",AssemblyStamp);
    print("name",an.Name);
    print("publickeytoken",an.GetPublicKeyToken());
    print("version",an.Version);
   
    Type[] cs = a.GetTypes();
    for (int i = 0; i < cs.Length; i++)
    {
      
      if (Importable(cs[i]))
      {
      print(fullName(cs[i]));
      print(" ");
      if (cs[i].GetConstructors().Length == 0 || cs[i].IsAbstract || cs[i].IsInterface) 
        print(".");
      else
        print("C");
      if (cs[i] == typeof(string) || cs[i].IsEnum || (cs[i].IsPrimitive && cs[i] != typeof(float) && cs[i] != typeof(double)))
        print("=");
      else
        print(".");
      if (cs[i].IsEnum) 
        print("E");
      else
        print(".");
      if (cs[i].IsValueType) 
        print("V");
      else
        print(".");
      print("\n");
      }
    }
    return 0;
  }
}

static void Usage()
{
#if WHIDBEY
  print("Usage: clslist2 [AssemblyFile] [AssemblyStamp] [out] \n\n");
#else
  print("Usage: clslist [AssemblyFile] [AssemblyStamp] [out] \n\n");
#endif
}

public static int Main(String[] a)
{
  String AssemblyFile = null;
  String AssemblyStamp = null;
  String[] args = System.Environment.GetCommandLineArgs();  
  if (args.Length == 1 || args.Length > 4){Usage();return -1;};
  if (args.Length > 1) AssemblyFile = args[1];
  if (args.Length > 2) AssemblyStamp = args[2];
  if (args.Length > 3) 	{  
      try { TextWriter tmp = Console.Out;
	    FileStream fs1 = new FileStream(args[3], FileMode.Create);
	    StreamWriter sw1 = new StreamWriter(fs1);
	    Console.SetOut(sw1);	
	    int result = Run(AssemblyFile,AssemblyStamp);
	    sw1.Close();
	    Console.SetOut(tmp);
	    return result; }
      catch {return -1;}
  }
  else {
      return Run(AssemblyFile,AssemblyStamp);
  };
}

}
