using System.Reflection;
using System.Globalization;
using System;
using System.IO;

class NonVerifiable : Exception { }

class GetMeta
{
static NumberFormatInfo NFI = CultureInfo.InvariantCulture.NumberFormat;

static String fullName(Type t){
	return t.ToString();
}

static void print(String s)
{
  Console.Out.Write(s);
}

static void printLine(String s)
{
  Console.Out.WriteLine(s);
}

// Can this type be imported to SML.NET?
// Currently, only pointer types are excluded
// Also exclude generic types for now
static bool Importable(Type t)
{
#if WHIDBEY
  if (t.IsGenericType)
    return false;
#endif
  if (t.IsPointer)
    return false;
  if (t.IsArray)
    return Importable(t.GetElementType());
  return true;
}

// Print a CLR type in its imported SML form.
static void printType(Type c)
{
  if (c==typeof(void)) print("%");
  else
  {
    /* The second condition is necessary only because of a URT Bug */
    if (c.IsArray && c!=typeof(System.Array))
    {
      print("a");
      printType(c.GetElementType());
    }
    else
    if (c.IsByRef)
    {
      print("&");
      printType(c.GetElementType());
    }
    else 
    {
      if (c.IsValueType) 
      {
        if ((c.IsPrimitive && c != typeof(float) && c != typeof(double))
          || c.IsEnum)
          print("V");
        else
          print("v");
      }
      else
      {
        if (c == typeof(string))
          print("C");
        else
          print("c");
      }
      print(c.Assembly.GetName().Name + "!");
      print(fullName(c) + "!");
#if WHIDBEY
     if (c.IsGenericType) {
       foreach (Type t in c.GetGenericArguments()) {
         printType(t);
       }
     }
#endif

    }
  }
}

static void DumpMethod(Type c, MethodInfo methi)
{ 
  Type retty = methi.ReturnType;
  ParameterInfo[] pi = methi.GetParameters();
  int n = pi.Length;
  String name = methi.Name;

#if WHIDBEY
  if (methi.IsGenericMethodDefinition)
    return;
#endif
  foreach (ParameterInfo p in pi) {
    if (!Importable(p.ParameterType)) return;
  }
  if (!Importable(retty)) return;
  if (c == methi.DeclaringType && !methi.IsPrivate)
  {
    if (methi.IsAbstract) print("a");
    if (methi.IsStatic) print("s");
    if (methi.IsFinal) print("f");
    if (methi.IsPublic) print("p");
    if (methi.IsFamily) print("P");
/*  if (methi.IsSynchronized) print("y"); */
    if (methi.IsVirtual) print("v");

    print("\"");
    print(name);
    print("\":");

    printType(retty);
    print("(");
    for (int j=0; j < n; j++)
    {
      if (j > 0) print(",");
      printType(pi[j].ParameterType);
    }
    print(")\n");
  }
}

static void DumpConstructor(Type c, ConstructorInfo coni)
{
  ParameterInfo[] pi = coni.GetParameters();
  int n = pi.Length;
      
  foreach (ParameterInfo p in pi) {
    if (!Importable(p.ParameterType)) return;
  }
  if (c == coni.DeclaringType && !coni.IsPrivate)
  {
    if (coni.IsPublic) print("p");
    if (coni.IsFamily) print("P");
   
    print(":(");
    for (int j=0; j < n; j++)
    {
      if (j > 0) print(",");
      printType(pi[j].ParameterType);
    }
    print(")");
    print("\n");
  }
}

static void DumpMethods(Type c)
{
  MethodInfo[] ms = c.GetMethods(/*true */);
	
  for (int i=0; i < ms.Length; i++) 
  {
    DumpMethod(c, ms[i]);
  }

  ConstructorInfo[] cs = c.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance); 

  print("!\n");
  for (int i=0; i < cs.Length; i++) 
  {
    DumpConstructor(c, cs[i]);
  }
}

static void DumpFields(Type c, bool isenum)
{
  FieldInfo[] fs = c.GetFields(/* true*/);
	
  for (int i=0; i < fs.Length; i++) 
  {
    FieldInfo fldi = fs[i];
    Type t = fldi.FieldType;

    if (c == fldi.DeclaringType && !fldi.IsPrivate && Importable(t))
    {
      if (fldi.IsStatic) print("s");
      if (fldi.IsInitOnly || fldi.IsLiteral) print("f");
      if (fldi.IsPublic) print("p");
      if (fldi.IsFamily) print("P");

      print("\"");print(fldi.Name);print("\"");

      if (fldi.IsLiteral)
      {
        Object v = fldi.GetValue(null);
        Type ft = fldi.FieldType;
        print("=");
        printType(t);
        if (ft == typeof(char))
             {print (System.Convert.ToUInt32((System.Char)v).ToString(NFI));}  
        else 
	if (ft == typeof(string)) {
          if (v == null) {
	      print ("null");
	  }
	  else {
	      string s = (System.String) v;
	      for (int j = 0; j < s.Length; j++) {
		  print(System.Convert.ToUInt32(s[j]).ToString(NFI));
		  print(" ");
	      };
	  };
	}
        else {
	  if(isenum) {print (((System.IConvertible)(System.Enum)v).ToInt32(null).ToString(NFI));}  
          else if (v is System.Double) print(((System.Double)v).ToString("R",NFI));  
          else if (v is System.Single) print(((System.Single)v).ToString("R",NFI));  
	  else {print(v.ToString(/* NFI ?*/));};
	}
     }
     else 
     {
      print(":");
      printType(t);
     }
      print("\n");
    }
  }
}

static void DumpType(Type c)
{
  Type[] cs = c.GetInterfaces();
  Type superc = c.BaseType;
  int n = cs.Length;
  Assembly a = c.Assembly;
  
  if (c.IsAbstract) print("a");
  if (c.IsSealed) print("s");
  if (c.IsInterface) print("i"); 
  if (c.IsValueType) print("v");
  if (c.IsEnum) print("e");
  if (c.IsPublic) print("p");
  if (superc != null) 
  {
    print(":");
    printType(superc);
  }
  
  if (n > 0)
  {
    for (int i = 0; i < n; i++)
    {
      if (!cs[i].IsNotPublic && Importable(cs[i])) {
        print("$");
        printType(cs[i]);
      }
    }
  } 

  print("\n");
  DumpFields(c, c.IsEnum);
  print("!\n");
  DumpMethods(c);
}

static int Run(String TypeName, String AssemblyFile, String AssemblyStamp)
{
  Assembly a = null;
  try {a = Assembly.LoadFrom(AssemblyFile);} 
  catch {};

  if (a == null) 
  {
    print("ERROR: Assembly ");
    print(AssemblyFile);
    print(" not found\n");
    return -1;
  };

  Type c = a.GetType(TypeName);
  if (c == null) 
  {
    print("ERROR: Type ");
    print(TypeName);
    print(" not found\n");
    return -1;
  }
  else 
  {
    printLine("getmeta v12");
    printLine(AssemblyFile);
    printLine(AssemblyStamp);
    
    DumpType(c); 
  }

  return 0;
}

static void Usage()
{
#if WHIDBEY
   print("Usage: getmeta2 [TypeName] [AssemblyFile] [AssemblyStamp] [out] \n\n");
#else
   print("Usage: getmeta [TypeName] [AssemblyFile] [AssemblyStamp] [out] \n\n");
#endif
}

public static int Main(String[] a)
{
  String TypeName = null;
  String AssemblyFile = null;
  String AssemblyStamp = null;
  String[] args = System.Environment.GetCommandLineArgs();  
  if (args.Length == 1 || args.Length > 5){Usage();return -1;};
  if (args.Length > 1) TypeName = args[1];
  if (args.Length > 2) AssemblyFile = args[2];
  if (args.Length > 3) AssemblyStamp = args[3];
  if (args.Length > 4) 	{       
      try { TextWriter tmp = Console.Out;
	    FileStream fs1 = new FileStream(args[4], FileMode.Create);
	    StreamWriter sw1 = new StreamWriter(fs1);
	    Console.SetOut(sw1);	
	    int result = Run(TypeName,AssemblyFile,AssemblyStamp);
	    sw1.Close();
	    Console.SetOut(tmp);
            return result;}
      catch {print ("Cannot open file"); return -1;}
  }
  else {
      return Run(TypeName,AssemblyFile,AssemblyStamp);
  };
}

}