using System; 

/* CLS-compliant tools are only required to deal with a subset of the
encodings of custom attributes.  The only types that can appear in
these encodings are: System.Type, System.String, System.Char,
System.Boolean, System.Byte, System.Int16, System.Int32, System.Int64,
System.Single, System.Double, any enumeration type based on a
CLS-compliant base integer type
*/ 

namespace Classes {

public enum Enum { A, B, C }

public class CSharpAttribute : System.Attribute {

    public CSharpAttribute(){}

    public CSharpAttribute(/* System.Type t; */
  	 		   string s,
			   char c,
			   bool b,
			   byte u8,
			   sbyte i8,
			   short i16,
			   int i32,	
			   long i64,
			   float f,
			   double d,
			   Enum e) {
    }

    private string sf = null;

    public string Property {
	get{return sf;}
	set{sf = value;}
    }

    public string Field = null;

};
}














