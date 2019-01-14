// Example of using fields from SML
namespace Fields
{
public class C
{
  // STATIC FIELDS

  // First, a constant field; this will appear as
  //   val static_const_field : int
  public const int static_const_field = 12345;

  // Next, a readonly field; this will appear as
  //   val static_readonly_field : string option
  public static readonly string static_readonly_field = System.String.Concat("Hello, ", "World");

  // Finally, a mutable field; this will appear as a convenient type abbreviation  describing the field as a storage type
  //   type static_field = (int,(C,static_field)static) reference
  // together with a reference of that type:
  //   val static_field : static_field
  public static int static_field = 23;

  // INSTANCE FIELDS
  // A readonly field; this will appear as
  //   val readonly_field : C -> string option
  //   and for any c : C,  
  //	c.#readonly_field : string option
  public readonly string inst_readonly_field;

  // Finally, a mutable field; this will appear as a convenient type abbreviation, describing the field as a storage type:
  //   type instfield = (int,(C,inst_field)field) reference 
  // and for any c : C, the member #inst_field is defined and a reference of that type
  //	c.#inst_field : inst_field
  public int inst_field;

  public C(string s, int i) { inst_readonly_field = System.String.Concat("Hello, ", s); inst_field = i; }

}
}

