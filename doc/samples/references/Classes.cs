using System; 

namespace Classes {

public class Class {
  public static int staticField = 0;
  public int objectField = 0;
  public static void swap(ref int i, ref int j){
    int temp = i; i = j; j = temp; return;
  }
  public Class(){return;}
}
public struct ValueType {
  public int valueField;
  };
}









