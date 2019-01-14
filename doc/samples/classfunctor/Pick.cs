// Very dull C# program to pick at random one element of an array

public class Pick
{
  public static object PickRandom(object[] x)
  {
    System.Random r = new System.Random();
    return x[r.Next(x.Length)];
  }
}
