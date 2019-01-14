namespace Arrays
{

public class Utils
{
  public static void Reverse(int[] ia)
  {
    for (int i = 0; i < ia.Length/2; i++)
    {
      int t = ia[i];
      ia[i] = ia[ia.Length-i-1];
      ia[ia.Length-i-1] = t;
    }
  }

  public static string[] Slice(string[] a, int start, int n)
  {
    string[] a2 = new string[n];
    for (int i = 0; i < n; i++)
    {
      a2[i] = a[start+i];
    }
    return a2;
  }
}

}
