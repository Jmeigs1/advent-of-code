public class Helpers
{
    public static long gcd(long a, long b)
    {
        while (b != 0)
        {
            var temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    public static long lcm(long a, long b)
    {
        return a * b / gcd(a, b);
    }
}
