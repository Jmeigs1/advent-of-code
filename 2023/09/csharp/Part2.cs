namespace Part2;

class Sequence
{
    private int[] numbers;

    public Sequence(string line)
    {
        this.numbers = line.Trim().Split(' ')
          .Select(x => int.Parse(x))
          .ToArray();
    }

    public int getNext()
    {
        var working = new List<int>(this.numbers);

        var stack = new List<int>();

        while (working.Count > 1 && !working.All(v => v == 0))
        {
            var newList = new List<int>();
            stack.Add(working.First());

            for (int i = 0; i < working.Count - 1; i++)
            {
                newList.Add(working[i + 1] - working[i]);
            }
            working = newList;
        }

        if (working.Count == 1)
        {
            throw new Exception("getNext did not correctly find an end state");
        }

        var evenIdx = stack.Where((c, i) => i % 2 == 0).Sum();
        var oddIdx = stack.Where((c, i) => i % 2 != 0).Sum();

        return evenIdx - oddIdx;
    }
}

class Runner
{
    public static void Run(string filename)
    {
        var count = 0;

        String? data = null;
        const Int32 BufferSize = 128;

        using (var fileStream = File.OpenRead(filename))
        using (var streamReader = new StreamReader(fileStream, System.Text.Encoding.UTF8, true, BufferSize))
        {
            Sequence? s = null;
            while ((data = streamReader.ReadLine()) != null)
            {
                s = new Sequence(data);
                count += s.getNext();
            }
        }

        System.Console.WriteLine(count);

    }
}
