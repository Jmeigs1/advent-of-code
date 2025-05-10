namespace Part2;
using System.Text.RegularExpressions;


class Navigator
{
    private char[] _directions { get; set; }
    private Dictionary<string, (string, string)> _desertMap { get; set; }

    private Dictionary<string, (string, string)> makeMap(string mapInput)
    {
        var regex = new Regex(@"\w+");
        var dict = new Dictionary<string, (string, string)>();
        foreach (var line in mapInput.Trim().Split('\n'))
        {
            var matches = regex.Matches(line);
            if (matches.Count != 3)
            {
                throw new Exception($"Invalid map line: {line}");
            }

            dict.Add(matches[0].Value, (matches[1].Value, matches[2].Value));
        }


        return dict;
    }

    public Navigator(string directions, string mapInput)
    {
        _directions = directions.ToCharArray();
        _desertMap = makeMap(mapInput);
    }

    private int traverseToEnd(string start)
    {
        int i = 0;
        var current = start;
        var isDone = (string current) => current.EndsWith('Z');

        while (i < 100_000_000 && !isDone(current))
        {
            var dir = _directions[i % _directions.Length];
            current = dir switch
            {
                'L' => _desertMap[current].Item1,
                'R' => _desertMap[current].Item2,
                _ => throw new Exception($"Invalid direction: {dir}")
            };
            i++;
        }
        if (!isDone(current))
        {
            throw new Exception($"Did not reach end of desert map. Current: {current}");
        }
        return i;
    }

    public long traverseAllToEnd()
    {
        var starts = _desertMap.Keys.Where(k => k.EndsWith('A')).ToList();

        var endings = starts.Select(start => traverseToEnd(start)).ToList();

        var lcm = (long)endings[0];

        foreach (var end in endings.Skip(1))
        {
            lcm = Helpers.lcm(lcm, end);
        }

        return lcm;
    }
}

class Runner
{
    public static void Run(string filename)
    {

        String? data = null;
        const Int32 BufferSize = 128;
        using (var fileStream = File.OpenRead(filename))
        using (var streamReader = new StreamReader(fileStream, System.Text.Encoding.UTF8, true, BufferSize))
        {
            data = streamReader.ReadToEnd().Trim();
        }

        var parts = data.Split("\n\n", StringSplitOptions.RemoveEmptyEntries);

        var part1 = parts[0];
        var part2 = parts[1];

        var navigator = new Navigator(part1, part2);

        System.Console.WriteLine(navigator.traverseAllToEnd());
    }
}
