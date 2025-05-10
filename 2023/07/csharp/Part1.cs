namespace Part1;

class CardValue
{
    static public int toValue(String card)
    {
        switch (card.ToLower())
        {
            case "a": return 14;
            case "k": return 13;
            case "q": return 12;
            case "j": return 11;
            case "t": return 10;
            default: return int.Parse(card);
        }
    }

    static public char toCard(int card)
    {
        switch (card)
        {
            case 14: return 'A';
            case 13: return 'k';
            case 12: return 'Q';
            case 11: return 'J';
            case 10: return 'T';
            default: return card.ToString()[0];
        }
    }
}

public class Hand : IComparable<Hand>
{
    private int[] _originalCards { get; set; }
    private (int, int)[] _cards { get; set; }

    public int CompareTo(Hand? other)
    {
        if (other == null) return 1;

        if (_cards.Length != other._cards.Length)
        {
            return _cards.Length < other._cards.Length ? 1 : -1;
        }

        if (_cards[0].Item2 > other._cards[0].Item2)
            return 1;
        else if (_cards[0].Item2 < other._cards[0].Item2)
            return -1;

        for (int i = 0; i < _originalCards.Length; i++)
        {
            if (_originalCards[i] > other._originalCards[i])
            {
                return 1;
            }
            else if (_originalCards[i] < other._originalCards[i])
            {
                return -1;
            }
        }

        return 0;
    }

    public Hand(string cards)
    {
        var local_cards = cards.ToArray()
          .Select(card => CardValue.toValue(card.ToString()))
          .Aggregate(
            new List<(int, int)>(), (agg, card) =>
            {
                var nextAgg = new List<(int, int)>(agg);

                var idx = agg.FindIndex(x => x.Item1 == card);
                if (idx == -1)
                {
                    nextAgg.Add((card, 1));
                }
                else
                {
                    nextAgg[idx] = (card, agg[idx].Item2 + 1);
                }

                return nextAgg;
            });

        _cards = local_cards
            .Where(x => x.Item2 > 0)
            .OrderByDescending(x => x.Item2).ThenByDescending(x => x.Item1)
            .ToArray();

        _originalCards = cards.ToArray()
            .Select(card => CardValue.toValue(card.ToString()))
            .ToArray();
    }

    public override string ToString()
    {
        return String.Join(",", _cards
            .Select(x => x.Item1.ToString() + ":" + x.Item2.ToString()));
    }

}

public class Round : IComparable<Round>
{
    public Hand Hand { get; private set; }
    public int Bid { get; private set; }

    public int CompareTo(Round? other)
    {
        return Hand.CompareTo(other?.Hand);
    }

    public Round(string cards, int bid)
    {
        Hand = new Hand(cards);
        Bid = bid;
    }
    public override string ToString()
    {
        return $"{Hand.ToString()} {Bid}";
    }
}

class Runner
{
    public static void Run(string filename)
    {
        List<Round> rounds = new List<Round>();

        const Int32 BufferSize = 128;
        using (var fileStream = File.OpenRead(filename))
        using (var streamReader = new StreamReader(fileStream, System.Text.Encoding.UTF8, true, BufferSize))
        {
            String? line;
            while ((line = streamReader.ReadLine()) != null)
            {
                var parts = line.Split(" ");
                var hand = new Hand(parts[0].Trim());
                var bid = int.Parse(parts[1].Trim());
                var round = new Round(parts[0], bid);
                rounds.Add(round);
            }
        }

        rounds = rounds.OrderBy(x => x).ToList();
        Int64 score = 0;

        for (int i = 0; i < rounds.Count; i++)
        {
            var round = rounds[i];
            var bid = round.Bid;
            score += (i + 1) * bid;
            // System.Console.WriteLine($"{i + 1} {round.Hand.ToString()} {bid} {score}");
        }

        System.Console.WriteLine($"Score: {score}");
    }
}
