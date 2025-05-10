namespace Part2;

class CardValue
{
    static public int toValue(String card)
    {
        switch (card.ToLower())
        {
            case "a": return 14;
            case "k": return 13;
            case "q": return 12;
            case "j": return 1;
            case "t": return 10;
            default: return int.Parse(card);
        }
    }

    static public char toCard(int card)
    {
        switch (card)
        {
            case 14: return 'A';
            case 13: return 'K';
            case 12: return 'Q';
            case 1: return 'J';
            case 10: return 'T';
            default: return card.ToString()[0];
        }
    }
}

public class Hand : IComparable<Hand>
{
    private int[] _originalCards { get; set; }
    private (int, int)[] _cards { get; set; }
    private int _jokers { get; set; }

    public int CompareTo(Hand? other)
    {
        if (other == null) return 1;

        // General case
        if (_cards.Length != other._cards.Length)
        {
            return other._cards.Length - _cards.Length;
        }

        // Full house vs 4 of a kind Test
        if (_cards[0].Item2 + _jokers != other._cards[0].Item2 + other._jokers)
        {
            return _cards[0].Item2 + _jokers - (other._cards[0].Item2 + other._jokers);
        }

        // Tiebreaker
        for (int i = 0; i < _originalCards.Length; i++)
        {
            var diff = _originalCards[i] - other._originalCards[i];
            if (diff != 0)
            {
                return diff;
            }
        }

        return 0;
    }

    public Hand(string cards)
    {
        int jokers = 0;
        var local_cards = cards.ToArray()
          .Select(card => CardValue.toValue(card.ToString()))
          .Aggregate(
            new List<(int, int)>(), (agg, card) =>
            {
                if (card == 1)
                {
                    jokers++;
                    return agg;
                }
                var idx = agg.FindIndex(x => x.Item1 == card);
                if (idx == -1)
                {
                    agg.Add((card, 1));
                }
                else
                {
                    agg[idx] = (card, agg[idx].Item2 + 1);
                }

                return agg;
            });

        _cards = local_cards
            .Where(x => x.Item2 > 0)
            .OrderByDescending(x => x.Item2).ThenByDescending(x => x.Item1)
            .ToArray();

        // Hack to deal with all jokers
        if (_cards.Length == 0)
        {
            _cards = new (int, int)[1];
            _cards[0] = (0, 0);
        }

        _originalCards = cards.ToArray()
            .Select(card => CardValue.toValue(card.ToString()))
            .ToArray();
        _jokers = jokers;
    }

    public override string ToString()
    {
        return String.Join(",", _originalCards.Select(v => CardValue.toCard(v))) + " | " + String.Join(",", _cards
            .Select(x => x.Item1.ToString() + ":" + x.Item2.ToString())) + " jokers:" + _jokers;
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
