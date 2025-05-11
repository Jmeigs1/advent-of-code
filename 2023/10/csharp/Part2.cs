namespace Part2;

enum Direction
{
    Up = 0,
    Right = 1,
    Down = 2,
    Left = 3,
}

class DirectionMover
{
    public static (int, int) move(Direction d, (int, int) pos)
    {
        switch (d)
        {
            case Direction.Up:
                return (-1, 0);
            case Direction.Right:
                return (0, 1);
            case Direction.Down:
                return (1, 0);
            case Direction.Left:
                return (0, -1);
            default:
                throw new Exception("bad direction in mover");
        }
    }
}

class Tile
{
    private char Type { get; set; }

    public bool isValidDirection(Direction direction)
    {
        switch (Type)
        {
            case '.':
                return false;
            case '-':
                return direction == Direction.Left || direction == Direction.Right;
            case '|':
                return direction == Direction.Up || direction == Direction.Down;
            case 'J':
                return direction == Direction.Down || direction == Direction.Right;
            case '7':
                return direction == Direction.Up || direction == Direction.Right;
            case 'L':
                return direction == Direction.Down || direction == Direction.Left;
            case 'F':
                return direction == Direction.Up || direction == Direction.Left;
            default:
                throw new Exception($"Invalid tile type: {Type}");
        }
    }

    public Direction getNextDirection(Direction inputDirection)
    {
        switch (Type)
        {
            case '.':
                throw new Exception("Invalid tile type: .");
            case '-':
                if (inputDirection == Direction.Left)
                {
                    return Direction.Left;
                }
                else if (inputDirection == Direction.Right)
                {
                    return Direction.Right;
                }
                throw new Exception("Invalid direction for tile -");
            case '|':
                if (inputDirection == Direction.Up)
                {
                    return Direction.Up;
                }
                else if (inputDirection == Direction.Down)
                {
                    return Direction.Down;
                }
                throw new Exception("Invalid direction for tile |");
            case 'J':
                if (inputDirection == Direction.Down)
                {
                    return Direction.Left;
                }
                else if (inputDirection == Direction.Right)
                {
                    return Direction.Up;
                }
                throw new Exception("Invalid direction for tile J");
            case '7':
                if (inputDirection == Direction.Up)
                {
                    return Direction.Left;
                }
                else if (inputDirection == Direction.Right)
                {
                    return Direction.Down;
                }
                throw new Exception("Invalid direction for tile 7");
            case 'L':
                if (inputDirection == Direction.Down)
                {
                    return Direction.Right;
                }
                else if (inputDirection == Direction.Left)
                {
                    return Direction.Up;
                }
                throw new Exception("Invalid direction for tile L");
            case 'F':
                if (inputDirection == Direction.Up)
                {
                    return Direction.Right;
                }
                else if (inputDirection == Direction.Left)
                {
                    return Direction.Down;
                }
                throw new Exception("Invalid direction for tile F");
            default:
                throw new Exception($"Invalid tile type: {Type}");
        }
    }

    public Tile(char type)
    {
        Type = type;
    }
}

class Path
{
    public List<(int, int)> Nodes { get; private set; }

    public void add((int, int) pos)
    {
        Nodes?.Add(pos);
    }

    public Path()
    {
        Nodes = new List<(int, int)>();
    }
}


class Maze
{
    private char[][] tiles;
    private (int, int) start;
    private Direction startDirection;

    private (int, int) getStart()
    {
        for (int i = 0; i < tiles.Length; i++)
        {
            for (int j = 0; j < tiles[i].Length; j++)
            {
                if (tiles[i][j] == 'S')
                {
                    return (i, j);
                }
            }
        }

        throw new Exception("No start found");
    }

    private Direction getStartDirection((int, int) start)
    {
        foreach (var d in Enum.GetValues(typeof(Direction)).Cast<Direction>())
        {
            var move = DirectionMover.move(d, start);
            var newPos = (start.Item1 + move.Item1, start.Item2 + move.Item2);
            var tileChar = tiles[newPos.Item1][newPos.Item2];
            var tile = new Tile(tileChar);
            if (tile.isValidDirection(d))
            {
                return d;
            }
        }
        throw new Exception("No start direction found");
    }

    public Path traverse()
    {
        var count = 1;
        var path = new Path();
        path.add(start);
        var currentPos = start;
        var currentDirection = startDirection;

        while (count < 100_000_000)
        {
            var nextMove = DirectionMover.move(currentDirection, currentPos);
            var newPos = (currentPos.Item1 + nextMove.Item1, currentPos.Item2 + nextMove.Item2);
            var nextTileChar = tiles[newPos.Item1][newPos.Item2];
            if (nextTileChar == 'S')
            {
                break;
            }
            var nextTile = new Tile(nextTileChar);
            var nextDirection = nextTile.getNextDirection(currentDirection);

            currentPos = newPos;
            currentDirection = nextDirection;
            path.add(currentPos);
            count++;
        }

        if (count == 100_000_000)
        {
            throw new Exception("Unable to find end in traverse");
        }

        return path;
    }

    public Maze(string inputData)
    {
        tiles = inputData.Trim().Split(Environment.NewLine)
          .Select(x => x.ToCharArray())
          .ToArray();

        start = getStart();
        startDirection = getStartDirection(start);
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
            data = streamReader.ReadToEnd();
        }

        var maze = new Maze(data);
        var path = maze.traverse();


        System.Console.WriteLine(path.Nodes.Count / 2);
    }
}
