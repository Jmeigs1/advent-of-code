key_words = ["NOT", "OR", "AND", "RSHIFT", "LSHIFT"]
graph = {}
cache = {}


def f_not(a, _):
    return ~a


def f_or(a, b):
    return a | b


def f_and(a, b):
    return a & b


def f_rsft(a, b):
    return a >> b


def f_lsft(a, b):
    return a << b


key_words_to_fn = {
    "NOT": f_not,
    "OR": f_or,
    "AND": f_and,
    "RSHIFT": f_rsft,
    "LSHIFT": f_lsft,
}


def get_inputs(op, s):
    if op is None:
        return [s]

    parts = s.split(op)
    parts = map(lambda s: s.strip(), parts)
    parts = filter(lambda s: len(s) != 0, parts)
    return list(parts)


def get_op(s: str):
    for word in key_words:
        if word in s:
            return word
    return None


def parse_line(line: str):
    p1, output = line.split(" -> ")
    op = get_op(p1)
    inputs = get_inputs(op, p1)
    return (inputs, output, op)


def walk_back(reg, graph):
    def search_if_reg(v):
        if v in cache:
            return cache[v]
        else:
            try:
                return int(v)
            except:
                ret = walk_back(v, graph)
                cache[v] = ret
                return ret

    def make_inputs(inputs):
        if len(inputs) == 1:
            return (search_if_reg(inputs[0]), -1)
        else:
            return (search_if_reg(inputs[0]), search_if_reg(inputs[1]))

    data = graph[reg]
    op = data["op"]
    inputs = data["in"]
    (a, b) = make_inputs(inputs)

    if op is None:
        return a
    op_fn = key_words_to_fn[op]
    return op_fn(a, b)


file = open("input.txt", "r")
for line in file:
    line = line.strip()
    inputs, child, op = parse_line(line)

    graph[child] = {"op": op, "in": inputs}

print(walk_back("a", graph))
