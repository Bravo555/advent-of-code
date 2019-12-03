from functools import reduce

with open('input.txt') as f:
    data = list(map(str.strip, f.readlines()))
    data = [seq.split(',') for seq in data]


# assume central port as (0, 0)
def visited_coords(move, origin):
    length = int(move[1:])
    unit = {
        'U': (0, 1),
        'D': (0, -1),
        'L': (-1, 0),
        'R': (1, 0),
    }[move[:1]]
    return [(origin[0] + unit[0] * length, origin[1] + unit[1] * length) for length in range(1, length + 1)]


def trace(sequence):
    visited = []
    origin = (0, 0)
    for line in sequence:
        visited_in_move = visited_coords(line, origin)
        origin = visited_in_move[-1]
        visited += visited_in_move
    return visited


def manhattan_distance_from_origin(point):
    return abs(point[0]) + abs(point[1])


traces = list(map(trace, data))
intersections = set(traces[0]).intersection(set(traces[1]))

print(min([traces[0].index(intersection) + traces[1].index(intersection) + 2
           for intersection in intersections]))


solution = reduce((lambda x, y: x if manhattan_distance_from_origin(
    x) < manhattan_distance_from_origin(y) else y), intersections)
