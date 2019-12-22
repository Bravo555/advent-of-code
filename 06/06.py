from functools import reduce

data = open('06/input.txt', 'r').read().splitlines()


def parse_orbits(lines):
    orbit_tree = {}
    for line in lines:
        parent, child = line.split(')')
        orbit_tree[child] = parent
    return orbit_tree


def trace_orbit(orbit_tree, curr_node, num_orbits=0):
    if curr_node == 'COM':
        return num_orbits
    return trace_orbit(orbit_tree, orbit_tree[curr_node], num_orbits+1)


def count_orbits(orbit_tree):

    orbits = 0
    for orbit in orbit_tree:
        orbits += trace_orbit(orbit_tree, orbit)
    return orbits


def distance(orbit_tree, src, dst):
    src = [src]
    dst = [dst]

    while True:
        srctrace = trace_step(orbit_tree, src)
        dsttrace = trace_step(orbit_tree, dst)

        if srctrace[-1] in dsttrace:
            return len(srctrace) + dsttrace.index(srctrace[-1]) - 3
        if dsttrace[-1] in srctrace:
            return len(dsttrace) + srctrace.index(dsttrace[-1]) - 3


def trace_step(orbit_tree, trace):
    if trace[-1] == 'COM':
        return trace
    parent = orbit_tree[trace[-1]]
    trace.append(parent)
    return trace


tree = parse_orbits(data)
print(distance(tree, 'YOU', 'SAN'))
