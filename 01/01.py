# part 1
from functools import reduce


def fuel(mass):
    return mass // 3 - 2


data = list(map(int, map(str.strip, open('input.txt', 'r').readlines())))
solution = reduce((lambda x, y: x + y), map(fuel, data))
print(solution)
