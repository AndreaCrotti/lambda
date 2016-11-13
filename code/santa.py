import unittest

from itertools import permutations
from collections import namedtuple

INPUT = """Luke Skywalker
Leia Skywalker
Toula Portokalos
Gus Portokalos
Bruce Wayne
Virgil Brigman
Lindsey Brigman"""

lines = INPUT.split('\n')

Person = namedtuple('Person', 'first last')


def is_valid_comb(giv_recv):
    for giver, receiver in giv_recv:
        if (giver == receiver) or (giver.last == receiver.last):
            return False
    return True


def input_to_tuples(persons):
    splitted = [p.split(' ') for p in persons]
    return [Person(*x) for x in splitted]


def gen_valid_combinations(persons):
    for p in permutations(persons):
        comb = zip(persons, p)
        if is_valid_comb(zip(persons, p)):
            yield comb


class TestSantaExtraction(unittest.TestCase):
    def test_is_valid(self):
        not_valid = [
            (Person('bruce', 'wayne'), Person('pippo', 'wayne'))
        ]
        valid = [
            (Person('bruce', 'wayne'), Person('pippo', 'pluto'))
        ]
        self.assertTrue(is_valid_comb(valid))
        self.assertTrue(not is_valid_comb(not_valid))

    def test_input_to_tuples(self):
        ls = ['fst snd', 'first last']
        des = [Person(first='fst', last='snd'), Person(first='first', last='last')]
        self.assertEqual(input_to_tuples(ls), des)


def str_person(person):
    return ' '.join([person.first, person.last])


if __name__ == '__main__':
    tuples = input_to_tuples(lines)
    for idx, valid in enumerate(gen_valid_combinations(tuples)):
        print("\nValid combination %d" % idx)
        for p1, p2 in valid:
            print("%s -> %s" % (str_person(p1), str_person(p2)))
