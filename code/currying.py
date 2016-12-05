import pytest

from toolz import functoolz
from toolz.curried import pipe, map, filter, get, mapcat


def get_gifts(people):
    # ``pipe(data, f, g, h)`` is equivalent to ``h(g(f(data)))`
    return pipe(people,
        filter(lambda v: v['age'] < 18 and v['well_behaved']),
        mapcat(get(['name'])),
        list)


def get_gifts_classic(people):
    getting_gifts = []
    for person in people:
        if person['age'] < 18 and person['well_behaved']:
            getting_gifts.append(person['name'])

    return getting_gifts


def uppercase_first_letter_classic(word):
    return word[0].upper()


def uppercase_first_letter_compose(word):

    comp = functoolz.compose(get(0), lambda c: c.upper())
    return comp(word)
    # equivalent to
    # to_up = lambda c: c.upper()
    # return to_up(get(0, word))


def test_piping():
    people = [
        {'name': 'Bob', 'age': 10, 'well_behaved': True},
        {'name': 'Rick', 'age': 10, 'well_behaved': False},
        {'name': 'John', 'age': 20, 'well_behaved': True},
    ]
    assert get_gifts_classic(people) == ['Bob']
    assert get_gifts(people) == ['Bob']


@pytest.mark.parametrize(('inp', 'out'), [
    ('hello', 'H')
])
def test_compose(inp, out):
    assert uppercase_first_letter_classic(inp) == out
    assert uppercase_first_letter_compose(inp) == out
