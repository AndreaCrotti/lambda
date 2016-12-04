from toolz import functoolz
from toolz.curried import pipe, map, filter, get, mapcat


def get_gifts(people):
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


def test_piping():
    people = [
        {'name': 'Bob', 'age': 10, 'well_behaved': True},
        {'name': 'Rick', 'age': 10, 'well_behaved': False},
        {'name': 'John', 'age': 20, 'well_behaved': True},
    ]
    assert get_gifts_classic(people) == ['Bob']
    assert get_gifts(people) == ['Bob']
