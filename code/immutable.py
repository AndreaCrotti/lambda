import pytest    

from toolz import dicttoolz


def enrich_dict_mutable(dic):
    for key in dic:
        if key % 2 == 0:
            dic[key] = 'bar'

    keys_below_zero = [x for x in dic if x < 0]
    for neg in keys_below_zero:
        del dic[neg]

    return dic


def _even_bar(item):
    """Given an item (key, 'string')
    return (key, 'bar') if key is an even number
    """
    key, val = item
    if key % 2 == 0:
        return key, 'bar'
    else:
        return key, val


def enrich_dict_immutable(dic):
    is_positive = lambda v: v >= 0
    filtered = dicttoolz.keyfilter(is_positive, dic)
    return dicttoolz.itemmap(_even_bar, filtered)


@pytest.mark.parametrize(('inp', 'out'), [
    ({}, {}),
    ({-1: 'foo'}, {}),
    ({0: 'foo'}, {0: 'bar'}),
    ({-1: 'foo', 0: 'foo', 1: 'foo', 2: 'foo'}, {0: 'bar', 1: 'foo', 2: 'bar'}),
])
def test_enrich_dictionary(inp, out):
    assert enrich_dict_immutable(inp) == out
    enrich_dict_mutable(inp)
    assert inp == out
