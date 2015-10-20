import toolz

counter = 0

def process_item_wrong(item):
    global counter
    # ... do something with item ...
    counter += 1

# this function is instead thread safe
def process_item_fp(item, current):
    # .. do something with the item
    return current + 1


# modifying a dictionary
def handle_dict_no_fp(dic):
    del dic['key']


def handle_dict_fp(dic):
    return toolz.dissoc(dic, 'key')


class Example(object):
    def __init__(self, some_rdd):
        self.data = some_rdd

    def func(self, old_rdd):
        new_rdd = old_rdd.filter(lambda x: x % 2 ==0)
        return new_rdd

    def transform(self):
        self.new_data = self.func(self.data)


def func(rdd):
    return rdd.filter(lambda x: x % 2 ==0)


def transform(func, data):
    return func(data)

