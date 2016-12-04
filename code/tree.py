"""
Simple Tree representation in Python
"""


class Tree:
    def __init__(self):
        pass

    def find_gifts(self):
        raise NotImplementedError

    @classmethod
    def parse(cls, tree_string):
        pass


class Branch(Tree):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def find_gifts(self):
        return self.left.find_gifts() + self.right.find_gifts()


class Leaf(Tree):
    def __init__(self, gift=None):
        self.gift = gift

    def __str__(self):
        return "Nothing" if self.gift is None else self.gift

    def find_gifts(self):
        return [self.gift]


def main():
    mytree = Branch(Leaf(gift="Toy"), Leaf())
    print(mytree.find_gifts())


if __name__ == '__main__':
    main()
