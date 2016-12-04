from collections import Counter
# from typing import Callable, Iterable


def enrich(text: str) -> Counter:
    return Counter(text)


if __name__ == '__main__':
    print(enrich("Hello world"))
    print(enrich(100))

# Local Variables:
# compile-command: "mypy --silent-imports types.py"
# End:
