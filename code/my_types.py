from collections import Counter
# from typing import Callable, Iterable


def enrich(text: str) -> Counter:
    return Counter(text)

def typed_addition(a: int, b: int) -> int:
    return a + b


if __name__ == '__main__':
    print(enrich("Hello world"))
    print(enrich(100))
    typed_addition(1, "string")

# Local Variables:
# compile-command: "mypy --silent-imports types.py"
# End:
