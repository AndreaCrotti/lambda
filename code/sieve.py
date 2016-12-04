def sieve(n):
    multiples = set()

    for i in range(2, n + 1):
        if i not in multiples:
            yield i
            multiples.update(range(i * i, n + 1, i))


if __name__ == '__main__':
    print(list(sieve(1000)))
