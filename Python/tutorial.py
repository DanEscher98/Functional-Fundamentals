#!/usr/bin/python3

from functools import lru_cache
from typing import Iterable


@lru_cache(10)
def fibonacci(n: int) -> int:
    if n < 2:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)


def python_zen():
    import contextlib
    import io

    with contextlib.redirect_stdout(zen := io.StringIO()):
        """Usage of the Walrus Operator"""
        import this
    print(zen.getvalue())


if __name__ == "__main__":
    with open("tutorial.py") as file:
        for l in file:
            print(l)

    for i in range(5):
        print(fibonacci(i))
    print("h0la")
    x = 2 if 1 < 2 else 3
    mylist = [x**2 for x in range(5) if x % 2 == 0]
