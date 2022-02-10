#!/usr/bin/python3
from operator import neg


class Failure:
    def __init__(self, value, failed=False):
        self.value = value
        self.failed = failed

    def __str__(self):
        if self.failed:
            return "Nothing"
        else:
            return "Just(" + str(self.value) + ")"

    def __or__(self, f):
        return self.bind(f)

    def bind(self, f -> List):
        if self.failed:
            return self
        try:
            x = f(self.get())
            return Failure(x)
        except:
            return Failure(None, True)


if __name__ == "__main__":
    proc = lambda x: Failure(x) | int | (lambda y: y + 3) | neg | str
    x = proc("1")
    y = proc("xyz")
    print(x)
    print(y)
