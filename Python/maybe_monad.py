#!/usr/bin/python3
from operator import neg


class Maybe:
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

    def bind(self, f):
        if self.failed:
            return self
        try:
            x = f(self.value)
            return Maybe(x)
        except:
            return Maybe(None, True)


def usr_input(text, f=(lambda x: x), errors=1):
    ans = Maybe(input(text + ": ")).bind(f)
    if ans.failed:
        print(f"(Err: {errors}) ", end="")
        return usr_input(text, f, errors + 1)
    else:
        return ans.value


if __name__ == "__main__":
    proc1 = lambda x: Maybe(x) | int | (lambda y: y + 3) | neg | str
    print(proc1("1"))
    print(proc1("xyz"))

    proc2 = lambda x: Maybe(x) | int | (lambda y: 1 / y) | str
    print(proc2("0"))
    print(proc2("2"))

    name = usr_input("Ingresa tu nombre")
    print(name)
    x = usr_input("Ingresa un entero", int)
    print(f"Result: {(x + 3)}")


# REFERENCES
# - https://medium.com/swlh/monads-in-python-e3c9592285d6
# - Martin McBride: Functional Programming in Python
