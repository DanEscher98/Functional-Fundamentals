factorial = z_combinator(lambda  rec: lambda n:
    1 if n==0 else n*rec(n-1))

# The Beginning
def f(g):
    print("in f")
    return lambda: g(g)

fact_base = (lambda rec, n:
    1 if n==0 else n*rec(rec, n-1))

fact1 = lambda n: fact_base(fact_base, n)

# Simplify
fact_simple = (lambda rec, n:
    1 if n==0 else n*rec(rec)(n-1))

fact2 = fact_simple(fact_simple)

# Abstract the outer self application
mkrec = lambda f: f(f)

fact_abstract = mkrec(lambda rec, n:
    1 if n==0 else n*rec(rec)(n-1))

# Abstract the inner self application
mkrec_nice = (lambda g:
    mkrec(lambda rec:
        g(lambda y: rec(rec)(y))))

fact_abstract_inner = mkrec_nice(lambda rec, n:
    1 if n==0 else n*rec(n-1))

# Combine both mkrec
z_combinator = (lambda h:
    ( lambda rec: h(lambda y: rec(rec)(y)) )
    ( lambda rec: h(lambda y: rec(rec)(y)) ))
