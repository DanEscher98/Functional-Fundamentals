
def qsortR(lista):
    if len(lista) == 0:
        return []
    else:
        piv = lista.pop()
        smaller = [a for a in lista if a<=piv]
        bigger  = [b for b in lista if b> piv]
        return qsortR(smaller) + [piv] + qsortR(bigger)
