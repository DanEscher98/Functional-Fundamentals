def digital_root(n):
    def aux(a):
        ans = []
        print(a, a%10)
        while a > 0:
            ans.append(a % 10)
            a = int(a / 10)
    n = sum(aux(n))
    if n < 10:
        return n
    else:
        digital_root(n)

if __name__=='__main__':
    print(digital_root(12102394872135345678))
