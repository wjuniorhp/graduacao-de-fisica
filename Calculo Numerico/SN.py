import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy
import math
import string

def b10 (num,base0):
    tam = len(num)
    ponto = num.find('.')-1
    s = 0
    k = 0

    for i in range(tam):
        if (num[i]!='.'):
            s += int(num[i])*base0**(ponto-k)
            k += 1
        print(i+1,k,num[i],ponto)

    return (s)

def b2 (num,base0):
    tam = len(num)
    ponto = num.find('.')-1
    s = []
    d = float(num)
    d = int(d)
    dp = float(num) - d
    i = 0

    while (d!=0):
        s += str(d%2)
        d = int(d/2)
    while (dp%1!=0):
        if(i==0): s+='.'
        s += str(int(dp*2))
        dp = dp*2
        i += 1
    s = reduce(lambda x, y: x+y, s)

    return (s)

l = (b2('11.25',10))
print(l)