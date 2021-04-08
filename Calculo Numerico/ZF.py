import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy
import math

def f(x):
    return(np.exp(-x)-x)

def f_(x):
    return(-np.exp(-x)-1)

def phi(x):
    return(np.exp(-x))

# ##############################################################################
# ######################## Metodos de Refinamento ##############################
# ##############################################################################

def bissec(A,B):
    a=A
    b=B
    e = 1e-8

    k = 0

    while (b-a>=e):
        x = (a+b)/2
        if (f(a)*f(x)>0):
            a = x
        elif (f(x)*f(b)>0):
            b = x
        k += 1

    print(k)
    return(x)


##Metodo Iterativo Linear
def MIL(A,B):
    a=A
    b=B
    e = 1e-8

    k = 0
    c = (a+b)/2
    x = 0.0
    x0 = e+1
    while (abs(x-x0)>e):
        x0 = c
        x = phi(x0)
        c = x
        k += 1

    print(k)
    return(x)


def Newton_Raph(A,B):
    a=A
    b=B
    e = 1e-8

    k = 0
    c = (a+b)/2
    x = 0.0
    x0 = e+1

    while (abs(x-x0)>e):
        x0 = c
        x = x0 - f(x0)/f_(x0)
        c = x
        k += 1


    print(k)
    return(x)




# ##############################################################################
# ################################ MAIN ########################################
# ##############################################################################

a = 0.5
b = 0.75

if (f(a)*f(b)<0.0):
    print('existe ao menos uma raiz!')
    print('Bissecao:',bissec(a,b))
    print('Met It Lin:',MIL(a,b))
    print('Newton-Raphson:',Newton_Raph(a,b))
##    print(bissec(a,b)-MIL(a,b))
else:
    print('Nao existe raiz no intervalo: f(',a,')*f(',b,') = ',f(a)*f(b),'>0')

x = np.arange(a,b,0.05)



##plt.plot(x,f(x))
##plt.plot(xzero,f(xzero),'ro')
##plt.show()