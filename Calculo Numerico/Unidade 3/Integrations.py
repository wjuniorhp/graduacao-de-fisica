import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy
import math

def f(x):
    c=2.997925e10
    h=6.6256e-27
    k=1.38054e-16
    T=2000
    pi=math.pi
    return ((2*pi*h*c**2)/(x**5*(math.exp((h*c)/(k*x*T))-1)))

# ##############################################################################
# ###################### Integra??es Num?ricas #################################
# ##############################################################################

# Grau 1 (2 pontos)
def trapezS(a,b):
    h=b-a
    sum = f(a)+f(b)
    return (h/2*sum)

def trapezR(a,b,n):
    h=(b-a)/n
    sum = f(a)+f(b)
    for i in range (1,n-1):
        x = a + i*h
        sum = sum + 2*f(x)
    return (h/2*sum)

#Grau 2 (3 pontos)
def simp13(a,b):
    h=(b-a)/2
    sum = f(a)+4*f(a+h)+f(b)
    return (h/3*sum)

def simp13R(a,b,n):
    h=(b-a)/n
    sum = f(a)+f(b)
    for i in range (1,n-1):
        x = a + i*h
        if(i%2==0):
            sum = sum + 2*f(x)
        else:
            sum = sum + 4*f(x)
    return (h/3*sum)

#Grau 3 (4 pontos)
def simp38(a,b):
    h=(b-a)/3
    sum = f(a)+3*f(a+h)+3*f(a+2*h)+f(b)
    return (3*h/8*sum)

def simp38R(a,b,n):
    h=(b-a)/n
    sum = f(a)+f(b)
    for i in range (1,n-1):
        x = a + i*h
        if(i%3==0):
            sum = sum + 2*f(x)
        else:
            sum = sum + 3*f(x)
    return (3*h/8*sum)

# ##############################################################################
# ################################ MAIN ########################################
# ##############################################################################

a=3933.666e-8
b=5895.923e-8

n=10000                        ##n is the number of points
## ou...
##h= 0.01
##n=int((b-a)/h)

##x1=(b-a)/2

I = trapezR(a,b,n)

print(I)