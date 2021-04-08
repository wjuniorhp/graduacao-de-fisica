import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy
import math

def prodmat (A,Y,k,j,m):
    soma = 0.0
    for l in range(m):
        soma = soma + A[k,l]*Y[l,j]

    return(soma)

def Euler(A,a,b,Y,h,m):
    n = int((b - a) / h)
    x = a
    for i in range(n):
        for k in range(m):
            Y[k,0] = Y[k,0] + h * prodmat(A,Y,k,0,m)
        x = x + h
        print(x)
        print(Y)


a = 0.0
b = 1.0
h = 0.1

A = np.loadtxt('matriz5.dat', comments='#')

m = int(np.size(A,axis=(0)))

Y0 = np.zeros([m,1])
Y0[:,0] = A[:,m]
A = scipy.delete(A,m,1)

Y = np.zeros([m,1])
Y[:,0] = Y0[:,0]

Euler(A,a,b,Y,h,m)