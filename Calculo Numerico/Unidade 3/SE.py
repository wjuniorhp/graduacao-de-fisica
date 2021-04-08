import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy
import math

#Determinante
def det(A):
    return (np.linalg.det(A))

#Norma - 0=> max colunas; 1=> max linhas; 2=> norma euclidiana
def norma(A,m,n,tipo):
    soma=[]
    if(tipo==0):
        for i in range(n):
            soma.append(np.sum(A[:,i]))
        return(np.max(soma[i]))

    if(tipo==1):
        for i in range(m):
            soma.append(np.sum(A[i,:]))
        return(np.max(soma[i]))

    if(tipo==2):
        s=0.0
        for i in range(m):
            for j in range(n):
                s = s + A[i,j]**2
##        print(s)
        return(s**(0.5))

#Teste de convergencia pro metodo de Gauss Jacobi
def testeconv(A,n):
    alpha=np.zeros(n)
    for k in range(n):
        for j in range(n):
            if (j!=k):
                alpha[k]=alpha[k]+abs(A[k,j])
        alpha[k]=1.0/abs(A[k,k])*alpha[k]
        if (alpha[k]>=1):
            print('Alfa',k,'=',alpha[k])
            return (0)
            break
    return (1)

#Teste de Sassenfeld de convergencia para o metodo de Gauss Seidel
def sassenfeld(A,n):
    beta=np.zeros(n)
    for k in range(n):
        for j in range(n):
            if (j<k):
                beta[k]=beta[k]+abs(A[k,j])*beta[j]
            if (j>k):
                beta[k]=beta[k]+abs(A[k,j])
        beta[k]=1.0/abs(A[k,k])*beta[k]
        if (beta[k]>=1):
            print('Beta',k,'=',beta[k])
            return (0)
            break
    return (1)

#Retro-solucao de um SE escalonado
def RetroSol(A,b,n):
    x=np.zeros(n)
    x[n-1] = b[n-1]/A[n-1,n-1]
    for k in range(n-2,-1,-1):
        for j in range(k+1,n):
            x[k] = x[k] + A[k,j]*x[j]
        x[k] = 1.0/A[k,k] * (b[k]-x[k])
    print(x)

#Pivotamento Parcial
def pivot (A,b,n,k):
    C = np.zeros(n)
    cc = 0.0
    for i in range(k,n):
        if (abs(A[i,k])>abs(A[k,k])):
            C[:] = A[i,:]
            A[i,:] = A[k,:]
            A[k,:] = C[:]
            cc = b[i]
            b[i] = b[k]
            b[k] = cc


# ##############################################################################
# ###################### Metodos de Solucao SE #################################
# ##############################################################################

# Metodos iterativos

def GaussJac(A,xo,b,n):
    e = 1e-2
    x = np.zeros(n)
    em = np.zeros([n,1])
    print(xo)
    for i in range (n):
        for j in range(n):
            if (i!=j):
                x[i] = x[i] + A[i,j]*xo[j]
        x[i] = 1.0/A[i,i]*(b[i]-x[i])
        em[i,0] = x[i] - xo[i]
    if (norma(em,n,1,2)<=e):
        print(x)
    elif(norma(em,n,1,2)>e):
        GaussJac(A,x,b,n)


def GaussSeid(A,xo,b,n):
    e = 1e-2
    x = np.zeros(n)
    em = np.zeros([n,1])
    print(xo)
    for i in range (n):
        for j in range(n):
            if (j<i):
                x[i] = x[i] + A[i,j]*x[j]
            if (j>i):
                x[i] = x[i] + A[i,j]*xo[j]
        x[i] = 1.0/A[i,i]*(b[i]-x[i])
        em[i,0] = x[i] - xo[i]
    if (norma(em,n,1,2)<=e):
        print(x)
    elif(norma(em,n,1,2)>e):
        GaussSeid(A,x,b,n)

#Metodos Diretos
def ElimGauss (A,b,n):
    S = np.zeros([n,n])
    bb = np.zeros(n)
    S[:,:] = A[:,:]
    bb[:] = b[:]
    for k in range(n-1):
        pivot(S,bb,n,k)
        for i in range(k+1,n):
            m = S[i,k]/S[k,k]
            for j in range(n):
                S[i,j] = S[i,j] - m*S[k,j]
            bb[i] = bb[i] - m*bb[k]

    RetroSol(S,bb,n)


# ##############################################################################
# ################################ MAIN ########################################
# ##############################################################################
##A is a nxn matrix in A x = b

A = np.loadtxt('NSmatrix.dat', comments='#')

n = int(np.size(A,axis=(0)))

b = A[:,n]
A = scipy.delete(A,n,1)
xo = np.zeros(n)
xo[:]=0.5

if (det(A)!=0):
    if (testeconv(A,n)==1):
        print('Metodo de Gauss Jacobi:')
        GaussJac(A,xo,b,n)
    if (sassenfeld(A,n)==1):
        print('Metodo de Gauss Seidel:')
        GaussSeid(A,xo,b,n)
    else:
        print('nao converge')

    print('Metodo de Eliminacao de Gauss:')
    ElimGauss(A,b,n)
else:
    print('nao convergiu. det=0!')
