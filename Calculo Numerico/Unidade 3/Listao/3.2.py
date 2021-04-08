import numpy as np
import scipy
import math

#Determinante
def det(A):
    return (np.linalg.det(A))

#Retro-solucao de um SN escalonado
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
# ####################### Metodo de Solucao SN #################################
# ##############################################################################

def ElimGauss (A,b,n):
    S = np.zeros([n,n])
    bb = np.zeros(n)
    S[:,:] = A[:,:]
    bb[:] = b[:]
    for k in range(n-1):
        pivot(S,bb,n,k)
        for i in range(k+1,n):
            m = S[i,k]/S[k,k]
            for j in range(i-1,i+1):
                S[i,j] = S[i,j] - m*S[k,j]
            bb[i] = bb[i] - m*bb[k]

    RetroSol(S,bb,n)


# ##############################################################################
# ################################ MAIN ########################################
# ##############################################################################
##A is a nxn matrix in A x = b

n = 10
A = np.zeros([n,n])
b = np.zeros(n)

A[0,0:2] = [2,-1]
A[n-1,n-2:n] = [-1,2]
b[0] = 1.0
for i in range(1,n-1):
    for j in range(n):
        if(abs(i-j)>1):
            A[i,j] = 0.0
        elif(i==j):
            A[i,j-1] = -1.0
            A[i,j] = 2.0
            A[i,j+1] = -1.0


if (det(A)!=0):
    print('Resultado pelo Metodo de Eliminacao de Gauss:')
    ElimGauss(A,b,n)
else:
    print('nao convergiu. det=0!')
