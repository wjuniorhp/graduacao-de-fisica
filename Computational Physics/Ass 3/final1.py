import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy

#####################################

def step():

    r = rnd.random()

    if r<0.5:
        x = 1
    if r>0.5:
        x = -1

    return x

###########################################

n =100
m = 500
x2ave = np.zeros(n)
mx2ave = np.zeros(n)
x = np.zeros(m)
xtot = np.zeros(n)
vis = np.zeros((m,n))

for j in range(m):
    x[j] = 0

    xtot[1] = step()
    x2ave[1] = x2ave[1] + xtot[1]**2
    mx2ave[1] = x2ave[1]/m
    vis[j,xtot[1]]=1

    for i in range(2,n-1):

        b = step()

        if vis[j,i+b]==0:

            xtot[i] = xtot[i-1] + b

            x[j] = xtot[i]

            x2ave[i] = x2ave[i] + x[j]**2

            mx2ave[i] = x2ave[i]/m
            vis[i] = 1
        else:
            i=i-1

plt.plot(xtot,'-')
plt.title('displacement')
plt.show()

plt.plot(x2ave,'-')
plt.title('squared displacement')
plt.show()

plt.plot(mx2ave,'-')
plt.title('mean squared displacement')
plt.show()