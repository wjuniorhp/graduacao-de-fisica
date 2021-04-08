import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy

n =100
m = 500
x2ave = np.zeros(n)
mx2ave = np.zeros(n)
x = np.zeros(m)
xtot = np.zeros(n)

for j in range(m):
    x[j] = 0
    for i in range(1,n):
        xtot[i] = 0
        r = rnd.random()
        if r<0.5:
            x[j] = x[j] + 1
        if r>0.5:
            x[j] = x[j] - 1
        xtot[i] = xtot[i] + x[j]
        x2ave[i] = x2ave[i] + x[j]**2
        mx2ave[i] = x2ave[i]/m

plt.plot(mx2ave,'-')
plt.show()

#plt.plot(mx2ave,'-')
#plt.show()