import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy
import math

def exaty(x):
    return (2*x**2-x-0.5)

def f(x,y):
    return ((2*y+x+1.0)/x)

def Euler(x,y,h,n):
      for i in range(1,n):
          y[i] = y[i-1] + h * f(x[i-1], y[i-1])
          x[i] = x[i-1] + h

a = 1.0
b = 1.8
y0 = 0.5

ha = 0.2
hb = 0.1

na = int((b - a) / ha) + 1
nb = int((b - a) / hb) + 1

ya = np.zeros(na)
yb = np.zeros(nb)
xa = np.zeros(na)
xb = np.zeros(nb)

ya[0] = y0
yb[0] = y0
xa[0] = a
xb[0] = a

Euler(xa,ya,ha,na)
Euler(xb,yb,hb,nb)

print (xb)
print (yb)

plt.plot(xa,ya,'b')
plt.plot(xb,yb,'g')
plt.plot(xb,exaty(xb),'r',linewidth=2.0)
plt.show()