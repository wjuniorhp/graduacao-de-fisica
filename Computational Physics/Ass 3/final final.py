import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy
import math

def disps (dx,dy,dz,m,r2,m2):

    r2.append(dx**2+dy**2+dz**2)
    m2.append((dx**2+dy**2+dz**2)/m)

m = 3
n = 50
x = np.zeros(m)
y = np.zeros(m)
z = np.zeros(m)


for j in range(m):
    visited = np.zeros(((n,n,n)))
    x[j] = n/2
    y[j] = n/2
    z[j] = n/2
    visited[x[j],y[j],z[j]] = 1

    nw = 0
    dx = 0
    dy = 0
    dz = 0
    r2 = []
    m2 = []

    while ((not(visited[x[j]-1,y[j],z[j]]) or not(visited[x[j]+1,y[j],z[j]])) or (not(visited[x[j],y[j]-1,z[j]]) or not(visited[x[j],y[j]+1,z[j]])) or (not(visited[x[j],y[j],z[j]-1]) or not(visited[x[j],y[j],z[j]+1]))):
        while True:
            r = 1.5 * rnd.random()

            if ((r<0.25) and not(visited[x[j]-1,y[j],z[j]])):
                x[j] = x[j]-1
                dx = dx +1
                disps (dx,dy,dz,m,r2,m2)
                nw = nw +1
                break
            elif ((r<0.50) and not(visited[x[j]+1,y[j],z[j]])):
                x[j] = x[j]+1
                dx = dx +1
                disps (dx,dy,dz,m,r2,m2)
                nw = nw +1
                break
            elif ((r<0.75) and not(visited[x[j],y[j]-1,z[j]])):
                y[j] = y[j]-1
                dy = dy + 1
                disps (dx,dy,dz,m,r2,m2)
                nw = nw +1
                break
            elif ((r<1.00) and not(visited[x[j],y[j]+1,z[j]])):
                y[j] = y[j]+1
                dy = dy + 1
                disps (dx,dy,dz,m,r2,m2)
                nw = nw +1
                break
            elif ((r<1.25) and not(visited[x[j],y[j],z[j]-1])):
                z[j] = z[j]-1
                dz = dz + 1
                disps (dx,dy,dz,m,r2,m2)
                nw = nw +1
                break
            elif ((r<1.50) and not(visited[x[j],y[j],z[j]+1])):
                z[j] = z[j]+1
                dz = dz + 1
                disps (dx,dy,dz,m,r2,m2)
                nw = nw +1
                break

        visited[x[j],y[j],z[j]]=1

        #if x[j]<=0  or x[j]>=n-1 or y[j]<=0 or y[j]>=n-1 or z[j]<=0 or z[j]>=n-1 :
        if nw>50:
            break


    plt.plot(m2,'-')

    print math.log(m2[nw-1])/(2*math.log(nw-1))

plt.show()