from matplotlib.pylab import *

nmax  = 100
steps = 1000
temp  = 2.4

spin = ones((nmax,nmax))
mag  = zeros(steps)

# loop over 'Monte Carlo time'
for n in range(steps):

    # for each step in time, loop over every cell and determine flip
    for i in range(nmax):

      # find index for right and left neighbors using periodic boundary conditions
      iplus1 = i + 1
      if i == nmax-1: iplus1 = 0
      imins1 = i - 1
      if i == 0: imins1 = nmax-1

      for j in range(nmax):

          # find index for top and bottom neighbors using periodic boundary conditions
          jplus1 = j + 1
          if j == nmax-1: jplus1 = 0
          jmins1 = j - 1
          if j == 0: jmins1 = nmax-1

          # energy of flip = E_after - E_before = 2.0*E_after
     	  Eflip = 2.0*spin[i,j] * (spin[iplus1,j]+spin[imins1,j]+spin[i,jplus1]+spin[i,jmins1])

	  # if Eflip is negative, flip is energetically favorable and always happens
	  if Eflip < 0.0:
		spin[i,j] = -spin[i,j]

	  # if Eflip is positive, use Boltzmann factor to determine if thermal energy drives a flip
	  elif exp(-Eflip/temp) > random():
		spin[i,j] = -spin[i,j]

    # after each pass over the grid, sum up spins to get bulk magnetization
    print type(spin)
    print len(spin)
    mag[n]  = sum(spin) / (nmax*nmax)
    print mag[n]



plot(mag)
ylabel('Magnetization')
xlabel('Time')
title('Temperature = %2.1f' %temp)
show()


