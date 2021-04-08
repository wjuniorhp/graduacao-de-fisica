import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy
import math


#######################################################################################
############################# global constants ########################################
#######################################################################################
J_ising = 1.                                    # J in the Ising model (+1 or -1)
linear_sites = 10                              # number of lattice sites in one direction
dimension = 2
num_sites = int(linear_sites * linear_sites)

## number of different energies
num_energies = int(2 * dimension * num_sites + 1)

## # of Monte Carlo steps (mcs)
num_mcs = 1000
kT = 2.25                                       #temperature (in energy units)
dist_metropolis = np.zeros(num_energies)        # energy distribution at kT from
                                                # importance sampling (Metropolis)
nt = 35
M = np.zeros(nt)
mag = np.zeros(num_mcs)
temperature = np.zeros(nt)
mi = 1.0
H_ising = 1.0
Efinal = np.zeros(num_mcs)
## Find the energy distribution from a Markov chain of configurations
config_metropolis = np.zeros(num_sites)         # current configuration


#######################################################################################
################################ functions ############################################
#######################################################################################

def calculate_energy (configuration):
    nearest = 0
    energy = 0.0
    m = 0


    ## go through 2d lattice with free boundary conditions
    for i in range(linear_sites - 1):

        for j in range(linear_sites - 1):
            Id = i + j*linear_sites
            # x direction
            nearest = Id + 1
            energy += - J_ising * float(configuration[Id]*configuration[nearest])
            #print configuration[Id]

            # y direction;
            nearest = Id + linear_sites
            energy += - J_ising * float(configuration[Id]*configuration[nearest])

        energy += - mi*H_ising*float(configuration[Id])

    return (energy)


#######################################################################################

##def calculate_mag (configuration):
##    nearest = 0
##    energy = 0.0
##    m = 0.0
##    #print configuration
##    for i in range(num_sites):
##        m += float(configuration[i])*mi
##
##    return (m/num_sites)


#######################################################################################
#################################### main code ########################################
#######################################################################################

## generate a random configuration to start and find its energy
for i in range(num_sites):
    random = rnd.random()
    if (random > 0.5):
      config_metropolis[i] = -1     # spin down

    else:
      config_metropolis[i] = +1     # spin up

    energy0 = calculate_energy (config_metropolis);

#temp = 1.0
## Take num_mcs Monte Carlo steps (mcs)
#for k in range(nt):
 #   temp += 0.1
    for step in range(num_mcs):

        for i in range(num_sites):          # Entire loop is only one mcs

            # pick a random lattice site
            random = rnd.random()
            Id = int(random * num_sites)    # from 0 to num_sites

            # flip that spin (i.e., if +/- 1, change to -/+ 1)
            config_metropolis[Id] *= -1.0
            energy = calculate_energy(config_metropolis)  # new energy
            delta_energy = energy - energy0

            #decide whether to accept or reject the new configuration
            random =  rnd.random()

            if ( (delta_energy > 0.0) and (random > math.exp(-delta_energy/kT)) ):
            # reject the new configuration: flip the spin back
                config_metropolis[Id] *= -1

            else:
                energy0 = energy  # accept the new configuration
        Efinal[step] = energy

    mag[step] = sum(config_metropolis)/num_sites

    #print len(dist_metropolis)
    #print dimension * num_sites + int(energy0)

    ## add to distribution
    dist_metropolis[dimension * num_sites + int(energy0)] += 1.0
    ##        if ((step < 100) or (step % 100 == 0)):
    ##            print step, energy0
    #print calculate_mag(config_metropolis)

#    M[k] = np.average(mag)
#    temperature[k] = temp
print Efinal
## normalize the distribution
for i in range(num_energies):
    dist_metropolis[i] /= num_mcs

    ## output distributions of energies P(E)
##    for i in range(num_energies,2):
##        print dist_metropolis[i]

#print np.average(config_metropolis)
plt.plot(Efinal)
plt.show()