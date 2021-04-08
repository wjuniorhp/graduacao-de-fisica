import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rnd
import scipy
import math

# Compute eigenfunctions and energies of 1D potential.

# ...this code can be *dramatically* improved.


#######################################################################################
############################# global constants ########################################
#######################################################################################

## spatial increment (you can play with this)
delta_x = 0.01

## initial energy and increment (you can play with this)
E = 1.0
deltaE = 0.2

## the solution vector
psi = []
xplot = []


## the spatial range (you can play with this)
xLeft = -2.0
xRight = 2.0

## Loop until we're done
notdone = 1                                                                     #true or false

while(notdone):
    ## clear vector and push back the two initial conditions
    psi.append(0)
    psi.append(1e-3 * delta_x)

    ## Generate the solution
    for x in xrange(xLeft,xRight,delta_x):
        ## extract the last 2 values
        vecsize = np.size(psi)
        psiPrev = psi[vecsize - 1]
        psiPrevPrev = psi[vecsize - 2]

        ##creating the range of the x's axis:
        xplot.append(x)

        ## compute the new value
        psiNew = 2 * psiPrev
        - 2 * delta_x * delta_x * (E - potential(x)) * psiPrev
        - psiPrevPrev
        ## store it.
        psi.append(psiNew)



    ## plot the data
    plt.plot(xplot,psi)

    ## Ask user for interactive advice:
    cout << "Enter an option for energy increment:" << endl
    << "1. E -> E + delta_E" << endl
    << "2. E -> E - delta_E" << endl
    << "3. E -> E + delta_E / 2 (changes delta_E)" << endl
    << "4. E -> E - delta_E / 2 (changes delta_E)" << endl
    << "5. E -> E + 2 delta_E (changes delta_E)" << endl
    << "6. E -> E - 2 delta_E (changes delta_E)" << endl
    << "7. We're done!" << endl << endl
    << "Current E, delta_E: " << E << "\t" << deltaE << endl;

    bool valid = false;
    while (!valid) {
        cout << "Enter Choice: ";
        string choice;
        cin >> choice;
        valid = true;

        ## Take appropriate action
        if (choice == "1") {
            E += deltaE
        } else if (choice == "2") {
            E -= deltaE
        } else if (choice == "3") {
            deltaE *= 0.5
            E += deltaE
        } else if (choice == "4") {
            deltaE *= 0.5
            E -= deltaE
        } else if (choice == "5") {
            deltaE *= 2
            E += deltaE
        } else if (choice == "6") {
            deltaE *= 2
            E -= deltaE
        } else if (choice == "7") {
            notdone = 0
        } else {
            ## bad data given.  ask again.
            valid = false
        }
    }
 ## while (notdone)

## write solution vector to file
std::ofstream datafile("shooting.data");
datafile << "# E = " << E << endl
   << "# x     psi(x)      v(x)" << endl;
for (int i = 0; i != psi.size(); ++i) {
datafile << xLeft + i * delta_x << "\t"
     << psi[i] << "\t" << potential(xLeft + i * delta_x) << endl;
}
datafile.close();
return 0;
}

##define potential funtion (you can play with this)
double potential(double x) {
  static const double v_left = 5;
  static const double v_right = 7;
  if (x < -1) return v_left;
  if (x >  1) return v_right;
}
