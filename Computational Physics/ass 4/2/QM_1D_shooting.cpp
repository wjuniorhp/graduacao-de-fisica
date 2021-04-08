// Compute eigenfunctions and energies of 1D potential.
//
// ...this code can be *dramatically* improved.

#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using std::string;
using std::vector;
using std::cin; using std::cout; using std::endl;

double potential(double x); //function declaration; definition at the bottom

int main() {

  // spatial increment (you can play with this)
  double delta_x = 0.01;

  // initial energy and increment (you can play with this)
  double E = 1;
  double deltaE = 0.2;

  // the solution vector
  vector<double> psi;

  // the spatial range (you can play with this)
  double xLeft = -2;
  double xRight = 2;

  // Loop until we're done
  bool notdone = true;

  while(notdone) {
    // clear vector and push back the two initial conditions
    psi.clear();
    psi.push_back(0);
    psi.push_back(1e-3 * delta_x);

    // Generate the solution
    for (double x = xLeft; x <= xRight; x += delta_x) {
      // extract the last 2 values
      int vecsize = psi.size();
      double psiPrev = psi[vecsize - 1];
      double psiPrevPrev = psi[vecsize - 2];

      // compute the new value
      double psiNew = 2 * psiPrev
	- 2 * delta_x * delta_x * (E - potential(x)) * psiPrev
	- psiPrevPrev;
      // store it.
      psi.push_back(psiNew);
    }

    // write to a temporary file
    string datafilename = "/tmp/shooting.tmp";
    std::ofstream tempfile(datafilename.c_str());
    for (int i = 0; i != psi.size(); ++i) {
      tempfile << xLeft + i * delta_x << "\t" << psi[i] << endl;
    }
    tempfile.close();

    // write a gnuplot script
    string plotfilename = "/tmp/gnuplot.script";
    std::ofstream scriptfile(plotfilename.c_str());
    scriptfile << "plot \"" << datafilename << "\"" << endl;
    scriptfile.close();

    // plot the data
    string plotCommand = "gnuplot -persist " + plotfilename;
    system(plotCommand.c_str());

    // Ask user for interactive advice:
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

      // Take appropriate action
      if (choice == "1") {
	E += deltaE;
      } else if (choice == "2") {
	E -= deltaE;
      } else if (choice == "3") {
	deltaE *= 0.5;
	E += deltaE;
      } else if (choice == "4") {
	deltaE *= 0.5;
	E -= deltaE;
      } else if (choice == "5") {
	deltaE *= 2;
	E += deltaE;
      } else if (choice == "6") {
	deltaE *= 2;
	E -= deltaE;
      } else if (choice == "7") {
	notdone = false;
      } else {
	// bad data given.  ask again.
	valid = false;
      }
    }
    // delete all temporary files
    string delete_command = "rm " + datafilename + " " + plotfilename;
    system(delete_command.c_str());
  } // while (notdone)

  // write solution vector to file
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

//define potential funtion (you can play with this)
double potential(double x) { 
  static const double v_left = 5;
  static const double v_right = 7;
  if (x < -1) return v_left;
  if (x >  1) return v_right;
}
