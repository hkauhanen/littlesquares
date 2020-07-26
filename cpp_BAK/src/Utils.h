#ifndef UTILS_H
#define UTILS_H

#include <vector>
#include <cstdlib>
#include <iostream>

class Utils {
  public:
    int closestIndex(double, std::vector <double>);
    double min(std::vector <double>);
    int whichMin(std::vector <double>);
};

#endif
