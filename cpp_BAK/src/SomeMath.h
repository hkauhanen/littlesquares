#ifndef SOMEMATH_H
#define SOMEMATH_H

#include <cmath>

class SomeMath {
  double pi_constant;

  public:
  SomeMath();
  double pi(void);
  double agm(double, double, int);
  double complete_elliptic_integral_of_the_first_kind(double, int);
};


#endif
