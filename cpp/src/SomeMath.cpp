#include "SomeMath.h"

SomeMath::SomeMath() {
  pi_constant = M_PI;
}

double SomeMath::pi(void) {
  return pi_constant;
}

double SomeMath::agm(double x, double y, int iterations) {
  double a [iterations];
  double g [iterations];
  a[0] = x;
  g[0] = y;
  for (int i=1; i<iterations; i++) {
    a[i] = 0.5*(a[i-1] + g[i-1]);
    g[i] = sqrt(a[i-1]*g[i-1]);
  }
  // Take mean of a[iterations-1] and g[iterations-1] as representative
  // of the theoretical value to which both series converge
  double ret = 0.5*(a[iterations-1] + g[iterations-1]);
  return ret;
}

double SomeMath::complete_elliptic_integral_of_the_first_kind(double k, int iterations) {
  return (0.5*pi())/agm(1, sqrt(1 - (k*k)), iterations);
}
