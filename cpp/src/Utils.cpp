#include "Utils.h"

int Utils::closestIndex(double x, std::vector <double> vec) {
  std::vector <double> dists = vec;
  for (int i=0; i<dists.size(); i++) {
    dists[i] = std::abs(dists[i] - x);
  }
  return whichMin(dists);
}

double Utils::min(std::vector <double> vec) {
  double min = vec[0];
  for (int i=1; i<vec.size(); i++) {
    if (vec[i] < min) {
      min = vec[i];
    }
  }
  return min;
}

int Utils::whichMin(std::vector <double> vec) {
  double min = vec[0];
  int minIndex = 0;
  for (int i=1; i<vec.size(); i++) {
    if (vec[i] < min) {
      min = vec[i];
      minIndex = i;
    }
  }
  return minIndex + 1;
}
