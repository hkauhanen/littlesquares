#include "RNG.h"

int RandomIntGenerator::get(void) {
  return rand_dist(mt_eng);
}

int RandomIntUnspecifiedGenerator::get(int min, int max) {
  std::uniform_int_distribution<int> rand_dist(min, max);
  return rand_dist(mt_eng);
}

std::vector <int> RandomIntGenerator::get(int howmany) {
  std::vector <int> out;
  for (int i=0; i<howmany; i++) {
    out.push_back(rand_dist(mt_eng));
  }
  return out;
}

double RandomProbGenerator::get(void) {
  return rand_dist(mt_eng);
}

std::vector <double> RandomProbGenerator::get(int howmany) {
  std::vector <double> out;
  for (int i=0; i<howmany; i++) {
    out.push_back(rand_dist(mt_eng));
  }
  return out;
}
