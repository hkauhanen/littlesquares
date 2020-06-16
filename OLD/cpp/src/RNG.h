#ifndef RNG_H
#define RNG_H

#include <vector>
#include <random>

class RandomIntGenerator {
  std::mt19937 mt_eng;
  std::uniform_int_distribution<int> rand_dist;

  public:
  RandomIntGenerator(int min, int max) : mt_eng{std::random_device{}()}, rand_dist(min, max) {}
  int get(void);
  std::vector <int> get(int);
};

class RandomIntUnspecifiedGenerator {
  std::mt19937 mt_eng;
  std::uniform_int_distribution<int> rand_dist;

  public:
  RandomIntUnspecifiedGenerator() : mt_eng{std::random_device{}()} {}
  int get(int, int);
  std::vector <int> get(int);
};

class RandomProbGenerator {
  std::mt19937 mt_eng;
  std::uniform_real_distribution<double> rand_dist;

  public:
  RandomProbGenerator() : mt_eng{std::random_device{}()}, rand_dist(0,1) {}
  double get(void);
  std::vector <double> get(int);
};

#endif
