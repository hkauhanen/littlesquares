#ifndef WORLD_H
#define WORLD_H

#include <vector>
#include <string>
#include <random>
#include <fstream>
#include "Language.h"
#include "RNG.h"
#include "DataFrame.h"
#include "SomeMath.h"
#include "Utils.h"

class World {
  protected:
    // Self-explanatory member variables
    int no_of_languages;
    int no_of_iterations;
    int no_of_features;
    double branching_rate;
    double voter_rate;
    std::ofstream outfile;
    std::vector <double> ingress_rates;
    std::vector <double> egress_rates;
    std::vector <double> theoretical_taus;
    std::vector <Language> languages;
    SomeMath math;

    // This DataFrame holds the tau hash that is used to invert the H(tau) function
    DataFrame* tau_hash;

    // Utilities
    Utils utils;

    // RNG generator for generating integers, so as to pick a random language
    // by referring to its index in the 'languages' vector. Here we need a
    // pointer, since we don't want the object to be constructed right away
    // (as we need to set the random integer distribution's limits in
    // LatticeWorld.cpp; see the constructor).
    RandomIntGenerator* randomLanguageGenerator;

    // Same for drawing random neighbours
    RandomIntUnspecifiedGenerator* randomNeighbourGenerator;

    // Same for features
    RandomIntGenerator* randomFeatureGenerator;

    // Same for feature values
    RandomIntGenerator* randomValueGenerator;

    // RNG generator for probabilities
    RandomProbGenerator* randomProbGenerator;

  public:
    Language randomLanguage(void);
    Language getLanguage(int);
    void step(void);
    void writeOut(int);
    double getFeatureDensity(int);
    double getIsoglossDensity(int);
    double invertHtau(double);
};

#endif
