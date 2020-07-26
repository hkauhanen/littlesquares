#include "LatticeWorld.h"
#include "Language.h"
#include <cstdlib>
#include <iostream>



LatticeWorld::LatticeWorld(int n, DataFrame ing_eg, std::string taufile, int iter, double r, double q, std::string file) {
  // Set lattice side and number of features, number of iterations
  side = n;
  no_of_languages = n*n;
  no_of_features = ing_eg.nrow();
  no_of_iterations = iter;

  // Set tau hash
  tau_hash = new DataFrame(taufile);

  // Open outfile for writing and write CSV header
  outfile.open(file);
  outfile << "iteration,feature,no_of_iterations,no_of_languages,no_of_features,branching_rate,voter_rate,ingress_rate,egress_rate,horiz_ingress_rate,horiz_egress_rate,rho,sigma,tau,tau_predicted,tau_predicted_lambda\n";

  // Set rate parameters
  branching_rate = r;
  voter_rate = q;

  // Initialize random number generator engines
  randomLanguageGenerator = new RandomIntGenerator(1, no_of_languages);
  //randomNeighbourGenerator = new RandomIntGenerator(1, 4);
  randomNeighbourGenerator = new RandomIntUnspecifiedGenerator();
  randomFeatureGenerator = new RandomIntGenerator(1, no_of_features);
  randomValueGenerator = new RandomIntGenerator(0, 1);
  randomProbGenerator = new RandomProbGenerator();

  // Set ingress and egress rates from the infile, now a DataFrame in ing_eg
  ingress_rates = ing_eg.getColumn(1);
  egress_rates = ing_eg.getColumn(2);
  lambda_in = ing_eg.getColumn(3);
  lambda_eg = ing_eg.getColumn(4);

  // Calculate theoretical temperatures at stationary distribution (from analytical
  // solution) assuming a zero branching rate and zero horizontal error or the
  // lambda approximation
  theoretical_taus.reserve(no_of_features);
  theoretical_taus_lambda.reserve(no_of_features);
  double sum_lambda = 0;
  for (int i=0; i<no_of_features; i++) {
    sum_lambda = lambda_in[i] + lambda_eg[i];
    theoretical_taus.push_back(((1 - voter_rate)*(ingress_rates[i] + egress_rates[i]))/voter_rate);
    theoretical_taus_lambda.push_back(((1 - voter_rate)*(ingress_rates[i] + egress_rates[i]) + voter_rate*sum_lambda)/(voter_rate*(1 - sum_lambda)));
  }

  // Populate the world
  // (We use a vector to contain the languages of the LatticeWorld; even
  // though at the moment the number of languages stays constant, in future
  // applications we may want to relax this assumption.)
  languages.reserve(no_of_languages);
  for (int k=0; k<no_of_languages; k++) {
    // Construct a language
    int i = k+1;
    Language lang(i);

    // Set feature values to random
    lang.setFeatureValues(randomValueGenerator->get(no_of_features));

    // Set language's neighbours
    // First, we assign neighbours "blindly", and take care of lattice
    // boundaries after. To make sense of what follows, imagine that the
    // language indices from 1 to no_of_languages are laid out in a matrix,
    // with i growing by row. Languages in the "inside" of the matrix receive
    // neighbours trivially; the ones on the boundaries need special treatment.
    int north = i - side;
    int south = i + side;
    int west = i - 1;
    int east = i + 1;
    // Take care of north boundary
    if (north < 1) {
      north = no_of_languages + north;
    }
    // Take care of south boundary
    if (south > no_of_languages) {
      south = south - no_of_languages;
    }
    // Take care of west boundary
    if (i%side == 1) {
      west = i - 1 + side;
    }
    // Take care of east boundary
    if (i%side == 0) {
      east = i - side + 1;
    }
    lang.addNeighbour(north);
    lang.addNeighbour(east);
    lang.addNeighbour(south);
    lang.addNeighbour(west);

    // Push language to the vector of languages
    languages.push_back(lang);
  }
}
