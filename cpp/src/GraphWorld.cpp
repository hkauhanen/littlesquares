#include "GraphWorld.h"
#include "Language.h"
#include <cstdlib>
#include <iostream>



GraphWorld::GraphWorld(StringFrame neighbours, DataFrame ing_eg, std::string taufile, int iter, double r, std::string file) {
  // Set number of features, number of iterations
  no_of_languages = neighbours.nrow();
  no_of_features = ing_eg.nrow();
  no_of_iterations = iter;

  // Set tau hash
  tau_hash = new DataFrame(taufile);

  // Open outfile for writing and write CSV header
  outfile.open(file);
  outfile << "iteration,feature,no_of_iterations,no_of_languages,no_of_features,branching_rate,voter_rate,ingress_rate,egress_rate,horiz_ingress_rate,horiz_egress_rate,rho,sigma,tau,tau_predicted\n";

  // Set rate parameters
  branching_rate = r;

  // Initialize random number generator engines
  randomLanguageGenerator = new RandomIntGenerator(1, no_of_languages);
  randomNeighbourGenerator = new RandomIntUnspecifiedGenerator();
  randomFeatureGenerator = new RandomIntGenerator(1, no_of_features);
  randomValueGenerator = new RandomIntGenerator(0, 1);
  randomProbGenerator = new RandomProbGenerator();

  // Set ingress and egress rates from the infile, now a DataFrame in ing_eg
  ingress_rates = ing_eg.getColumn(1);
  egress_rates = ing_eg.getColumn(2);
  lambda_in = ing_eg.getColumn(3);
  lambda_eg = ing_eg.getColumn(4);
  voter_rates = ing_eg.getColumn(5);

  // Calculate theoretical temperatures at stationary distribution (from analytical
  // solution) assuming a zero branching rate and zero horizontal error or the
  // lambda approximation
  theoretical_taus.reserve(no_of_features);
  predictTaus();
  /*
  theoretical_taus_lambda.reserve(no_of_features);
  double sum_lambda = 0;
  for (int i=0; i<no_of_features; i++) {
    sum_lambda = lambda_in[i] + lambda_eg[i];
    theoretical_taus.push_back(((1 - voter_rates[i])*(ingress_rates[i] + egress_rates[i]))/voter_rates[i]);
    theoretical_taus_lambda.push_back(((1 - voter_rates[i])*(ingress_rates[i] + egress_rates[i]) + voter_rates[i]*sum_lambda)/(voter_rates[i]*(1 - sum_lambda)));
  }
  */

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
    std::string neighbours_as_string = neighbours.getColumn(3)[i-1];
    if (neighbours_as_string != "NA") {
      std::string extracted_token = "";
      std::string delimiter = ":";
      while (extracted_token != neighbours_as_string) {
        extracted_token = neighbours_as_string.substr(0, neighbours_as_string.find_first_of(delimiter));
        neighbours_as_string = neighbours_as_string.substr(neighbours_as_string.find_first_of(delimiter) + 1);
        lang.addNeighbour(std::stoi(extracted_token));
      }
    }

    // Push language to the vector of languages
    languages.push_back(lang);
  }
}
