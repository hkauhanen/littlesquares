#include "World.h"
#include "Language.h"
#include <cstdlib>
#include <iostream>



Language World::randomLanguage(void) {
  return languages[randomLanguageGenerator->get() - 1];
}

Language World::getLanguage(int i) {
  return languages[i - 1];
}

void World::step(void) {
  // Target a random language during this time step, one that has neighbours
  //Language target_language = randomLanguage();
  Language target_language(0);
  do {
    target_language = randomLanguage();
  } while (target_language.isIsolate());

  // For branching and voting, we also need a donor language (a neighbour)
  Language donor_language = languages[target_language.getNeighbourIndices()[randomNeighbourGenerator->get(1, target_language.getNeighbourIndices().size()) - 1] - 1];

  // Target a random feature (except in the case of branching; then we
  // copy all features)
  int target_feature = randomFeatureGenerator->get();

  // Conduct either a branching, a voter, an ingress or an egress event
  if (randomProbGenerator->get() < branching_rate) {
    // Branching
    target_language.setFeatureValues(donor_language.getFeatureValues());
  } else if (randomProbGenerator->get() < voter_rate) {
    // Voter
    if (donor_language.spinIsDown(target_feature)) {
      // Voter-ingress
      if (randomProbGenerator->get() < lambda_in[target_feature - 1]) {
        target_language.spinUp(target_feature);
      } else {
        target_language.spinDown(target_feature);
      }
    } else if (donor_language.spinIsUp(target_feature)) {
      // Voter-egress
      if (randomProbGenerator->get() < lambda_eg[target_feature - 1]) {
        target_language.spinDown(target_feature);
      } else {
        target_language.spinUp(target_feature);
      }
    }
  } else {
    if (target_language.spinIsDown(target_feature)) {
      if (randomProbGenerator->get() < ingress_rates[target_feature - 1]) {
        // Ingress
        target_language.spinUp(target_feature);
      }
    } else if (target_language.spinIsUp(target_feature)) {
      if (randomProbGenerator->get() < egress_rates[target_feature - 1]) {
        // Egress
        target_language.spinDown(target_feature);
      }
    }
  }

  // Update target language in the languages vector
  languages[target_language.getIndex() - 1] = target_language;
}

void World::writeOut(int iteration) {
  for (int f=0; f<no_of_features; f++) {
    outfile << iteration;
    outfile << ",";
    outfile << f+1;
    outfile << ",";
    outfile << no_of_iterations;
    outfile << ",";
    outfile << no_of_languages;
    outfile << ",";
    outfile << no_of_features;
    outfile << ",";
    outfile << branching_rate;
    outfile << ",";
    outfile << voter_rate;
    outfile << ",";
    outfile << ingress_rates[f];
    outfile << ",";
    outfile << egress_rates[f];
    outfile << ",";
    outfile << lambda_in[f];
    outfile << ",";
    outfile << lambda_eg[f];
    outfile << ",";
    double rho = getFeatureDensity(f+1);
    double sigma = getIsoglossDensity(f+1);
    outfile << rho;
    outfile << ",";
    outfile << sigma;
    outfile << ",";
    outfile << invertHtau(sigma/(2*rho*(1-rho)));
    outfile << ",";
    outfile << theoretical_taus[f];
    outfile << ",";
    outfile << theoretical_taus_lambda[f];
    outfile << "\n";
  }
}

double World::getFeatureDensity(int feat) {
  int ups = 0;
  for (int i=0; i<languages.size(); i++) {
    if (languages[i].getFeatureValue(feat) == 1) {
      ups = ups + 1;
    }
  }
  double ret = ((double) ups)/((double) languages.size());
  return ret;
}

double World::getIsoglossDensity(int feat) {
  int isoglosses = 0;
  int interfaces = 0;
  for (int i=0; i<languages.size(); i++) {
    std::vector <int> neighbour_indices = languages[i].getNeighbourIndices();
    for (int j=0; j<neighbour_indices.size(); j++) {
      interfaces = interfaces + 1;
      if (languages[i].getFeatureValue(feat) != languages[neighbour_indices[j] - 1].getFeatureValue(feat)) {
        isoglosses = isoglosses + 1;
      }
    }
  }
  double ret = ((double) isoglosses)/((double) interfaces);
  return ret;
}

double World::invertHtau(double htau) {
  int closest_index = utils.closestIndex(htau, tau_hash->getColumn(2));
  return tau_hash->getColumn(1)[closest_index - 1];
}
