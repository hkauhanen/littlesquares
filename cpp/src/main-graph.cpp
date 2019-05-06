#include <iostream>
#include <vector>
#include <string>
#include <cstdlib>
#include <getopt.h>
#include <time.h>

#include "GraphWorld.h"
#include "DataFrame.h"
#include "StringFrame.h"


int main(int argc, char *argv[]) {
  // Program name and version number
  std::string program = "littlesquares-graph";
  std::string version = "0.1";

  // Use a clock to measure execution time
  clock_t execstart, execfinish;
  execstart = clock();

  // Some self-explanatory variables
  int opt;
  int iterations = -1;
  int measurement_phase = -1;
  double branching_rate = -1;
  double voter_rate = -1;
  std::string infile = "";
  std::string outfile = "";
  std::string taufile = "";
  std::string graphfile = "";

  // Print welcome message
  std::cout << "-------------" << std::endl;
  std::cout << program << std::endl;
  std::cout << "version: " << version << std::endl;
  std::cout << "-------------" << std::endl;

  // Read in command-line arguments
  const char* const short_opts = "i:m:g:b:v:p:o:t:";
  const option long_opts[] = {
    {"iterations", required_argument, nullptr, 'i'},
    {"measurementphase", required_argument, nullptr, 'm'},
    {"graphfile", required_argument, nullptr, 'g'},
    {"branchingrate", required_argument, nullptr, 'b'},
    {"voterrate", required_argument, nullptr, 'v'},
    {"parameterfile", required_argument, nullptr, 'p'},
    {"outfile", required_argument, nullptr, 'o'},
    {"taufile", required_argument, nullptr, 't'}
  };

  while (true) {
    const auto opt = getopt_long(argc, argv, short_opts, long_opts, nullptr);
    if (opt == -1) {
      break;
    }
    switch (opt) {
      case 'i':
        iterations = std::atoi(optarg);
        break;
      case 'm':
        measurement_phase = std::atoi(optarg);
        break;
      case 'g':
        graphfile = optarg;
        break;
      case 'b':
        branching_rate = std::atof(optarg);
        break;
      case 'v':
        voter_rate = std::atof(optarg);
        break;
      case 'p':
        infile = optarg;
        break;
      case 'o':
        outfile = optarg;
        break;
      case 't':
        taufile = optarg;
        break;
      default:
        exit(EXIT_FAILURE);
    }
  }

  // Check all command-line arguments were given; if not, exit and complain
  std::string errstart = "ERROR: Invalid or no argument '";
  std::string errend = "' provided";
  bool failed = false;
  if (iterations <= 0) {
    std::cerr << errstart + "iterations" + errend << std::endl;
    failed = true;
  }
  if (measurement_phase <= 0) {
    std::cerr << errstart + "measurementphase" + errend << std::endl;
    failed = true;
  }
  if (graphfile.length() == 0) {
    std::cerr << errstart + "graphfile" + errend << std::endl;
    failed = true;
  }
  if (branching_rate < 0 || branching_rate > 1) {
    std::cerr << errstart + "branchingrate" + errend << std::endl;
  }
  if (voter_rate < 0 || voter_rate > 1) {
    std::cerr << errstart + "voterrate" + errend << std::endl;
    failed = true;
  }
  if (infile.length() == 0) {
    std::cerr << errstart + "parameterfile" + errend << std::endl;
    failed = true;
  }
  if (outfile.length() == 0) {
    std::cerr << errstart + "outfile" + errend << std::endl;
    failed = true;
  }
  if (taufile.length() == 0) {
    std::cerr << errstart + "taufile" + errend << std::endl;
    failed = true;
  }
  if (failed) {
    std::cerr << "See README.txt for instructions." << std::endl;
    std::cerr << "Exiting." << std::endl;
    exit(EXIT_FAILURE);
  }

  // If all command-line arguments were provided (and valid), echo these
  // back to the user
  std::cout << "Parameter values supplied:" << std::endl;
  std::cout << "\titerations:\t\t" << iterations << std::endl;
  std::cout << "\tmeasurementphase:\t" << measurement_phase << std::endl;
  std::cout << "\tbranchingrate:\t\t" << branching_rate << std::endl;
  std::cout << "\tvoterrate:\t\t" << voter_rate << std::endl;
  std::cout << "\tparameterfile:\t\t" << infile << std::endl;
  std::cout << "\tgraphfile:\t\t" << graphfile << std::endl;
  std::cout << "\ttaufile:\t\t" << taufile << std::endl;
  std::cout << "\toutfile:\t\t" << outfile << std::endl;
  std::cout << "Running..." << std::flush;

  // Read in CSV containing ingress and egress rates for features
  DataFrame ing_eg(infile);
  StringFrame neighbours(graphfile);

  // Construct world
  GraphWorld world(neighbours, ing_eg, taufile, iterations, 
      branching_rate, voter_rate, outfile);

  // Simulation
  for (int iteration=1; iteration<iterations+1; iteration++) {
    world.step();
    if (iteration >= iterations - measurement_phase) {
      world.writeOut(iteration);
    }
  }

  // If all went well, notify user and return
  execfinish = clock();
  std::cout << " done!" << std::endl;
  std::cout << iterations << " iterations completed in " 
    << ((double) (execfinish - execstart)) / CLOCKS_PER_SEC 
    << " seconds, output written to " << outfile << "." << std::endl;
  return 0;
}
