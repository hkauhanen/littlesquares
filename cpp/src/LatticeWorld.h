#ifndef LatticeWorld_H
#define LatticeWorld_H

#include <vector>
#include <string>
#include <random>
#include <fstream>
#include "World.h"
#include "Language.h"
#include "RNG.h"
#include "DataFrame.h"
#include "SomeMath.h"
#include "Utils.h"

class LatticeWorld : public World {
  // Self-explanatory member variables
  int side;

  public:
  LatticeWorld(int, DataFrame, std::string, int, double, std::string);
};

#endif
