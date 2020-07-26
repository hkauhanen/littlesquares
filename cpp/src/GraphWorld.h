#ifndef GRAPHWORLD_H
#define GRAPHWORLD_H

#include <vector>
#include <string>
#include <random>
#include <fstream>
#include "World.h"
#include "Language.h"
#include "RNG.h"
#include "DataFrame.h"
#include "StringFrame.h"
#include "SomeMath.h"
#include "Utils.h"

class GraphWorld : public World {
  public:
  GraphWorld(StringFrame, DataFrame, std::string, int, double, std::string);
};

#endif
