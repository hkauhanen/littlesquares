#ifndef Language_H
#define Language_H

#include <vector>

class Language {
  std::vector <int> neighbour_indices;
  std::vector <int> feature_values;
  int index;

  public:
  Language(int i);
  int getIndex(void);
  std::vector <int> getNeighbourIndices(void);
  void addNeighbour(int);
  std::vector <int> getFeatureValues(void);
  int getFeatureValue(int);
  void setFeatureValues(std::vector <int>);
  void setFeatureValue(int, int);
  bool spinIsUp(int);
  bool spinIsDown(int);
  void spinUp(int);
  void spinDown(int);
  bool isIsolate(void);
};

#endif
