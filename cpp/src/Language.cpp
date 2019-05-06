#include "Language.h"

Language::Language(int i) {
  index = i;
}

int Language::getIndex(void) {
  return index;
}

std::vector <int> Language::getNeighbourIndices(void) {
  return neighbour_indices;
}

void Language::addNeighbour(int i) {
  neighbour_indices.push_back(i);
}

std::vector <int> Language::getFeatureValues(void) {
  return feature_values;
}

int Language::getFeatureValue(int feat) {
  return feature_values[feat - 1];
}

void Language::setFeatureValues(std::vector <int> vals) {
  feature_values = vals;
}

void Language::setFeatureValue(int feat, int val) {
  feature_values[feat - 1] = val;
}

bool Language::spinIsUp(int feat) {
  if (feature_values[feat - 1] == 1) {
    return true;
  } else {
    return false;
  }
}

bool Language::spinIsDown(int feat) {
  if (feature_values[feat - 1] == 0) {
    return true;
  } else {
    return false;
  }
}

void Language::spinUp(int feat) {
  feature_values[feat - 1] = 1;
}

void Language::spinDown(int feat) {
  feature_values[feat - 1] = 0;
}

bool Language::isIsolate(void) {
  if (getNeighbourIndices().size() == 0) {
    return true;
  } else {
    return false;
  }
}
