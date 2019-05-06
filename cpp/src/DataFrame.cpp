#include "DataFrame.h"

DataFrame::DataFrame(std::string filename) {
  NoFrillsCSVReader reader;
  array = reader.read(filename);
}

int DataFrame::nrow(void) {
  return array.size();
}

std::vector <double> DataFrame::getColumn(int index) {
  std::vector <double> out;
  out.reserve(array.size());
  for (int i=0; i<array.size(); i++) {
    out.push_back(array[i][index - 1]);
  }
  return out;
}

std::vector <double> DataFrame::getRow(int index) {
  return array[index - 1];
}

std::vector <std::vector <double>> NoFrillsCSVReader::read(std::string filename) {
  // Open infile stream
  std::ifstream file;
  file.open(filename);

  // Variables to hold line, cell and entire 2D array, respectively
  std::string line;
  std::string val;
  std::vector <std::vector <double>> array;

  // Read line at a time, parse into row vector, push row vector into 2D array
  while(std::getline(file, line)) {
    std::vector <double> v;
    std::stringstream s(line);
    while (std::getline(s, val, ',')) {
      v.push_back(std::stod(val));
    }
    array.push_back(v);
  }

  return array;
}
