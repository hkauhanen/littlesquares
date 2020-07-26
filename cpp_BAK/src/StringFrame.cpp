#include "StringFrame.h"

StringFrame::StringFrame(std::string filename) {
  NoFrillsCSVStringReader reader;
  array = reader.read(filename);
}

int StringFrame::nrow(void) {
  return array.size();
}

std::vector <std::string> StringFrame::getColumn(int index) {
  std::vector <std::string> out;
  out.reserve(array.size());
  for (int i=0; i<array.size(); i++) {
    out.push_back(array[i][index - 1]);
  }
  return out;
}

std::vector <std::string> StringFrame::getRow(int index) {
  return array[index - 1];
}

std::vector <std::vector <std::string>> NoFrillsCSVStringReader::read(std::string filename) {
  // Open infile stream
  std::ifstream file;
  file.open(filename);

  // Variables to hold line, cell and entire 2D array, respectively
  std::string line;
  std::string val;
  std::vector <std::vector <std::string>> array;

  // Read line at a time, parse into row vector, push row vector into 2D array
  while(std::getline(file, line)) {
    std::vector <std::string> v;
    std::stringstream s(line);
    while (std::getline(s, val, ',')) {
      v.push_back(val);
    }
    array.push_back(v);
  }

  return array;
}
