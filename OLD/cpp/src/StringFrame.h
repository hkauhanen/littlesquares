/* Credit: CSV reader based on answer at https://stackoverflow.com/questions/48994605/csv-data-into-a-2d-array-of-integers */

#ifndef STRINGFRAME_H
#define STRINGFRAME_H

#include <fstream>
#include <sstream>
#include <string>
#include <vector>

class StringFrame {
  std::vector <std::vector <std::string>> array;

  public:
  StringFrame(std::string);
  int nrow(void);
  std::vector <std::string> getColumn(int);
  std::vector <std::string> getRow(int);
};

class NoFrillsCSVStringReader {
  public:
  std::vector <std::vector <std::string>> read(std::string);
};

#endif
