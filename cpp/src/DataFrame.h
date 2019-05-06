/* Credit: CSV reader based on answer at https://stackoverflow.com/questions/48994605/csv-data-into-a-2d-array-of-integers */

#ifndef DATAFRAME_H
#define DATAFRAME_H

#include <fstream>
#include <sstream>
#include <string>
#include <vector>

class DataFrame {
  std::vector <std::vector <double>> array;

  public:
  DataFrame(std::string);
  int nrow(void);
  std::vector <double> getColumn(int);
  std::vector <double> getRow(int);
};

class NoFrillsCSVReader {
  public:
  std::vector <std::vector <double>> read(std::string);
};

#endif
