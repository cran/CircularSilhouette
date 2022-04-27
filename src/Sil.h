#include <Rcpp.h>
#include <vector>
#include <string>
#include <algorithm>
#include <iostream>
#include <iterator>
#include <cmath>
#include <ctime>
using namespace Rcpp;

/*
 * x and y both doubles representing coordinates
 * c is double representing circumference
 * returns circular distance
 */
double distance(double x, double y, double c);

NumericVector intra(NumericVector x, NumericVector y, double m, int id,
                    double l);

NumericVector inter(NumericVector x, NumericVector y, NumericVector b,
                    NumericVector id, double l, NumericVector center,
                    int k, int k1);

int nearestClusterIndex(double point, int i, NumericVector centers,
                        double c);

double getA(double point, int start, int size, NumericVector y, double c);


double getB(double point, int start, int size, NumericVector y, double c);

double linearsil(const NumericVector & points,
                 const IntegerVector & cluster);
