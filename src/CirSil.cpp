#include <Rcpp.h>
#include <vector>
#include <string>
#include <algorithm>
#include <iostream>
#include <iterator>
#include <cmath>
#include <ctime>
#include "Sil.h"
using namespace Rcpp;

/*
 * x and y both doubles representing coordinates
 * c is double representing circumerence
 * returns circular distance
 */
double distance(double x, double y, double c){
  return fmin(fabs(x - y), c - fabs(x-y));
}

NumericVector intra(NumericVector x, NumericVector y, double m, int id, double l) {
    NumericVector tra(m);
    NumericVector u(2 * m);
    if (m == 1)
    {
        tra[0] = DBL_MAX;
        return tra;
    }
    for (int i = 0;i < m;++i) {
        u[i] = y[id + i] - y[id];
        //cout << u[i] << " ";
    }
    if (u[m - 1] - u[0] < l / 2) {
        double A = 0;
        for (int i = 0;i < m;++i) {
            A += u[i];
        }
        for (int n = 0;n < m;n++) {
            tra[n] = A / (m - 1);
            if (n < m - 1) { A += (2 * (n + 1) - m) * (u[n + 1] - u[n]); }
        }
    }
    else {
        for (int i = 0;i < m;++i) {
            u[i + m] = u[i] + l;
            //cout << u[2*i] << " "<<u[2*i+1]<<" ";
        }
        int h = 0;
        double A = 0;
        while (l / 2 > u[h]) {
            A += u[h];
            h++;
        }
        for (int i = h;i < m;i++) {
            A += l - u[i];
        }
        tra[0] = A / (m - 1);
        int f;
        for (int n = 1;n < m;n++) {
            f = h;
            while (l / 2 + u[n] > u[h]) {
                A += 2 * u[h] - l;
                h++;
            }
            A += (2 * n + m - 2 * h) * u[n] + (2 * f - m - 2 * n) * u[n - 1];
            tra[n] = A / (m - 1);
        }
    }
    //for(int i=0;i<m;i++)cout << tra[i] << endl;
    return tra;
}


NumericVector inter(NumericVector x, NumericVector y, NumericVector b, NumericVector id, double l, NumericVector center, int k, int k1) {
    NumericVector ter(b[k]);
    NumericVector u(b[k]);
    NumericVector v(b[k1]);
    for (int i = 0;i < b[k];++i) {
        u[i] = y[id[k] + i] - y[id[k]];
        //cout << u[i] << " ";
    }
    int SC2 = 0;
    int h;
    double B;
    for (int n = 0;n < b[k];n++) {
        //cout << n <<  " " <<y[n + id[k]]  << endl;
        int f;
        if (fmod(y[id[k1] + 0] - y[id[k]] + l, l) < fmod(y[n + id[k]] - y[id[k]] + l / 2, l) && fmod(y[n + id[k]] - y[id[k]] + l / 2, l) < fmod(y[id[k1] + b[k1] - 1] - y[id[k]] + l, l))
        {
            //cout << n << " ";
            if (SC2 == 0) {
                for (int i = 0;i < b[k1];++i) {
                    v[i] = fmod(y[id[k1] + i] - y[id[k]] + l, l);
                    //cout << v[i] << " ";
                }
                SC2 = 1;
                h = 0;
                B = 0;
                while (l / 2 > v[h]) {
                    B += v[h];
                    h++;
                }
                for (int i = h;i < b[k1];i++) {
                    B += l - v[i];
                }

            }
            f = h;
            if (n == 0)ter[0] = (B / b[k1]);
            else {
                while (l / 2 + u[n] > v[h]) {
                    B += 2 * v[h] - l;
                    h++;
                }
                //cout << f<<" "<<h << endl;
                B += (b[k1] - 2 * h) * u[n] + (2 * f - b[k1]) * u[n - 1];
                ter[n] = B / b[k1];
            }
        }
        else {
            ter[n] = distance(y[id[k] + n], center[k1], l);
        }
        //cout << B << " ";
    }
    return ter;
}

/*
 * point is the points coordinate
 * i is the point's cluster ID
 * centers is double vector of centers
 * c is the circumference
 * returns starting index of the nearest other cluster to a particular point
 */
int nearestClusterIndex(double point, int i, NumericVector centers, double c){
  int k = centers.size();
  if(k < 3){
    if(i == 1){
      return 1;
    }
    else return 0;
  }
  else if(i == 1)
  {
    if(distance(point, centers[1], c) < distance(point, centers[k - 1], c)){
      return 1;
    }
    else{
      return k - 1;
    }
  }
  else if(i == k){
    if(distance(point, centers[k-2], c) < distance(point, centers[0], c)){
      return k-2;
    }
    else{
      return 0;
    }
  }
  else{
    if(distance(point, centers[i], c) < distance(point, centers[i-2], c)){
      return i;
    }
    else{
      return i-2;
    }
  }
}

/*
 * point is current point coordinate
 * start is start index
 * size is size of cluster, pre-computed borders prevents checking every point.
 * y is double vector of points
 * c is the circumference
 * returns value of A/B for given point
 */
double getA(double point, int start, int size, NumericVector y, double c){
  if(size == 1){
    return DBL_MAX;
  }
  double A = 0;
  for(int i = start; i < start + size; i++){
    A = A + distance(point, y[i], c);
  }
  A = A / (size - 1);
  return A;
}

double getB(double point, int start, int size, NumericVector y, double c){
  double B = 0;
  for(int i = start; i < start + size; i++){
    B = B + distance(point, y[i], c);
  }
  B = B / (size);
  return B;
}

