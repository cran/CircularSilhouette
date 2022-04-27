// LinearSil.cpp --- fast silhouette calculation on linear data clusters

#include "Sil.h"
using namespace Rcpp;

// [[Rcpp::export]]
double linearsil(const NumericVector & points,
                 const IntegerVector & cluster)
{
  //sort points
  auto n = points.size();

  std::vector<int> idx(n);
  std::vector<double> y(n);
  std::vector<int> x(n);

  for (int i = 0; i < n; i++) idx[i] = i;

  std::sort(idx.begin(), idx.end(),
            [&points](int i1, int i2) {
              return points[i1] < points[i2];});

  for (int i = 0; i < n; i++) {
    y[i] = points[idx[i]];
    x[i] = cluster[idx[i]];
  }

  //get cluster centers and number of points in each cluster
  NumericVector sizes(n);
  NumericVector centers(n);
  int size = 0;
  int current = x[0];
  int count = 0;
  double sum = 0;
  for(auto i = 0; i < n; i++){
    if(x[i] == current){
      sum += y[i];
      count ++;
    }
    else{
      current = x[i];
      sizes[size] = count;
//    sizes.push_back(count);
//    centers.push_back(sum / count);
      centers[size] = sum / count;
      size++;
      count = 1;
      sum = y[i];
    }
  }
//  sizes.push_back(count);
  sizes[size] = count;
//  centers.push_back(sum / count);
  centers[size] = sum / count;
//  if(sizes.size() == 1){
  if(size == 0){
    return NAN;
  }

  //std::cout << "sizes: ";
  //std::cout << sizes << " ";

  //std::cout << "centers: ";
  //std::cout << centers << " ";



  int offset = 0;
  double Asum = 0;
  double A;
  double B;
  double sil = 0;
  for(int i = 0; i <= size; i++){
    for(int j = 0; j < sizes[i]; j++){
      if(j == 0){
        for(int k = offset; k < offset + sizes[i]; k++){
          Asum = Asum + y[k] - y[offset];
        }
      }
      else{
        Asum = Asum + (y[j + offset] - y[j + offset- 1]) * (j - 1) - (y[j + offset] - y[j + offset- 1]) * (sizes[i] - 1 - j);
      }
      if(i == 0){
        B = centers[i + 1] - y[j + offset];
      }
 //     else if(i == sizes.size() - 1){
      else if(i == size){
        B = y[j + offset] - centers[i - 1];
      }
      else{
        B = fmin(fabs(y[j + offset] - centers[i - 1]),
                 fabs(centers[i + 1] - y[j + offset]));
      }
      A = Asum / (sizes[i] - 1);

      //std::cout << "A: " << A << " ";
      //std::cout << "B: " << B << " ";
      sil = sil + (B-A)/(fmax(B,A));
    }
    Asum = 0;
    offset += sizes[i];
  }
  return sil / x.size();

}
