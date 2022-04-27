#include "Sil.h"
using namespace Rcpp;

// [[Rcpp::export]]

/*
 * x is integer vector of clusters
 * y is double vector points
 * b is double vector of cluster sizes
 * c is double representing circumference
 * returns averaged silhouette score for given cluster assignment
 */
double slowsil( NumericVector x,  NumericVector y, NumericVector b, const double c) {
  if (b.size() == 1) {
    return -1;
  }
  int n = x.size();

  for (int i = 0;i < n;i++) {
    y[i] = fmod(y[i], c);
    //cout << y[i] << " ";
  }
  std::vector<int> X(2 * n);
  NumericVector Y(2 * n);
  std::vector<int> idx(n);
  for (int i = 0;i < n;i++)idx[i] = i;
  std::sort(idx.begin(), idx.end(),
            [&y](int i1, int i2) { return y[i1] < y[i2];});
  for (int i = 0;i < n;i++) {
    X[i] = x[idx[i]];
    Y[i] = y[idx[i]];
  }
  for (int i = 0;i < n;i++) {
    X[n + i] = X[i];
    Y[n + i] = Y[i] + c;
  }
  int a = 0;
  for (int i = 0;i < n;i++)
  {
    if (X[i] == 1 && X[fmod(i + n - 1, n)] != 1) {
      a = i;
      break;
    }
  }

  for (int i = 0; i < n; i++)
  {
    x[i] = X[i + a];
    y[i] = Y[i + a] - Y[a];
  }

  std::vector <int> order(b.size());
  int s = 0;
  for (int i = 0; i < b.size(); i++)
  {
    order[i] = x[s];
    s = s + b[order[i]-1];
  }
  s = 0;
  for (int i = 0; i < b.size(); ++i)
  {
    for (int j = 0;j < b[order[i]-1];++j) {
      x[s] = i+1;
      s++;
    }
  }

  NumericVector b1(b.size());
  b1 = b;
  for (int i = 0; i < b.size(); ++i)
  {
    b[i] = b1[order[i]-1];
    //cout <<b[i] << endl;
  }

  NumericVector indexes(b.size());
  NumericVector center(b.size());
  int cur = 0;
  for (int i = 0; i < b.size(); ++i)
  {
    double sum = 0;
    for (int j = 0;j < b[i];++j) {
      sum += y[cur + j];
    }
    center[i] = sum / b[i];
    indexes[i] = cur;
    cur = cur + b[i];
  }
  //now ready to compute sil for every point
  double sil = 0;

  for(int i = 0; i < n; i ++){
    double curPoint = y[i];
    int curCluster = x[i];
    int startIndex = indexes[curCluster - 1];
    int clusterSize = b[curCluster - 1];

    /* testing comments
     std::cout<<"cur point "<<curPoint<<"\n";
     std::cout<<"cur cluster "<<curCluster<<"\n";
     std::cout<<"start index "<<startIndex<<"\n";
     std::cout<<"cluster size "<<clusterSize<<"\n";
     */

    double inside = getA(curPoint, startIndex, clusterSize, y, c);

    //testing comments
    //std::cout<<"\n"<<inside<<" ";
    //std::cout<<"outside"<<"\n";


    int left=fmod(curCluster + b.size() - 2, b.size());;
    startIndex = indexes[left];
    clusterSize = b[left];

    /* testing comments
     std::cout<<"start index "<<startIndex<<"\n";
     std::cout<<"cluster size "<<clusterSize<<"\n";
     */

    double outside1 = getB(curPoint, startIndex, clusterSize, y, c);

    //std::cout<<outside<<" ";

    int right=fmod(curCluster, b.size());
      startIndex = indexes[right];
    clusterSize = b[right];

    /* testing comments
     std::cout<<"start index "<<startIndex<<"\n";
     std::cout<<"cluster size "<<clusterSize<<"\n";
     */

    double outside2 = getB(curPoint, startIndex, clusterSize, y, c);

    double outside=fmin(outside1,outside2);
    sil = sil + (outside - inside)/fmax(outside,inside);
  }

  return sil/x.size();
}



