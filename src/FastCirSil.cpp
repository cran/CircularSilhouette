#include "Sil.h"
using namespace Rcpp;

/*
 * x is integer vector of clusters
 * y is double vector points
 * b is double vector of cluster sizes
 * c is double representing circumference
 * returns averaged silhouette score for given cluster assignment
 */

// [[Rcpp::export]]
double FastCirSil(
    NumericVector x, NumericVector y,
    NumericVector b, const double c)
{
  if (b.size() == 1) {
    return -1;
  }
  int n = x.size();

  for (int i = 0;i < n;i++) {
    y[i] = fmod(y[i], c);
    //cout << y[i] << " ";
  }
  NumericVector X(2 * n);
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


  NumericVector order(b.size());
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

  NumericVector b1(b.begin(), b.end());

  // Incorrect to use:
  // NumericVector b1(b);
  // because the reference in b will be copied to b1.

  /*
  NumericVector b1(b.size());

  for (int i = 0; i < b.size(); ++i)
  {
    b1[i] = b[i];
    //cout <<b[i] << endl;
  }
  */
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
  //for (int i = 0;i < n;++i)
  {
    //std::cout << x[i] << "," << y[i] << std::endl;
  }

  double sil = 0;

  for (int k = 0; k < b.size(); ++k) {
    NumericVector a(b[k]);
    a = intra(x, y, b[k], indexes[k], c);
    NumericVector B(b[k]);
    int kl = fmod(k + b.size() - 1, b.size());
    int kr = fmod(k + 1, b.size());
    NumericVector bl(b[k]);
    bl = inter(x, y, b, indexes, c, center, k, kl);
    NumericVector br(b[k]);
    br = inter(x, y, b, indexes, c, center, k, kr);
    for (int i = 0; i < b[k];++i) {
      B[i] = fmin(bl[i], br[i]);
      //std::cout<<B[i]<<std::endl;
      sil = sil + (B[i] - a[i]) / fmax(a[i], B[i]);

    }
    //if(k==1)return B;
  }

  return sil/n;
}




