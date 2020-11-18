#include <Rcpp.h>
using namespace Rcpp;

int sign(const double& x){
  return x == 0 ? 0 : x > 0 ? 1 : -1;
}

// Defined in Hollander, Wolfe & Chicken, Nonparametric Statistical Methods, 3ed., eq. (8.5), p. 394
int Q(const double& a, const double& b, const double& c, const double& d) {
  return sign((d-b)*(c-a));
}

// Defined in Hollander, Wolfe & Chicken, Nonparametric Statistical Methods, 3ed., eq. (8.37), p. 415
// [[Rcpp::export]]
IntegerVector concordanceVector_cpp(const NumericVector& x, const NumericVector& y){
  int n = x.size();
  IntegerVector out(n);

  for(int i = 0; i < n; i++){
    out[i] = 0;
    for(int t = 0; t < n; t++){
      if(i != t) out[i] += Q(x[i], y[i], x[t], y[t]);
    }
  }

  return out;
}
