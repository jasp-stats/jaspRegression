#include <Rcpp.h>
using namespace Rcpp;

int sign(double x){
  if(x > 0){
    return 1;
  } else if(x < 0){
    return -1;
  } else {
    return 0;
  }
}

// Defined in Hollander, Wolfe & Chicken, Nonparametric Statistical Methods, 3ed., eq. (8.5), p. 394
int Q(double a, double b, double c, double d) {
  return sign((d-b)*(c-a));
}

// Defined in Hollander, Wolfe & Chicken, Nonparametric Statistical Methods, 3ed., eq. (8.37), p. 415
// [[Rcpp::export]]
IntegerVector concordanceVector_cpp(NumericVector x, NumericVector y){
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

/*** R
library(microbenchmark)
concordanceFunction <- function(i, j) {
  concordanceIndicator <- 0
  ij <- (j[2] - i[2]) * (j[1] - i[1])
  if (ij > 0) concordanceIndicator <- 1
  if (ij < 0) concordanceIndicator <- -1
  return(concordanceIndicator)
}

addConcordances <- function(x, y, i) {
  concordanceIndex <- 0
  for (k in 1:length(x)) {
    if (k != i) {
      concordanceIndex <- concordanceIndex + concordanceFunction(c(x[i], y[i]), c(x[k], y[k]))
    }
  }
  return(concordanceIndex)
}

concordanceVector <- function(x, y){
  n <- length(x)
  concordanceSumsVector <- numeric(n)
  for (i in 1:n) {
    concordanceSumsVector[i] <- addConcordances(x, y, i)
  }
  return(concordanceSumsVector)
}

x <- rnorm(200)
y <- rnorm(200)

bench <- microbenchmark::microbenchmark(
  R    = concordanceVector(x, y),
  Rcpp = concordanceVector_cpp(x, y),
  check = "equal"
)

print(bench)
plot(bench)
*/
