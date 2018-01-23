#include <Rcpp.h>

using namespace Rcpp;

Rcpp::NumericVector func_cpp(Rcpp::NumericVector x)
{
  return(5*x);
}
