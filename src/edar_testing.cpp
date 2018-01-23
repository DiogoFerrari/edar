#include <Rcpp.h>
#include <iostream>
#include "edar_testing.h"

using namespace Rcpp;

//' Multiply an number by 4
//'
//' @param x a single integer
//'
//'@export
// [[Rcpp::export]]
NumericVector eda_rcpp (NumericVector x)
{
  Rcout << "The result of the other function is" << func_cpp(x) << std::endl;
  return (4*x);
}
