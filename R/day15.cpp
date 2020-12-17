#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// vectorseq, step returns double

// [[Rcpp::export]]
int run_for_steps(IntegerVector vec, int steps) {
    int n = vec.size()-1;
    if(steps <= n) {
        return vec[steps-1];
    }
    int max_start = Rcpp::max(vec)+1;
    //make the vector
    Rcpp::NumericVector spoken(std::max(steps + 1, max_start));
    // fill with startvector
    for(int i =0; i <n; i++){
        spoken[vec[i]] = i+1;
    }
    int current = vec[n];
    int next_number;
    for( int i = n+1; i < steps; i++){
        next_number = int(spoken[current]>0) * i - spoken[current];
        spoken[current] =i;
        current = next_number;
    }
    return current;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
run_for_steps(c(2,3,1),4)
#run_for_steps(c(2,3,1),2020) ==  78
#run_for_steps(c(1,2,3),2020) ==  27
*/
