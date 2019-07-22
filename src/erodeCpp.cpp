#include <Rcpp.h>
#include <algorithm> 
using namespace Rcpp;

//' @import Rcpp
//' @references \insertRef{Weickert2019}{Raspository} 
// [[Rcpp::export]]
NumericVector erodeCpp(SEXP img, SEXP mask) {
    NumericMatrix imgMatrix(img); 
    NumericMatrix filteredMatrix(imgMatrix.nrow(), imgMatrix.ncol());
    IntegerMatrix maskMatrix(mask);
    
    // iterate over image
    for(std::size_t i = 0; i < imgMatrix.nrow(); i++){
        for(std::size_t j = 0; j < imgMatrix.ncol(); j++){
            
            double minValue = 1.0;
            // iterate over mask
            for(std::size_t k = 0; k < maskMatrix.nrow(); k++){
                int x = maskMatrix(k, 0);
                int y = maskMatrix(k, 1);
                
                // checking boundaries
                if(i + x < imgMatrix.nrow() && i + x >= 0 && 
                   j + y < imgMatrix.ncol() && j + y >= 0){
                    minValue = std::min(minValue, imgMatrix(i + x, j + y));
                }
            }
            
            // set pixel to max value from the neighborhood defined through the mask
            filteredMatrix(i, j) = minValue;
            
        }
    }
    
    return(filteredMatrix);
}



