#include <Rcpp.h>
#include <algorithm> 
using namespace Rcpp;

//' @import Rcpp
//' @references \insertRef{Weickert2019}{Raspository} 
// [[Rcpp::export]]
SEXP bilateralFilterCpp(SEXP img, double sdSpace, double sdTone, int patchRange) {
    NumericMatrix imgMatrix(img); 
    NumericMatrix filteredMatrix(imgMatrix.nrow(), imgMatrix.ncol());
    
    // iterate over image
    for(int i = 0; i < imgMatrix.nrow(); i++){
        for(int j = 0; j < imgMatrix.ncol(); j++){
            double weightSum = 0.0;
            // iterate over patch
            for(int k = std::max(0, i - patchRange); 
                k < std::min(imgMatrix.nrow(), i + patchRange); k++){
                
                for(int l = std::max(0, j - patchRange); 
                    l < std::min(imgMatrix.ncol(), j + patchRange); l++){
                    // spatial difference is calculated by Pythagorean theorem
                    NumericVector spatialDistance(1, sqrt( pow((i - k), 2) + pow((j - l),2) ));
                    NumericVector spatialWeight = dnorm(spatialDistance, 0.0, 
                                                        sdSpace);
                    
                    NumericVector tonalDistance(1, fabs(imgMatrix(i,j) - imgMatrix(k,l)));
                    NumericVector tonalWeight = dnorm(tonalDistance, 0.0, 
                                                       sdTone);
                    double weight = spatialWeight(0) * tonalWeight(0);
                    
                    
                    weightSum += weight;
                    
                    // add weighted tone to the new image
                    filteredMatrix(i,j) += weight * imgMatrix(k,l);
                                                     
                }
            }
            
            // normalisation 
            
            filteredMatrix(i,j) /= weightSum;
            
        }
    }
    
    
    return(filteredMatrix);
}

