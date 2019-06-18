#include <Rcpp.h>
using namespace Rcpp;

//' Dither the imge with minimized average error
//' 
//' @export meaDithering
//' @import Rcpp
//' 
//' 
//' @references \insertRef{Jarvis1976}{Raspository}
//' 
//' @return the image as matrix after dithering
//'
// [[Rcpp::export]]
SEXP meaDithering(NumericMatrix img, Function transformPaletteFunction) {
    LogicalMatrix imgNew(img.nrow(), img.ncol());
    
    for(size_t i = 0; i < img.nrow(); i++){
        for(size_t j = 0; j < img.ncol(); j++){
            
            double oldPixel = img(i, j);
            bool newPixel = Rcpp::as<bool>(transformPaletteFunction(oldPixel));
            
            double error = (oldPixel - newPixel)/48.0;
            
            imgNew(i,j) = newPixel;
            
            
            if(i <img.nrow() - 1){
               img(i + 1, j    ) += error * 7.0;
            }
            if(i < img.nrow() - 2){
               img(i + 2, j    ) += error * 5.0;
            }
            
            if(i > 2 && j < img.ncol() - 1){
               img(i - 2, j + 1) += error * 3.0;
            }
            
            if(i > 1 && j < img.ncol() - 1){
               img(i - 1, j + 1) += error * 5.0;
            }
            
            if(j < img.ncol() - 1){
               img(i    , j + 1) += error * 7.0;
            }
            
            
            if(i <img.nrow() - 1 && j < img.ncol() - 1){
               img(i + 1, j + 1) += error * 5.0;
            }
            
            if(i <img.nrow() - 2 && j < img.ncol() - 1){
               img(i + 2, j + 1) += error * 3.0;
            }
            
            if(i > 2 && j < img.ncol() - 2){
               img(i - 2, j + 2) += error;
            }
            
            if(i > 1 && j < img.ncol() - 2){
               img(i - 1, j + 2) += error * 3.0;
            }
            
            if(j < img.ncol() - 2){
               img(i    , j + 2) += error * 5.0;
            }
            
            
            if(i < img.nrow() - 1 && j < img.ncol() - 2){
               img(i + 1, j + 2) += error * 3.0;
            }
            
            if(i < img.nrow() - 2 && j < img.ncol() - 2){
               img(i + 2, j + 2) += error;
            }
            
        }
    }
    
    return imgNew;
}
