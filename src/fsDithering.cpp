#include <Rcpp.h>
using namespace Rcpp;

//' Dither the imge with Floyd Steinberg
//' 
//' @export fsDithering
//' @import Rcpp
//' 
//' 
//' @references \insertRef{Floyd}{Raspository}
//' 
//' @return the image as matrix after dithering
//'
// [[Rcpp::export]]
SEXP fsDithering(NumericMatrix img, Function transformPaletteFunction) {
    LogicalMatrix imgNew(img.nrow(), img.ncol());
    
    for(size_t i = 0; i < img.nrow(); i++){
        for(size_t j = 0; j < img.ncol(); j++){
            
            double oldPixel = img(i, j);
            bool newPixel = Rcpp::as<bool>(transformPaletteFunction(oldPixel));
            
            double error = (oldPixel - newPixel)/16.0;
            
            imgNew(i,j) = newPixel;
            
            
                if(i < img.nrow() - 1){
                    img(i + 1, j) +=  error * 7.0;
                }
                
                if(i > 0 && j < img.ncol() - 1){
                    img(i - 1, j + 1) += error * 3.0;
                }
                
                if(j < img.ncol() - 1){
                    img(i, j + 1)  += error * 5.0;
                }
                
                if(i < img.nrow() - 1 && j < img.ncol() - 1){
                    img(i + 1, j + 1) += error;
                }
                
                }
        }
    
    return imgNew;
}
