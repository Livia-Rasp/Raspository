#include <Rcpp.h>
using namespace Rcpp;


//' Calculating the Dissipation of Pixels
//' 
//' @export dissipatePixel
//' @import Rcpp
//' 
//' @keywords internal
//' 
//' @references \insertRef{Hagenburg2009}{Raspository}
//' 
//' @return the image as matrix after the next time step
//'
// [[Rcpp::export]]
SEXP dissipatePixel(const SEXP& imgOriginal, const double& minimalTreshold) {
    NumericMatrix img(imgOriginal);
    NumericMatrix imgAtNewStep(img.nrow(), img.ncol());
    
    for(std::size_t i = 0; i < img.nrow(); i++){
        for(std::size_t j = 0; j < img.ncol(); j++){
            
            double currentPixel = img(i,j);
            double dissipated = 0.0;
            
            if(currentPixel > 1.0 || currentPixel < minimalTreshold){
                
                double toDissipate = currentPixel; 
                
                if(currentPixel > 1){
                    toDissipate -= 1.0;
                }
                
                double tmp1 = toDissipate / 9.0;
                double tmp2 = toDissipate / 36.0;
                
                if(i > 0){
                    imgAtNewStep(i - 1, j) += tmp1;
                    dissipated += tmp1;
                }
                
                if(j > 0){
                    imgAtNewStep(i,j - 1) += tmp1;
                    dissipated += tmp1;
                }
                
                if(i < img.nrow() - 1){
                    imgAtNewStep(i + 1,j) += tmp1;
                    dissipated += tmp1;
                }
                
                if(j < img.ncol() - 1){
                    imgAtNewStep(i,j + 1) += tmp1;
                    dissipated += tmp1;
                }
                
                if( i > 0 && j > 0){
                    imgAtNewStep(i - 1,j - 1) += tmp2;
                    dissipated += tmp2;
                }
                
                if( i > 0 && j < img.ncol() - 1){
                    imgAtNewStep(i - 1,j + 1) += tmp2;
                    dissipated += tmp2;
                }
                
                if( i < img.nrow() - 1 && j > 0){
                    imgAtNewStep(i + 1,j - 1) += tmp2;
                    dissipated += tmp2;
                }
                
                if( i < img.nrow() - 1 && j > img.ncol() - 1){
                    imgAtNewStep(i + 1,j + 1) += tmp2;
                    dissipated += tmp2;
                }
                
                
            }else{
                
                double tmp1 = currentPixel / 9.0;
                double tmp2 = currentPixel / 36.0;
                
                if( i > 1 && img(i - 1,j) > currentPixel && img(i - 1,j) < 1){
                    imgAtNewStep(i - 1,j) += tmp1;
                    dissipated += tmp1;
                }
                
                if( j > 0 && img(i,j - 1) > currentPixel && img(i,j - 1) < 1){
                    imgAtNewStep(i,j - 1) += tmp1;
                    dissipated += tmp1;
                }
                
                if(i < img.nrow() - 1 && img(i + 1,j) > currentPixel && img(i + 1,j) < 1){
                    imgAtNewStep(i + 1,j) += tmp1;
                    dissipated += tmp1;
                }
                
                if(j < img.ncol() - 1 && img(i,j + 1) > currentPixel && img(i,j + 1) < 1){
                    imgAtNewStep(i,j + 1) += tmp1;
                    dissipated += tmp1;
                }
                
                if( i > 0 && j > 0 && img(i - 1,j - 1) > currentPixel && img(i - 1,j - 1) < 1){
                    imgAtNewStep(i - 1,j - 1) += tmp2;
                    dissipated += tmp2;
                }
                
                if( i > 0 && j < img.ncol() - 1 && img(i - 1,j + 1) > currentPixel && img(i - 1,j + 1) < 1){
                    imgAtNewStep(i - 1,j + 1) += 1.0 / 36.0 * currentPixel;
                    dissipated += 1.0 / 36.0 * currentPixel;
                }
                
                if( i < img.nrow() - 1 && j > 0 && img(i + 1,j - 1) > currentPixel && img(i + 1,j - 1) < 1){
                    imgAtNewStep(i + 1,j - 1) += tmp2;
                    dissipated += tmp2;
                }
                
                if( i < img.nrow() - 1 && j > img.ncol() - 1 && img(i + 1,j + 1) > currentPixel && img(i + 1,j + 1) < 1){
                    imgAtNewStep(i + 1,j + 1) += tmp2;
                    dissipated += tmp2;
                }
                
            }
            
            // add the non dissipated amount to the same pixel in next time-step
            imgAtNewStep(i,j) += currentPixel - dissipated;
        }
    }
    
return imgAtNewStep;
}
