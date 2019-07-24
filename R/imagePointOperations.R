#' Title
#'
#' @param img 
#' @param a 
#' @param b
#' 
#' @references \insertRef{Weickert2019}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
affineGreyscaleTransformation <- function(img, a, b){
    return(new("imageOneChannel", image = (img@imageMatrix * a) + b))
}

#' Title
#'
#' @param img 
#' @param c 
#' 
#' @references \insertRef{Weickert2019}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
logarithmicDynamicCompression <- function(img, c = NULL){
    if(is.null(c)){
        c <- 1 / log(1 + max(img@imageMatrix))
    }
    
    return(new("imageOneChannel", image = c * log(1 + img@imageMatrix)))
}

#' Title
#'
#' @param img 
#' @param gamma 
#' 
#' @references \insertRef{Weickert2019}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
gammaCorrection <- function(img, gamma){
    maxPixel <- max(img@imageMatrix)
    return(new("imageOneChannel", image = maxPixel * (img@imageMatrix/maxPixel)^gamma))
}