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
    return(new("imageBW", image = (img@image * a) + b))
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
        c <- 1 / log(1 + max(img@image))
    }
    
    return(new("imageBW", image = c * log(1 + img@image)))
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
    maxPixel <- max(img@image)
    return(new("imageBW", image = maxPixel * (img@image/maxPixel)^gamma))
}