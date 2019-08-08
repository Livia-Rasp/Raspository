#' Title
#'
#' @param object
#'
#' @return
#' @export
#' @importFrom methods slot
#' @importFrom methods slot<-
#'
#' @examples
cropPixels<- function(object){
    object@imageMatrix[object@imageMatrix > 1] <- 1
    object@imageMatrix[object@imageMatrix < 0] <- 0
    return(object)
}

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
    return(new("imageOneChannel", imageMatrix = (img@imageMatrix * a) + b))
}

#' Title
#'
#' @param img 
#'
#' @return
#' @export
#'
#' @examples
affineRescale <- function(img){
    a <- max(img) - min(img)
    b <- min(img) / a
    
    return(affineGreyscaleTransformation(img = img, 1.0/a, -b))
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
        c <- 1 / log1p(max(img@imageMatrix))
    }
    
    return(new("imageOneChannel", imageMatrix = c * log1p(img@imageMatrix)))
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
    return(new("imageOneChannel", imageMatrix = maxPixel * (img@imageMatrix/maxPixel)^gamma))
}