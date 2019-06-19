#' Title
#'
#' @slot image array.
#'
#' @return
#' @export
#'
#' @examples
imageRGB <- setClass("imageRGB", slots=list(image = "array"))

#' Title
#'
#' @param pathToJpeg
#'
#' @return
#' @export
#' @importFrom jpeg readJPEG
#'
#' @examples
imageRGBFromJpeg <-function(pathToJpeg){
    image<- readJPEG(pathToJpeg)
    return(new("imageRGB", image = image))
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
plot.imageRGB <- function(object){
    plot(as.raster(object@image), axes=FALSE, box=FALSE)
}



