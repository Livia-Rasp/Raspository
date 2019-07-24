#' Title
#'
#' @slot imageArray array.
#'
#' @return
#' @export
#'
#' @examples
imageRGB <- setClass("imageRGB", slots=list(imageArray = "array"))

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
    return(new("imageRGB", imageArray = image))
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
    plot(as.raster(object@imageArray))
}



