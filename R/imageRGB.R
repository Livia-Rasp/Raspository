#' Title
#'
#' @slot original array.
#' @slot current array.
#' @slot operations list.
#'
#' @return
#' @export
#'
#' @examples
imageRGB <- setClass("imageRGB", slots=list(original="array", current="array", operations="list"))

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
    return(new("imageRGB", original = image, current = image, operations = list()))
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
    plot(as.raster(object@current))}



