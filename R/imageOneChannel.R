#' Title
#'
#' @slot image matrix.
#'
#' @return
#' @export
#'
#' @examples
imageOneChannel <- setClass("imageOneChannel", slots=list(imageMatrix="matrix"))

#' Title
#' 
#' @description Checks if an object is of type \code{\link[Raspository]{imageOneChannel}}.
#' 
#' @export
#'
#' @param x any R object.
#'
#' @return \code{TRUE}, if the object is of type \code{\link[Raspository]{imageOneChannel}}.
#'  \code{FALSE} otherwise.
#' 
#'
#' @examples
is.imageOneChannel <-function(x){
    return(inherits(x, "imageOneChannel"))
}

#' Title
#'
#' @param pathToJpeg
#'
#' @return
#' @export
#' @importFrom jpeg readJPEG
#' @importFrom methods new
#'
#' @examples
imageOneChannelFromJpeg <-function(pathToJpeg){
    image<- readJPEG(pathToJpeg)
    imageOneChannel <- image[,,1]
    return(new("imageOneChannel", imageMatrix = imageOneChannel))
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#' @importFrom grDevices as.raster
#' @importFrom graphics plot
#' @importFrom methods slot
#'
#' @examples
plot.imageOneChannel <- function(object){
    plot(as.raster(object@imageMatrix))
}

#' Title
#' 
#' @importFrom jpeg writeJPEG
#'
#' @param target 
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
writeJPEG.imageOneChannel <- function(object, target){
    writeJPEG(image = object@imageMatrix, target = target)
}

#' Title
#' 
#' @importFrom png writePNG
#'
#' @param target 
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
writePNG.imageOneChannel <- function(object, target){
    writePNG(image = object@imageMatrix, target = target)
}
