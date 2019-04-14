#' Title
#'
#' @slot original matrix.
#' @slot current matrix.
#' @slot operations list.
#'
#' @return
#' @export
#'
#' @examples
imageBW <- setClass("imageBW", slots=list(original="matrix", current="matrix", operations="list"))

#' Title
#'
#' @param pathToJpeg
#'
#' @return
#' @export
#' @importFrom jpeg readJPEG
#'
#' @examples
imageBWFromJpeg <-function(pathToJpeg){
    image<- readJPEG(pathToJpeg)
    imageBW <- image[,,1]
    return(new("imageBW", original = imageBW, current = imageBW, operations = list()))
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
plot.imageBW <- function(object){
    plot(as.raster(object@current))
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
cropPixels<- function(object){
    object@current[object@current > 1] <- 1
    object@current[object@current < 0] <- 0
    return(object)
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
addUnifNoise <- function(object){
    slot(object, "current") <- slot(object, "current") +
        runif(length(slot(object, "current")), min = -1, max = 1)
    return(cropPixels(object))
}

#' Title
#'
#' @param object
#' @param sd
#'
#' @return
#' @export
#'
#' @examples
addNormalNoise <- function(object, sd = NULL){
    if(is.null(sd)){
        object@current <- object@current + rnorm(length(object@current), sd = sd(object@current))
    }else{
        object@current <- object@current + rnorm(length(object@current), sd = sd)
    }
    return(cropPixels(object))
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
multiplyUnifNoise <- function(object){
    object@current <- object@current * (1 + runif(length(object@current), min = -1, max = 1))
    return(cropPixels(object))
}

#' Title
#'
#' @param object
#' @param sd
#'
#' @return
#' @export
#'
#' @examples
multiplyNormalNoise <- function(object, sd = NULL){
    if(is.null(sd)){
        object@current <- object@current * ( 1 + rnorm(length(object@current),
                                                       sd = sd(object@current)))
    }else{
        object@current <- object@current * ( 1 + rnorm(length(object@current),
                                                       sd = sd))
    }
    return(cropPixels(object))
}
