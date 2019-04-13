imageBW <- setClass("imageBW", slots=list(original="matrix", current="matrix", operations="list"))

imageBWFromJpeg <-function(pathToJpeg){
    require(jpeg)
    image<- readJPEG(pathToJpeg)
    imageBW <- image[,,1]
    return(new("imageBW", original = imageBW, current = imageBW, operations = list()))
}

plot.imageBW <- function(object){
    plot(as.raster(object@current))
}

cropPixels<- function(object){
    object@current[object@current > 1] <- 1
    object@current[object@current < 0] <- 0
    return(object)
}

addUnifNoise <- function(object){
    slot(object, "current") <- slot(object, "current") +
        runif(length(slot(object, "current")), min = -1, max = 1)
    return(cropPixels(object))
}

addNormalNoise <- function(object, sd = NULL){
    if(is.null(sd)){
        object@current <- object@current + rnorm(length(object@current), sd = sd(object@current))
    }else{
        object@current <- object@current + rnorm(length(object@current), sd = sd)
    }
    return(cropPixels(object))
}

multiplyUnifNoise <- function(object){
    object@current <- object@current * (1 + runif(length(object@current), min = -1, max = 1))
    return(cropPixels(object))
}

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
