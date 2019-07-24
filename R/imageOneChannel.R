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
#' @param object
#'
#' @return
#' @export
#' @importFrom stats runif
#'
#' @references \insertRef{Weickert2019}{Raspository}
#'
#' @examples
addUnifNoise <- function(object){
    slot(object, "image") <- slot(object, "image") +
        runif(length(slot(object, "image")), min = -1, max = 1)
    return(cropPixels(object))
}

#' Title
#'
#' @param object
#' @param sd
#'
#' @return
#' @export
#' @importFrom methods slot
#' @importFrom methods slot<-
#'
#' @references \insertRef{Weickert2019}{Raspository}
#'
#' @examples
addNormalNoise <- function(object, sd = NULL){
    if(is.null(sd)){
        object@imageMatrix <- object@imageMatrix + rnorm(length(object@imageMatrix), sd = sd(object@imageMatrix))
    }else{
        object@imageMatrix <- object@imageMatrix + rnorm(length(object@imageMatrix), sd = sd)
    }
    return(cropPixels(object))
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#' @importFrom stats runif
#' @importFrom methods slot
#' @importFrom methods slot<-
#'
#' @references \insertRef{Weickert2019}{Raspository}
#'
#' @examples
multiplyUnifNoise <- function(object){
    object@imageMatrix <- object@imageMatrix * (1 + runif(length(object@imageMatrix), min = -1, max = 1))
    return(cropPixels(object))
}

#' Title
#'
#' @param object
#' @param sd
#'
#' @return
#' @export
#' @importFrom stats rnorm
#' @importFrom methods slot
#' @importFrom methods slot<-
#'
#' @references \insertRef{Weickert2019}{Raspository}
#'
#' @examples
multiplyNormalNoise <- function(object, sd = NULL){
    if(is.null(sd)){
        object@imageMatrix <- object@imageMatrix * ( 1 + rnorm(length(object@imageMatrix),
                                                       sd = sd(object@imageMatrix)))
    }else{
        object@imageMatrix <- object@imageMatrix * ( 1 + rnorm(length(object@imageMatrix),
                                                       sd = sd))
    }
    return(cropPixels(object))
}

#' Title
#'
#' @param object
#' @param percentage
#'
#' @return
#' @export
#' @importFrom stats rbinom
#' @importFrom methods slot
#' @importFrom methods slot<-
#'
#' @references \insertRef{Weickert2019}{Raspository}
#'
#' @examples
saltAndPepperNoise <- function(object, percentage = .2){
    # select the indices to set to 0 or 1 at random
    indices <- sample(length(object@imageMatrix), length(object@imageMatrix) * percentage)
    # draw zeros and ones from a binomial distribution
    values <- rbinom(length(indices), 1, 0.5)

    object@imageMatrix[indices] <- values
    return(object)
}

#' Title
#'
#' @param object
#' @param other
#'
#' @return
#' @export
#' @importFrom methods slot
#' @importFrom methods slot<-
#'
#' @references \insertRef{Weickert2019}{Raspository}
#'
#' @examples
MSE <- function(object, other){
    
    errorMatrix <- other@imageMatrix - object@imageMatrix
    
    squaredErrorMatrix <- errorMatrix ^ 2
    return(mean(squaredErrorMatrix))
}

#' Title
#'
#' @description
#' @details
#' \deqn{PSNR(x, y) = 10 \cdot log_{10}  ( \frac{MAX^2}{MSE(x, y)} )}
#' \deqn{PSNR(x, y) = 10 \cdot log_{10}(MAX^2) - 10 \cdot log_{10}(MSE(x, y))}
#' \deqn{PSNR(x, y) = 20 \cdot log_{10}(MAX) - 10 \cdot log_{10}(MSE(x, y))}
#'
#'
#' \deqn{PSNR(x, y) =  - 10 \cdot log_{10}(MSE(x, y))}
#'
#' @param object
#' @param other
#'
#' @return
#' @export
#'
#' @references \insertRef{Weickert2019}{Raspository}
#'
#' @examples
PSNR <- function(object, other = NULL){
    mse <- MSE(object, other)
    return(-10 * log(mse, base = 10))
}
