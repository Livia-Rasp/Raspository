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
#' @references insertRef{Weickert2019}{Raspository}
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
#' @references insertRef{Weickert2019}{Raspository}
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
#' @references insertRef{Weickert2019}{Raspository}
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
#' @references insertRef{Weickert2019}{Raspository}
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

#' Title
#'
#' @param object
#' @param percentage
#'
#' @return
#' @export
#'
#' @references insertRef{Weickert2019}{Raspository}
#'
#' @examples
saltAndPepperNoise <- function(object, percentage = .2){
    # select the indices to set to 0 or 1 at random
    indices <- sample(length(object@current), length(object@current) * percentage)
    # draw zeros and ones from a binomial distribution
    values <- rbinom(length(indices), 1, 0.5)

    object@current[indices] <- values
    return(object)
}

#' Title
#'
#' @param object
#' @param other
#'
#' @return
#' @export
#'
#' @references insertRef{Weickert2019}{Raspository}
#'
#' @examples
MSE <- function(object, other = NULL){
    if(is.null(other)){
        errorMatrix <- object@original - object@current
    }else{
        errorMatrix <- other@current - object@current
    }
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
#' @references insertRef{Weickert2019}{Raspository}
#'
#' @examples
PSNR <- function(object, other = NULL){
    mse <- MSE(object, other)
    return(-10 * log(mse, base = 10))
}
