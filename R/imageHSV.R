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
imageHSV <- setClass("imageHSV", slots=list(original="array", current="array", operations="list"))

#' Title
#'
#' @param max
#' @param maxIndex
#' @param min
#' @param r
#' @param g
#' @param b
#'
#' @return
#' @export
#'
#' @examples
calculateHUE <- function(max, maxIndex, min, r, g, b){
    h <- 0.0
    if(max == min){
        return(h)
    }else if(maxIndex == 1){
        h <- 60.0 * ((g - b)/(max - min))
    }else if(maxIndex == 2){
        h <- 60.0 * (2.0 + (b - r)/(max - min))
    }else if(maxIndex == 3){
        h <- 60.0 * (4.0 + (r - g)/(max - min))
    }

    # if the value is negativ add 360° to it
    if(h >= 0){
        return(h)
    }else{
        return(h + 360)
    }
}

#' Title
#'
#' @param rgbArray
#'
#' @return
#' @export
#' @importFrom abind abind
#'
#' @examples
rgbArrayToHsv <- function(rgbArray){
    # get the maximal color and its index in each pixel
    max <- apply(rgbArray, c(1,2), max)
    maxIndex <-apply(rgbArray, c(1,2), which.max)
    # get the minimal color in each pixel
    min <- apply(rgbArray, c(1,2), min)

    # calculate the hue for each pixel
    h <- mapply(FUN = calculateHUE, max = max, maxIndex = maxIndex, min = min,
                r = rgbArray[,,1], g = rgbArray[,,2], b = rgbArray[,,3])
    # convert vector back to matrix
    h <- matrix(h, ncol = ncol(max))

    # calculate saturation
    s <- (max - min)/max
    # set values to zero, where max is 0 (division by zero -> NA)
    s[is.na(s)] <- 0
    # max is equal to v (value/brightness)
    v <- max

    # bind matrices together to array and return
    hsvArray <- abind(h, s, v, along = 3)
    return(hsvArray)
}

#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @examples
imageHSVFromRGB <- function(img){
    return(new("imageHSV", original = rgbArrayToHsv(img@original),
               current = rgbArrayToHsv(img@current),
               operations = img@operations))
}
