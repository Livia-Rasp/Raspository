#' Title
#'
#' @param object An image of class \code{\link[Raspository]{imageOneChannel}}.
#'
#' @return
#' @export
#'
#' @examples
min.imageOneChannel <- function(object, na.rm = FALSE){
    return(min(object@imageMatrix, na.rm = na.rm))
}

#' Title
#'
#' @param object An image of class \code{\link[Raspository]{imageOneChannel}}.
#'
#' @return
#' @export
#'
#' @examples
max.imageOneChannel <- function(object, na.rm = FALSE){
    return(max(object@imageMatrix, na.rm = na.rm))
}

#' Title
#'
#' @param object An image of class \code{\link[Raspository]{imageOneChannel}}.
#'
#' @return
#' @export
#'
#' @examples
pixelSum.imageOneChannel <- function(object){
    return(sum(object@imageMatrix))
}

#' Mean Greyvalue
#' 
#' @description Calculates the arithmetic \code{\link{mean}} of all grey values
#' contained in the image.
#' 
#' @export mean
#'
#' @param object An image of class \code{\link[Raspository]{imageOneChannel}}.
#'
#' @return The mean greyvalue of the image
#' 
#' @seealso \code{\link{mean}}
#' 
#' @title Mean Greyvalue
#'
#' @examples
mean.imageOneChannel <- function(object){
    return(mean(object@imageMatrix))
}

#' Title
#' 
#' @importFrom stats sd
#'
#' @param object An image of class \code{\link[Raspository]{imageOneChannel}}.
#'
#' @return
#' @export
#'
#' @examples
sd.imageOneChannel <- function(object){
    return(sd(object@imageMatrix))
}

#' Title
#' 
#' @importFrom e1071 skewness
#'
#' @param object An image of class \code{\link[Raspository]{imageOneChannel}}.
#'
#' @return
#' @export
#'
#' @examples
skewness.imageOneChannel <- function(object){
    return(skewness(object@imageMatrix))
}

#' Title
#' 
#' @importFrom e1071 kurtosis
#'
#' @param object An image of class \code{\link[Raspository]{imageOneChannel}}.
#'
#' @return
#' @export
#'
#' @examples
kurtosis.imageOneChannel <- function(object){
    return(kurtosis(object@imageMatrix))
}

#' Calculate Coocurence
#'
#' @param imageOneChannel An image of class 
#' \code{\link[Raspository]{imageOneChannel}}.
#' @param direction 
#' @param greyvalues 
#'
#' @return
#' @export
#' 
#' @import data.table
#' @importFrom plyr count
#' 
#' @references \insertRef{Haralick1973}{Raspository}
#'
#' @examples
calculateCooccurrence <- function(imageOneChannel, direction = c(1, 1), greyvalues = 256){
    
    roundedImg <- round(imageOneChannel@imageMatrix * (greyvalues - 1) + 1)
    
    if(direction[1] >= 0 && direction[2] >= 0){
        originalValue <- roundedImg[1: (nrow(roundedImg) - direction[1]), 
                                    1: (ncol(roundedImg) - direction[2])]
        neighborValue <- roundedImg[(1 + direction[1]): nrow(roundedImg) , 
                                    (1 + direction[2]): ncol(roundedImg)]
    }else if(direction[1] < 0 && direction[2] >= 0){
        originalValue <- roundedImg[(1 + direction[1]): nrow(roundedImg), 
                                    1: (ncol(roundedImg) - direction[2])]
        neighborValue <- roundedImg[1: (nrow(roundedImg) - direction[1]) , 
                                    (1 + direction[2]): ncol(roundedImg)]
    }else if(direction[1] >= 0 && direction[2] < 0){
        originalValue <- roundedImg[1: (nrow(roundedImg) - direction[1]), 
                                    (1 + direction[2]): ncol(roundedImg)]
        neighborValue <- roundedImg[(1 + direction[1]): nrow(roundedImg) , 
                                    1: (ncol(roundedImg) - direction[2])]
    }else{
        originalValue <- roundedImg[(1 + direction[1]): nrow(roundedImg), 
                                    (1 + direction[2]): ncol(roundedImg)]
        neighborValue <- roundedImg[1: (nrow(roundedImg) - direction[1]), 
                                    1: (ncol(roundedImg) - direction[2])]
    }
    
    coocurrence <- as.data.table(count(data.frame(i = as.integer(originalValue), 
                                                  j = as.integer(neighborValue))))
    coocurrence[, prob := freq/sum(freq)]
    
    return(coocurrence)
}