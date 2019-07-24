#' Calculate Coocurence
#'
#' @param imageOneChannel
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