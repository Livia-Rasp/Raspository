#' Title
#'
#' @import data.table
#'
#' @param radius 
#'
#' @return
#' @export
#'
#' @examples
diskMask <- function(radius = 5, method = "naive"){
    DT_diskCoordinates <- data.table(x=integer(), y=integer())
    
    if(method == "naive"){
        radiusSq = radius ^ 2
        for(i in -radius:radius){
            for(j in -radius:radius){
                if(i ^ 2 + j ^ 2 <= radiusSq){
                    DT_diskCoordinates <- rbind(DT_diskCoordinates, 
                                                data.table(x=i, y=j))
                }
            }
        }
    }else{
        stop(paste(method, "method not implemented."))
    }
    
    return(DT_diskCoordinates)
}

#' Title
#'
#' @param img 
#' @param mask 
#' 
#' @references \insertRef{Weickert2019}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
morphDilate <- function(img, mask = diskMask()){
    imgMatrix <- img@imageMatrix
    
    return(new("imageOneChannel", imageMatrix = dilateCpp(imgMatrix, as.matrix(mask))))
}

#' Title
#'
#' @param img 
#' @param mask 
#' 
#' @references \insertRef{Weickert2019}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
morphErode <- function(img, mask = diskMask()){
    imgMatrix <- img@imageMatrix
    
    return(new("imageOneChannel", imageMatrix = erodeCpp(imgMatrix, as.matrix(mask))))
}

#' Title
#'
#' @param img 
#' @param mask 
#'
#' @return
#' @export
#'
#' @examples
morphOpen <- function(img, mask = diskMask()){
    return(morphDilate(morphErode(img, mask = mask), mask = mask))
}

#' Title
#'
#' @param img 
#' @param mask 
#'
#' @return
#' @export
#'
#' @examples
morphClose <- function(img, mask = diskMask()){
    return(morphErode(morphDilate(img, mask = mask), mask = mask))
}

#' Title
#'
#' @param img 
#' @param mask 
#'
#' @return
#' @export
#'
#' @examples
morphWTH <- function(img, mask = diskMask()){
    return(img - morphOpen(img, mask = mask))
}

#' Title
#'
#' @param img 
#' @param mask 
#'
#' @return
#' @export
#'
#' @examples
morphBTH <- function(img, mask = diskMask()){
    return(morphClose(img, mask = mask) - img)
}

#' Title
#'
#' @param img 
#' @param mask 
#'
#' @return
#' @export
#'
#' @examples
morphSTH <- function(img, mask = diskMask()){
    return(morphClose(img, mask = mask) - morphOpen(img, mask = mask))
}