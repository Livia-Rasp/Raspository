#' Title
#' 
#' @import data.table
#'
#' @param radius 
#' @param method 
#' 
#' @references \insertRef{Taubig2014}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
circleMask <- function(radius, method = c("bresenham", "sqrt", "anglefunction")){
    DT_Coordinates <- data.table(x=integer(), y=integer())
    
    
    if(method[1] == "anglefunction"){
        n <- ceiling((2 * pi * radius))
        for(i in 0:n){
            DT_Coordinates <- rbind(DT_Coordinates, 
                                    data.table(x = round(radius * cos(2 * pi * i/n)), 
                                               y = round(radius * sin(2 * pi * i/n))))
        }
    }else if(method[1] == "sqrt"){
        rSquared <- radius ^ 2
        for(x in -radius:radius){
            y  <- sqrt(rSquared - x ^ 2)
            
            ## discretize values
            x <- round(x)
            y <- round(y)
            DT_Coordinates <- rbind(DT_Coordinates, data.table(x = x, y = y))
            DT_Coordinates <- rbind(DT_Coordinates, data.table(x = x, y = -y))
        }
    }else if(method[1] == "bresenham"){
        x <- 0
        y <- radius
        DT_Coordinates <- rbind(DT_Coordinates, 
                                data.table(x = c(0, 0, radius, - radius), 
                                           y = c(radius, - radius, 0, 0)))
        f <- 5/4 -radius
        
        while(x < y){
            if(f < 0){
                f <- f + 2 * x + 1
            }else{
                f <- f + 2 * x  - 2 * y + 2
                y <- y - 1
            }
            x <- x + 1
            DT_Coordinates <- rbind(DT_Coordinates, 
                                    data.table(x = c(x, x, -x, -x, y, y, -y, -y), 
                                               y = c(y, -y, y, -y, x, -x, x, -x)))
            
        }
    }
    
    setkey(DT_Coordinates, x, y)
    
    return(DT_Coordinates)
}

#' Title
#' 
#' @import data.table
#'
#' @param mask 
#' @param x 
#' @param y 
#'
#'
#' @return
#' @export
#'
#' @examples
fillConvexShape <- function(mask){
    return(mask[,.(x=min(x):max(x)) , by="y"])
}

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
diskMask <- function(radius = 5, method = c("bresenham", "naive")){
    DT_diskCoordinates <- data.table(x=integer(), y=integer())
    
    if(method[1] == "naive"){
        radiusSq = radius ^ 2
        for(i in -radius:radius){
            for(j in -radius:radius){
                if(i ^ 2 + j ^ 2 <= radiusSq){
                    DT_diskCoordinates <- rbind(DT_diskCoordinates, 
                                                data.table(x=i, y=j))
                }
            }
        }
    }else if(method[1] == "bresenham"){
        DT_diskCoordinates <- fillConvexShape(circleMask(radius = radius, 
                                                         method = "bresenham"))
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