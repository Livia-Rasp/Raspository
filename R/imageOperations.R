#' Image Addition
#' 
#' @description Addition of one image with another image or a scalar. At least 
#' one of the two parameters has to be an image. If both are images, the operation
#' is executed entrywise.
#' 
#' @export
#'
#' @param A An image of class \code{\link[Raspository]{imageOneChannel}} or 
#' a scalar.
#' @param B An image of class \code{\link[Raspository]{imageOneChannel}} or 
#' a scalar.
#'
#' @return The resulting image of class \code{\link[Raspository]{imageOneChannel}}.
#' 
#'
#' @examples
`+.imageOneChannel` <- function(A, B){
    if(is.imageOneChannel(B)){
        if(is.imageOneChannel(A)){
            return(new("imageOneChannel", imageMatrix = A@imageMatrix + B@imageMatrix))
        }else{
            return(new("imageOneChannel", imageMatrix = A + B@imageMatrix))
        }
        
    }else{
        return(new("imageOneChannel", imageMatrix = A@imageMatrix + B))
    }
    
}

#' Image Subtraction
#' 
#' @description Subtraction of one image with another image or a scalar. At least 
#' one of the two parameters has to be an image. If both are images, the operation
#' is executed entrywise.
#' 
#' @export
#'
#' @param A An image of class \code{\link[Raspository]{imageOneChannel}} or 
#' a scalar.
#' @param B An image of class \code{\link[Raspository]{imageOneChannel}} or 
#' a scalar.
#'
#' @return The resulting image of class \code{\link[Raspository]{imageOneChannel}}.
#'
#' @examples
`-.imageOneChannel` <- function(A, B){
    if(is.imageOneChannel(B)){
        if(is.imageOneChannel(A)){
            return(new("imageOneChannel", imageMatrix = A@imageMatrix - B@imageMatrix))
        }else{
            return(new("imageOneChannel", imageMatrix = A - B@imageMatrix))
        }
    }else{
        return(new("imageOneChannel", imageMatrix  = A@imageMatrix - B))
    }
    
}

#' Image Multiplication
#' 
#' @description Multiplication of one image with another image or a scalar. At least 
#' one of the two parameters has to be an image. If both are images, the operation
#' is executed entrywise.
#' 
#' @export
#'
#' @param A An image of class \code{\link[Raspository]{imageOneChannel}} or 
#' a scalar.
#' @param B An image of class \code{\link[Raspository]{imageOneChannel}} or 
#' a scalar.
#'
#' @return The resulting image of class \code{\link[Raspository]{imageOneChannel}}.
#'
#' @examples
`*.imageOneChannel` <- function(A, B){
    if(is.imageOneChannel(B)){
        if(is.imageOneChannel(A)){
            return(new("imageOneChannel", imageMatrix = A@imageMatrix * B@imageMatrix))
        }else{
            return(new("imageOneChannel", imageMatrix = A * B@imageMatrix))
        }
    }else{
        return(new("imageOneChannel", imageMatrix = A@imageMatrix * B))
    }
    
}

#' Image Division
#' 
#' @description Division of one image with another image or a scalar. At least 
#' one of the two parameters has to be an image. If both are images, the operation
#' is executed entrywise.
#' 
#' @export
#'
#' @param A An image of class \code{\link[Raspository]{imageOneChannel}} or 
#' a scalar.
#' @param B An image of class \code{\link[Raspository]{imageOneChannel}} or 
#' a scalar.
#'
#' @return The resulting image of class \code{\link[Raspository]{imageOneChannel}}.
#'
#' @examples
`/.imageOneChannel` <- function(A, B){
    if(is.imageOneChannel(B)){
        if(is.imageOneChannel(A)){
            return(new("imageOneChannel", imageMatrix = A@imageMatrix / B@imageMatrix))
        }else{
            return(new("imageOneChannel", imageMatrix = A / B@imageMatrix))
        }
    }else{
        return(new("imageOneChannel", imageMatrix = A@imageMatrix / B))
    }
    
}

