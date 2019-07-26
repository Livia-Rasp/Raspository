#' Title
#'
#' @param imgA 
#' @param B 
#'
#' @return
#' @export
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

#' Title
#'
#' @param imgA 
#' @param B 
#'
#' @return
#' @export
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

#' Title
#'
#' @param imgA 
#' @param B 
#'
#' @return
#' @export
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

#' Title
#'
#' @param imgA 
#' @param B 
#'
#' @return
#' @export
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