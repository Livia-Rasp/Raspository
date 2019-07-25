#' Title
#'
#' @param imgA 
#' @param B 
#'
#' @return
#' @export
#'
#' @examples
`+.imageOneChannel` <- function(imgA, B){
    if(is.imageOneChannel){
        return(new("imageOneChannel", image = imgA@imageMatrix + B@imageMatrix))
    }else{
        return(new("imageOneChannel", image = imgA@imageMatrix + B))
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
`-.imageOneChannel` <- function(imgA, B){
    if(is.imageOneChannel){
        return(new("imageOneChannel", image = imgA@imageMatrix - B@imageMatrix))
    }else{
        return(new("imageOneChannel", image = imgA@imageMatrix - B))
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
`*.imageOneChannel` <- function(imgA, B){
    if(is.imageOneChannel){
        return(new("imageOneChannel", image = imgA@imageMatrix * B@imageMatrix))
    }else{
        return(new("imageOneChannel", image = imgA@imageMatrix * B))
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
`/.imageOneChannel` <- function(imgA, B){
    if(is.imageOneChannel){
        return(new("imageOneChannel", image = imgA@imageMatrix / B@imageMatrix))
    }else{
        return(new("imageOneChannel", image = imgA@imageMatrix / B))
    }
    
}