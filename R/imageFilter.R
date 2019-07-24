#' Title
#'
#' @param img 
#' @param sdSpace 
#' @param sdTone 
#' 
#' @references \insertRef{Weickert2019}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
bilateralFilter <- function(img, sdSpace, sdTone, patchRange = 5){
    ## This function is a wrapper around the corresponding Cpp function
    imgMatrix <- img@imageMatrix
    
    return(new("imageOneChannel", image = bilateralFilterCpp(imgMatrix,sdSpace, sdTone,
                                                     patchRange)))
}
