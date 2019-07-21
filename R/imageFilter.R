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
bilateralFilter <- function(img, sdSpace, sdTone){
    imgMatrix <- img@image
    filteredMatrix <- matrix(0, nrow = nrow(imgMatrix), ncol = ncol(imgMatrix))
    
    ## iterate over image
    for(i in seq(nrow(imgMatrix))){
        for(j in seq(ncol(imgMatrix))){
            weightSum <- 0
            
            ## iterate over each combination of pixel
            for(k in seq(nrow(imgMatrix))){
                for(l in seq(ncol(imgMatrix))){
                    ## spatial difference is calculated by Pythagorean theorem
                    spatialWeight <- dnorm(x = sqrt((i - k)^2 + (j - l)^2), 
                                           sd = sdSpace)
                    
                    tonalWeight <- dnorm(x = abs(imgMatrix[i,j] - 
                                                     imgMatrix[k,l]), 
                                         sd = sdTone)
                    weight <- spatialWeight * tonalWeight
                    weightSum <- weightSum + weight
                    
                    
                    ## add weighted tone to the new image
                    filteredMatrix[i,j] <- filteredMatrix[i,j] + 
                        weight * imgMatrix[k,l]
                }
            }
            
            ## normalisation 
            filteredMatrix[i,j] <- (filteredMatrix[i,j] / weightSum)
            
        }
    }
    
    
    return(new("imageBW", image = filteredMatrix))
}