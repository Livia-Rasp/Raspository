#' Title
#'
#' @slot image matrix.
#'
#' @return
#' @export
#'
#' @examples
imageOneChannel <- setClass("imageOneChannel", slots=list(imageMatrix="matrix"))

#' Is It A One Channel Image
#' 
#' @description Checks if an object is of type \code{\link[Raspository]{imageOneChannel}}.
#' 
#' @export
#'
#' @param x any R object.
#'
#' @return \code{TRUE}, if the object is of type \code{\link[Raspository]{imageOneChannel}}.
#'  \code{FALSE} otherwise.
#' 
#'
#' @examples
is.imageOneChannel <-function(x){
    return(inherits(x, "imageOneChannel"))
}

#' Title
#'
#' @param pathToJpeg
#'
#' @return
#' @export
#' @importFrom jpeg readJPEG
#' @importFrom methods new
#'
#' @examples
imageOneChannelFromJpeg <-function(pathToJpeg){
    image<- readJPEG(pathToJpeg)
    imageOneChannel <- image[,,1]
    return(new("imageOneChannel", imageMatrix = imageOneChannel))
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#' @importFrom grDevices as.raster
#' @importFrom graphics plot
#' @importFrom methods slot
#'
#' @examples
plot.imageOneChannel <- function(object){
    plot(as.raster(object@imageMatrix))
}

#' Title
#' 
#' @importFrom jpeg writeJPEG
#'
#' @param target 
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
writeJPEG.imageOneChannel <- function(object, target){
    writeJPEG(image = object@imageMatrix, target = target)
}

#' Title
#' 
#' @importFrom png writePNG
#'
#' @param target 
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
writePNG.imageOneChannel <- function(object, target){
    writePNG(image = object@imageMatrix, target = target)
}

#' Title
#' 
#' @import data.table
#'
#' @param img 
#' @param log.scale 
#'
#' @return
#' @export
#'
#' @examples
calculateFourierSpectrum <- function(img, log.scale = TRUE, shift = TRUE){
    
    if(shift){
        
        DT <- CJ(1:nrow(img@imageMatrix), 1:ncol(img@imageMatrix))
    
        DT[, sign := (-1)^(V1 + V2)]
        shiftMatrix <- new("imageOneChannel", imageMatrix = as.matrix(dcast(DT, V1 ~ V2, value.var = "sign")[,-1]))
        
        shiftedImg <- img * shiftMatrix
        
        imgFTrans <- fft(shiftedImg@imageMatrix)
    }else{
        imgFTrans <- fft(img@imageMatrix)
    }
    
    ## calculate the complex norm
    fourierSpectrum <- new("imageOneChannel", 
                           imageMatrix = sqrt(Re(Conj(imgFTrans) * imgFTrans)))
    
    ## perform logarithmic rescaling
    if(log.scale){
        fourierSpectrum <- logarithmicDynamicCompression(fourierSpectrum)
    }
    
    return(fourierSpectrum)
}
