#' Title
#'
#' @param img 
#' @param kernel 
#' 
#' @references \insertRef{bookConv}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
convolveImage <- function(img, kernel){
    ## pad kernel with zeros
    if(!all(dim(img@image) == dim(kernel))){
        paddedKernel <- matrix(0, nrow = nrow(img@image), ncol = ncol(img@image))
        
        for(i in seq(nrow(kernel))){
            
            for(j in seq(ncol(kernel))){
                paddedKernel[i,j] <- kernel[i,j]
                }
        }
        
        kernel <- paddedKernel
    }
    
    fKernel <- fft(kernel)
    
    fImg <- fft(img@image)
    
    imgFilterApplied <- Re(fft(fKernel * fImg, inverse = TRUE))
    
    
    ## rescaling
    return(new("imageBW", image = (imgFilterApplied  - min(imgFilterApplied ))/ 
        (max(imgFilterApplied ) - min(imgFilterApplied ))))
    
}

#' Title
#' 
#' @importFrom mvtnorm dmvnorm
#'
#' @param mean 
#' @param sigma 
#' @param size 
#' 
#' @references \insertRef{Weickert2019}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
gaussianKernel <- function(mean = c(10,10), sigma = 0.5, n = 20000){
    DT <- data.table(rmvnorm(n=n, mean = c(4,4), sigma = diag(sigma, 2)))
    DT <- DT[, .(x = round(V1), y = round(V2))]
    DT <- DT[x > 0 & y >0,]
    DT <- DT[,.N, by=c("x", "y")]
    DT <- DT[,.(x,y, freq = N/sum(N)) ,]
    DT_gaussian <- dcast(DT, x~y, value.var = "freq")
    
    gaussianMatrix <- as.matrix(DT_gaussian[,-1])
    gaussianMatrix[is.na(gaussianMatrix)] <- 0.0
    rownames(gaussianMatrix) <- DT_gaussian[,x]
    
    return(gaussianMatrix)
}

#' Title
#'
#' @param type 
#' @param direction 
#' 
#' @references \insertRef{bookSobel}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
derivateKernel <- function(type = "sobel", direction = c("x", "y")){
    if(type == "sobel"){
        if(direction[1] == "x"){
            return(matrix(c(1,2,1,0,0,0,-1,-2,-1), nrow = 3, byrow = TRUE)/8)
        }else if(direction[1] == "y"){
            return(matrix(c(1,2,1,0,0,0,-1,-2,-1), nrow = 3)/8)
        }
    }else{
        stop(paste(type, "is not (yet) defined as derivate method"))
    }
}

#' Title
#'
#' @param img 
#' @param sigma 
#' 
#' @references \insertRef{Weickert2019}{Raspository} 
#'
#' @return
#' @export
#'
#' @examples
laplacianOfGaussian <- function(img, sigma){
    ## convolve with a Gaussian
    imgGauss <- convolveImage(img, gaussianKernel(sigma = sigma))
    
    ## calculate dxx and dyy of the image convolved with the Gaussian 
    dx <- convolveImage(imgGauss, derivateKernel(direction = "x"))
    dy <- convolveImage(imgGauss, derivateKernel(direction = "y"))
    
    dxx <- convolveImage(dx, derivateKernel(direction = "x"))
    dyy <- convolveImage(dy, derivateKernel(direction = "y"))
    

    
    return(new("imageBW", image = (dxx@image + dyy@image) / 2))
}