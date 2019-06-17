#' Title
#'
#' @param H
#' @param C
#' @param X
#' @param m
#'
#' @return
#' @export
#'
#' @examples
calculateRGBvalue <- function(H, C, X, m){
   if(H >= 0 && H <= 1){
       return(c(m + C, m + X, m))
   }else if(H >= 0 && H <= 2){
       return(c(m + X, m + C, m))
   }else if(H >= 0 && H <= 3){
       return(c(m, m + C, m + X))
   }else if(H >= 0 && H <= 4){
       return(c(m, m + X, m + C))
   }else if(H >= 0 && H <= 5){
       return(c(m + X, m, m + C))
   }else if(H >= 0 && H <= 6){
       return(c(m + C, m, m + X))
   }else{
       return(c(0,0,0))
   }
}


#' Title
#'
#' @param hsvArray
#'
#' @return
#' @export
#' @importFrom abind abind
#'
#' @examples
hsvArrayToRgb <- function(hsvArray){

    # Calculate the chroma
    C <- hsvArray[,,3] * hsvArray[,,2]

    H<- hsvArray[,,1] / 60

    X <-  C * (1 - abs(H %% 2 - 1))

    #https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB
    m <- hsvArray[,,3] - C
    rgb<-mapply(FUN = calculateRGBvalue, H = H, C = C, X = X, m = m)

    rgbArray<-abind(matrix(rgb[1,], nrow = nrow(hsvArray)),
                matrix(rgb[2,], nrow = nrow(hsvArray)),
                matrix(rgb[3,], nrow = nrow(hsvArray)),
                along = 3)


    return(rgbArray)
}

#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @examples
imageRGBFromHSV <- function(img){
    return(new("imageRGB", original = hsvArrayToRgb(img@original),
               current = hsvArrayToRgb(img@current),
               operations = img@operations))
}

#' Title
#'
#' @param img
#' @param chPortion
#'
#' @return
#' @export
#'
#' @examples
imageBWFromRGB <- function(img, chPortion = c(0.33, 0.33, 0.33)){
    if(sum(chPortion) > 1){
        stop("Channel portions mustn't add up to more than one.")
    }
    original <- img@original[,,1] * chPortion[1] + img@original[,,2] * chPortion[2] + img@original[,,3] * chPortion[3]
    current <- img@current[,,1] * chPortion[1] + img@current[,,2] * chPortion[2] + img@current[,,3] * chPortion[3]
    return(new("imageBW", original = original, current = current, operations = img@operations))
}

#' Title
#'
#' @param max
#' @param maxIndex
#' @param min
#' @param r
#' @param g
#' @param b
#'
#' @return
#' @export
#'
#' @references \insertRef{Weickert2019}{Raspository}
#'
#' @examples
calculateHUE <- function(max, maxIndex, min, r, g, b){
    h <- 0.0
    if(max == min){
        return(h)
    }else if(maxIndex == 1){
        h <- 60.0 * ((g - b)/(max - min))
    }else if(maxIndex == 2){
        h <- 60.0 * (2.0 + (b - r)/(max - min))
    }else if(maxIndex == 3){
        h <- 60.0 * (4.0 + (r - g)/(max - min))
    }

    # if the value is negativ add 360° to it
    if(h >= 0){
        return(h)
    }else{
        return(h + 360)
    }
}

#' Title
#'
#' @param rgbArray
#'
#' @return
#' @export
#' @importFrom abind abind
#'
#' @references \insertRef{Weickert2019}{Raspository}
#'
#' @examples
rgbArrayToHsv <- function(rgbArray){
    # get the maximal color and its index in each pixel
    max <- apply(rgbArray, c(1,2), max)
    maxIndex <-apply(rgbArray, c(1,2), which.max)
    # get the minimal color in each pixel
    min <- apply(rgbArray, c(1,2), min)

    # calculate the hue for each pixel
    h <- mapply(FUN = calculateHUE, max = max, maxIndex = maxIndex, min = min,
                r = rgbArray[,,1], g = rgbArray[,,2], b = rgbArray[,,3])
    # convert vector back to matrix
    h <- matrix(h, ncol = ncol(max))

    # calculate saturation
    s <- (max - min)/max
    # set values to zero, where max is 0 (division by zero -> NA)
    s[is.na(s)] <- 0
    # max is equal to v (value/brightness)
    v <- max

    # bind matrices together to array and return
    hsvArray <- abind(h, s, v, along = 3)
    return(hsvArray)
}

#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @references \insertRef{Weickert2019}{Raspository}
#'
#' @examples
imageHSVFromRGB <- function(img){
    return(new("imageHSV", original = rgbArrayToHsv(img@original),
               current = rgbArrayToHsv(img@current),
               operations = img@operations))
}


#' Title
#'
#' @param img
#' @param transformPaletteFunction
#' @param method
#'
#' @return
#' @export
#'
#' @references \insertRef{Floyd}{Raspository}
#' @references \insertRef{Jarvis1976}{Raspository}
#'
#' @examples
errorDiffusiondDithering <- function(img, transformPaletteFunction = round,
                                     method = c("FS", "mae")){
    pixel <- img@current

    n <- dim(pixel)[1]
    m <- dim(pixel)[2]

    for(y in seq(m)){
        for(x in seq(n)){

            oldPixel <- pixel[x,y]
            newPixel <- transformPaletteFunction(oldPixel)

            error <- oldPixel - newPixel

            pixel[x,y] <- newPixel

            if(method[1] == "FS"){
                if(x < n){
                    pixel[x + 1, y] <- pixel[x + 1, y] + error * 7/16
                }

                if(x > 1 && y < m){
                    pixel[x - 1, y + 1] <- pixel[x - 1, y + 1] + error * 3/16
                }

                if(y < m){
                    pixel[x, y + 1] <- pixel[x, y + 1] + error * 5/16
                }

                if(x < n && y < m){
                    pixel[x + 1, y + 1] <- pixel[x + 1, y + 1] + error * 1/16
                }
            }else if(method[1] == "mea"){
                if(x < n){
                    pixel[x + 1, y    ] <- pixel[x + 1, y    ] + error * 7/48
                }
                if(x < n - 1){
                    pixel[x + 2, y    ] <- pixel[x + 2, y    ] + error * 5/48
                }

                if(x > 2 && y < m){
                    pixel[x - 2, y + 1] <- pixel[x - 2, y + 1] + error * 3/48
                }

                if(x > 1 && y < m){
                    pixel[x - 1, y + 1] <- pixel[x - 1, y + 1] + error * 5/48
                }

                if(y < m){
                    pixel[x    , y + 1] <- pixel[x    , y + 1] + error * 7/48
                }


                if(x < n && y < m){
                    pixel[x + 1, y + 1] <- pixel[x + 1, y + 1] + error * 5/48
                }

                if(x < n - 1 && y < m){
                    pixel[x + 2, y + 1] <- pixel[x + 2, y + 1] + error * 3/48
                }

               if(x > 2 && y < m - 1){
                    pixel[x - 2, y + 2] <- pixel[x - 2, y + 2] + error * 1/48
                }

                if(x > 1 && y < m - 1){
                    pixel[x - 1, y + 2] <- pixel[x - 1, y + 2] + error * 3/48
                }

                if(y < m - 1){
                    pixel[x    , y + 2] <- pixel[x    , y + 2] + error * 5/48
                }


                if(x < n && y < m - 1){
                    pixel[x + 1, y + 2] <- pixel[x + 1, y + 2] + error * 3/48
                }

                if(x < n - 1 && y < m - 1){
                    pixel[x + 2, y + 2] <- pixel[x + 2, y + 2] + error * 1/48
                }

            }


        }
    }

    ditheredImage <- new(class(img)[[1]], original = img@original,
                         current = pixel, operations = img@operations)

    return(cropPixels(ditheredImage))
}


#' Lattice Blotzman Dithering
#'
#' @param img 
#' @param epsilon 
#' @param minimalTreshold 
#'
#' @return
#' @export
#' 
#' @references \insertRef{Hagenburg2009}{Raspository}
#'
#' @examples
lbDithering <- function(img, epsilon = 0.5, minimalTreshold = 0.01){
    
    i <- 0
    difference <-epsilon + 1
    while(difference > epsilon){
        imgAtNewStep <- dissipatePixel(img = img, minimalTreshold = minimalTreshold)
        
        difference <- norm(imgAtNewStep - img, type = "2")
        print(difference)
        
        img <- imgAtNewStep

        i <- i +1
    }
    
    return(img)
}
