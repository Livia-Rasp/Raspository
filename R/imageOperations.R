`+.imageOneChannel` <- function(imgA, imgB){
    return(new("imageOneChannel", image = imgA@imageMatrix + imgB@imageMatrix))
}

`-.imageOneChannel` <- function(imgA, imgB){
    return(new("imageOneChannel", image = imgA@imageMatrix - imgB@imageMatrix))
}