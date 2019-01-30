#' imputeALS
#'
#' @param data a matrix containing missing values that should be imputed
#' @param lambda the learning rate
#' @param r number of coefficients per sample and feature
#' @param iters the number of iterations the ALS should be executed
#' @param use.biases TRUE if the bias model should be used
#'
#' @usage
#' imputeALS(data, lambda, r, iters, use.biases)
#'
#' @return a matrix containing the original values with missing values replaced
#' by imputed values
#' @export imputeALS
#' @importFrom BEclear loss
#' @importFrom stats rnorm
#' @import futile.logger
#'
#' @references insertRef{Koren2009}{Raspository}
#' @references insertRef{Hastie2014}{Raspository}
#'
#' @examples
#' library(BEclear)
#' data("BEclearData")
#' batchEffect <- calcBatchEffects(
#' data = ex.data, samples = ex.samples,
#' adjusted = TRUE, method = "fdr")
#'
#' mdifs <- batchEffect$med
#' pvals <- batchEffect$pval
#'
#' summary <-calcSummary(mdifs, pvals)
#' cleared.data <- clearBEgenes(ex.data, ex.samples, summary)
#'
#' result <- imputeALS(cleared.data)
#'
imputeALS<- function(data, lambda = 0.75, r = 5, iters = 60, use.biases=FALSE){

    # copy the data
    D <- data

    # We initialise L and R with random values
    L <- matrix(rnorm(nrow(data) * r), nrow(data), r) / sqrt(r)
    R <- matrix(rnorm(r * ncol(data)), r, ncol(data)) / sqrt(r)

    currLoss <- loss(L,R, 1, D)$loss
    flog.info(paste("Loss for random L and R:", currLoss))

    if(use.biases){
        # we calculate the biases
        biasData<-mean(data, na.rm = TRUE)
        biasRows<-rowMeans(data - biasData, na.rm= TRUE)
        biasCols<-colMeans(data - biasData, na.rm= TRUE)

        # subtract the biases from the data
        D <- D - biasData - biasRows
        D <- t(t(D) - biasCols)
    }


    for(iter in 1:iters){

        # Now we iterate over the feature dimmension of L
        for(i in 1:dim(L)[[1]]){
            # We determine the revealed entries for the feature
            # And subset the data and R so to only retain the revealed entries
            revealedEntries <- !is.na(D[i,])
            y <- as.matrix(D[i, revealedEntries])
            x <- R[,revealedEntries]
            # We solve the linear equation for the feature
            L[i,] <- as.vector(solve(x %*% t(x) + diag(lambda, r), x %*% y))
        }

        # We iterate over the sample dimmension of R
        for(j in 1:dim(R)[[2]]){
            # We determine the revealed entries for the sample
            # And subset the data and L so to only retain the revealed entries
            revealedEntries <- !is.na(D[,j])
            y <- as.matrix(D[revealedEntries, j])
            x <- L[revealedEntries,]
            # We solve the linear equation for the sample
            R[,j] <- as.vector(solve(t(x) %*% x + diag(lambda, r), t(x) %*% y))
        }
        currLoss <- loss(L,R, 1, D)$loss

        flog.info(paste0("Loss for iteration ", iter, ": ", currLoss))
    }

    # L and R are multiplied to get the estimated values
    D <- L %*% R

    if(use.biases){
        # we add the biases again
        D <- t(t(D) + biasCols)
        D <- D + biasData + biasRows
    }

    # Missing values are replaced with estimated value

    for (i in seq_len(nrow(data)))
        for (j in seq_len(ncol(data)))
        {
            if (is.na(data[i, j])) {
                data[i, j] <- D[i, j]
            }
        }

    return(data)
}
