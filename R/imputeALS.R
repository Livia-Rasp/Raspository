#' imputeALS
#'
#' @param data
#' @param lambda
#' @param gamma
#' @param r
#' @param iters
#'
#' @return
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
imputeALS<- function(data, lambda = 0.75, r = 10, iters = 80){
    # We initialise L and R with random values
    L <- matrix(rnorm(nrow(data) * r), nrow(data), r) / sqrt(r)
    R <- matrix(rnorm(r * ncol(data)), r, ncol(data)) / sqrt(r)

    currLoss <- loss(L,R, 1, data)$loss
    flog.info(paste("Loss for random L and R:", currLoss))

    for(iter in 1:iters){

        # Now we iterate over the feature dimmension of L
        for(i in 1:dim(L)[[1]]){
            # We determine the revealed entries for the feature
            # And subset the data and R so to only retain the revealed entries
            revealedEntries <- !is.na(data[i,])
            y <- as.matrix(data[i, revealedEntries])
            x <- R[,revealedEntries]
            # We solve the linear equation for the feature
            L[i,] <- as.vector(solve(x %*% t(x) + diag(lambda, r), x %*% y))
        }

        # We iterate over the sample dimmension of R
        for(j in 1:dim(R)[[2]]){
            # We determine the revealed entries for the sample
            # And subset the data and L so to only retain the revealed entries
            revealedEntries <- !is.na(data[,j])
            y <- as.matrix(data[revealedEntries, j])
            x <- L[revealedEntries,]
            # We solve the linear equation for the sample
            R[,j] <- as.vector(solve(t(x) %*% x + diag(lambda, r), t(x) %*% y))
        }
        currLoss <- loss(L,R, 1, data)$loss

        flog.info(paste0("Loss for iteration ", iter, ": ", currLoss))
    }

    # L and R are multiplied to get the estimated values
    D <- L %*% R

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
