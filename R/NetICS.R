#' netICS
#'
#' @param adjacencyMatrix
#' @param networkGenes
#' @param mutationData
#' @param diffExpGenes
#' @param restartProbability
#'
#' @return
#' @export netICS
#' @import data.table
#'
#' @references \insertRef{Dimitrakopoulos2018}{Raspository}
#'
#' @examples
netICS <- function(adjacencyMatrix, networkGenes, mutationData, diffExpGenes,
                   restartProbability = 0.4){


    mutationInNetwork <- mutationData[networkGenes, on="Gene", nomatch = 0]
    diffExpInNetwork <- diffExpGenes[networkGenes, on="Gene", nomatch = 0]

    # connectivity is F in the paper
    connectivity <- performInsulatedHeatDiffusion(normaliseAdjacencyMatrix(adjacencyMatrix), restartProbability)
    connectivityBackward <- performInsulatedHeatDiffusion(normaliseAdjacencyMatrix(t(adjacencyMatrix)), restartProbability)

    # ranking mediator genes
    result<-prioritize(connectivity, connectivityBackward, networkGenes,
                           mutationData, mutationInNetwork, diffExpInNetwork)

    return(result)

}

#' Title
#'
#' @param diffExp1
#' @param diffExp2
#'
#' @return
#' @export
#' @importFrom stats pchisq
#'
#' @examples
combineDifferentialExpressions<-function(diffExp1, diffExp2){
    mergedDiff<-merge(diffExp1, diffExp2, by ="Gene", all=TRUE)
    mergedDiff[is.na(pval.x) & !is.na(pval.y), pval := pval.y]
    mergedDiff[!is.na(pval.x) & is.na(pval.y), pval := pval.x]
    mergedDiff[!is.na(pval.x) & !is.na(pval.y), pval := pchisq(-2 * (log(pval.x) + log(pval.y)) , 4)]
    return(mergedDiff[,.(Gene, pval)])
}

#' prioritize
#'
#' @param connectivity
#' @param connectivityBackward
#' @param networkGenes
#' @param mutationData
#' @param mutationInNetwork
#' @param diffExpInNetwork
#'
#' @return
#' @export prioritize
#' @import data.table
#' @importFrom RobustRankAggreg rhoScores
#' @importFrom stats median
#'
#' @references \insertRef{Dimitrakopoulos2018}{Raspository}
#'
#' @examples
prioritize<-function(connectivity, connectivityBackward, networkGenes,
                         mutationData, mutationInNetwork, diffExpInNetwork){


    if(!"Sample"  %in% colnames(diffExpInNetwork)){
        Ed<-diffuseSample(connectivityBackward, networkGenes, diffExpInNetwork)
    }

    scores <- data.table()

    for(sample in unique(mutationData$Sample)){
        Em<-diffuseSample(connectivity, networkGenes, mutationInNetwork[Sample == sample])

        if("Sample"  %in% colnames(diffExpInNetwork)){
            Ed<-diffuseSample(connectivityBackward, networkGenes, diffExpInNetwork[Sample == sample])
        }

        E <- Em * Ed
        scores <- rbind(scores, data.table(Gene = networkGenes$Gene, Sample = sample, Score = E))
    }

    scores[, rank.in.sample:=rank(-Score), by=.(Sample)]

    ranks<-scores[, .(med=median(rank.in.sample), sum=sum(rank.in.sample),
                      rho = rhoScores(rank.in.sample/max(rank.in.sample))),
                  by=.(Gene)]

    return(ranks)
}

#' diffuseSample
#'
#' @param connectivity
#' @param networkGenes
#' @param S
#'
#' @return
#' @export diffuseSample
#' @import data.table
#'
#' @references \insertRef{Dimitrakopoulos2018}{Raspository}
#' @references \insertRef{Leiserson2015}{Raspository}
#' @references \insertRef{Chung2007}{Raspository}
#'
#' @examples
diffuseSample<-function(connectivity, networkGenes, S){
    positions <- networkGenes$Gene %in% S$Gene
    weights <- rep(1/sum(positions), sum(positions)) %*% connectivity[positions,]
    return(as.vector(weights))
}

#' performInsulatedHeatDiffusion
#'
#' @param adjacencyMatrix
#' @param restartProbability
#'
#' @return
#' @export performInsulatedHeatDiffusion
#' @import data.table
#'
#' @references \insertRef{Dimitrakopoulos2018}{Raspository}
#'
#' @examples
performInsulatedHeatDiffusion <- function(adjacencyMatrix, restartProbability){
    temperature <- diag(dim(adjacencyMatrix)[1]) - (1 - restartProbability) * adjacencyMatrix
    return(restartProbability * solve(temperature))
}


#' normaliseAdjacencyMatrix
#'
#' @param adjacencyMatrix
#'
#' @return
#' @export normaliseAdjacencyMatrix
#' @import data.table
#'
#' @references \insertRef{Dimitrakopoulos2018}{Raspository}
#'
#' @examples
normaliseAdjacencyMatrix <- function(adjacencyMatrix){
    return(adjacencyMatrix %*% diag(1/colSums(adjacencyMatrix)))
}
