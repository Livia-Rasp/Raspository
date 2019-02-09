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
#' @references insertRef{Dimitrakopoulos2018}{Raspository}
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
    result<-prioritize(connectivity, connectivityBackward, networkGenes, rankMethod,
                           mutationData, mutationInNetwork, diffExpInNetwork)

    return(result)

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
#'
#' @references insertRef{Dimitrakopoulos2018}{Raspository}
#'
#' @examples
prioritize<-function(connectivity, connectivityBackward, networkGenes,
                         mutationData, mutationInNetwork, diffExpInNetwork){

    Ed<-diffuseSample(connectivityBackward, networkGenes, diffExpInNetwork)
    scores <- data.table()

    for(sample in unique(mutationData$Sample)){
        Em<-diffuseSample(connectivity, networkGenes, mutationInNetwork[Sample == sample])
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
#' @references insertRef{Dimitrakopoulos2018}{Raspository}
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
#' @references insertRef{Dimitrakopoulos2018}{Raspository}
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
#' @references insertRef{Dimitrakopoulos2018}{Raspository}
#'
#' @examples
normaliseAdjacencyMatrix <- function(adjacencyMatrix){
    return(adjacencyMatrix %*% diag(1/colSums(adjacencyMatrix)))
}


# library(RobustRankAggreg)
# library(data.table)
#
# mutationData <-fread("~/Data/NetICS/mutation_data_breast.txt", col.names = c("Gene", "Sample"), header = FALSE)
# networkGenes<-fread("~/Data/NetICS/network_genes.txt", header = FALSE, col.names = "Gene")
#
# diffExp<-fread("~/Data/NetICS/RNA_diff_expr_breast.txt", header = FALSE, col.names = c("Gene", "pval"))
# diffExp[, p.adjusted:=p.adjust(pval, method = "fdr")]
#
#
# tmp<-R.matlab::readMat("~/Data/NetICS/adj_lar_com.mat")
# adjacencyMatrix<-tmp$adj.lar.com
# rm(tmp)
#
# result<-netICS(adjacencyMatrix = adjacencyMatrix, networkGenes,
#                mutationData = mutationData, diffExpGenes = diffExp[p.adjusted < 0.05, .(Gene)])
# result[order(sum)]
