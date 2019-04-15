#' Bookmark Coloring Algorithm
#'
#' @aliases BookmarkColoringAlgorithm
#'
#' @description This function calculates a teleportation vector from a given
#' starting node to other nodes in a given network.
#'
#' @export BCA
#' @import dequer
#' @import igraph
#'
#' @param graph an object of type \code{\link[igraph]{igraph}}.
#' @param v a starting vertex from the above graph. Can be either its identifier
#' or a igraph.vs object.
#' @param retentionCoefficient the restart probability for each node.
#' @param tol a tolerance treshold, indicating what the smalltest value of color
#' is, that should propagate further
#'
#' @return a preference/teleportation vector
#'
#' @references \insertRef{Berkhin2006}{Raspository}
#'
#' @examples
#' library(igraph)
#' g <- make_ring(5)
#' preferenceVector <- BCA(g, 1)
BCA<- function(graph, v, retentionCoefficient = 0.4, tol = 0.001){
    # initialise vector of transition chances
    p<-c()
    p[V(graph)] <- 0

    q <- queue()
    pushback(q, v)

    # initialise vector that indicates how much color is in one node
    colorInVertex <- c()
    colorInVertex[v] <- 1

    # execute as long queque q has elements
    while(length(q) > 0){
        i <- pop(q)
        w <- colorInVertex[i]
        # use up the color in node
        colorInVertex[i] <- NA


        p[i] <- p[i] + retentionCoefficient * w

        # if all color is used up continuew to next element in queque
        if(w < tol){

            next
        }

        # execute for all neighbors
        for(j in neighbors(graph, i, mode = "out")){
            if(!is.na(colorInVertex[j])){
                # add color to neighbor
                colorInVertex[j] <- colorInVertex[j] +
                    ((1 - retentionCoefficient) * w/degree(graph, i, mode = "out"))
            }else{
                # initialise color in neighbor
                pushback(q, j)
                colorInVertex[j] <- (1 - retentionCoefficient) * w/degree(graph, i, mode = "out")
            }
        }

    }

    return(p)
}
