library(dequer)
BCA<- function(graph, v, retentionCoefficient = 0.4, tol = 0.001){
    p<-c()
    p[V(graph)] <- 0

    q <- queue()
    pushback(q, v)
    colorInVertex <- c()
    colorInVertex[v] <- 1

    while(length(q) > 0){
        i <- pop(q)
        w <- colorInVertex[i]
        colorInVertex[i] <- NA

        p[i] <- p[i] + retentionCoefficient * w

        # if all color is used up continuew to next element in queque
        if(w < tol){

            next
        }

        for(j in neighbors(graph, i, mode = "out")){
            if(!is.na(colorInVertex[j])){
                colorInVertex[j] <- colorInVertex[j] +
                    ((1 - retentionCoefficient) * w/degree(graph, i, mode = "out"))
            }else{
                pushback(q, j)
                colorInVertex[j] <- (1 - retentionCoefficient) * w/degree(graph, i, mode = "out")
            }
        }

    }

    return(p)
}
