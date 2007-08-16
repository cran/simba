"het2nbs" <-
function(mat, coord=NULL, dn, method="soerensen", ...){
    if (class(mat) != "dist") {
            if (is.null(rownames(mat))){
                rownames(mat) <- c(1:nrow(mat))
            }
            mat.sim <- sim(mat, method=method, ...)
            sims <- as.matrix(mat.sim)
            code <- attr(mat.sim, "Labels")
        }
        else {
            sims <- as.matrix(mat)
            code <- attr(mat, "Labels")
        }      
    if (!is.null(coord)) {
        dist.all <- dist(coord[,1:2])
        if (max(dist.all) < min(dn)){
            stop("Are you sure that the neighbour definition is correct?")
        }
        if(length(dn)==1){
            dist.nbs <- ifelse(((as.matrix(dist.all) <= dn) & (as.matrix(dist.all) != 0)), 1, 0)
        }
        else{
            dist.nbs <- ifelse(((as.matrix(dist.all) == 0) | ((as.matrix(dist.all) >= min(dn)) & (as.matrix(dist.all) <= max(dn)))), 1, 0)
        }
        nnbs <- rowSums(dist.nbs)
        sims <- sims*dist.nbs
        m.sims <- rowSums(sims)/nnbs
        sims[sims==0] <- NA
        sd.sims <- apply(sims, 1, "sd", na.rm=TRUE)
    }
    else {
        nnbs <- nrow(sims)
        m.sims <- mean(sims[row(sims) < col(sims)])
        sd.sims <- sd(sims[row(sims) < col(sims)])
        code <- "all"
    }  
    het <- data.frame(cbind(nnbs, m.sims, sd.sims))
    rownames(het) <- code 
    return(het)
}