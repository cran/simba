"simn" <-
function(veg, coord, dn, presabs=TRUE, d.inc=FALSE, ...){
    veg <- data.frame(veg)
    if (is.null(rownames(veg))){
        rownames(veg) <- c(1:nrow(veg))
    }
    dist.all <- dist(coord[,1:2])
    if (max(dist.all) < min(dn)){
        stop("Are you sure that the neighbour definition is correct?")
    }
    sia <- function(veg) {
        res <- t(replicate(nrow(x), colSums(x)/nrow(x)))
        return(res)
    }
    if(length(dn)==1){
        dist.nbs <- ifelse(as.matrix(dist.all) <= dn, 1, 0)
    }
    else{
        dist.nbs <- ifelse(((as.matrix(dist.all) == 0) | ((as.matrix(dist.all) >= min(dn)) & (as.matrix(dist.all) <= max(dn)))), 1, 0)
    }
    n.plots <- c(1:nrow(veg))
    x1 <- ifelse(veg > 0, 1, 0)
    tmp1 <- lapply(n.plots, function(x) x1[dist.nbs[ ,x]==1,])
    x1.tmp <- t(sapply(tmp1, function(x) colSums(x)/nrow(x)))
    if (presabs) {
        x2 <- ifelse(veg > 0, 0, 1)
        tmp2 <- lapply(n.plots, function(x) x2[dist.nbs[ ,x]==1,])
        x2.tmp <- t(sapply(tmp2, function(x) colSums(x)/nrow(x)))
        x.tmp <- x1.tmp*x1 + x2.tmp*x2
    }
    else{
        x.tmp <- x1.tmp*x1
    }
    sim2nbs <- rowSums(x.tmp)/sum(colSums(x.tmp)!=0)
    if (d.inc) {
        sim2nbs <- rowSums(x.tmp)/ncol(x.tmp)
    }
    nbs <- sapply(tmp1, function(x) nrow(x)-1)
    n.spec <- rowSums(ifelse(veg > 0, 1, 0))
    res <- data.frame(n.spec, nbs, sim2nbs)
    rownames(res) <- rownames(veg)    
    return(res)
}