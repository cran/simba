"sima" <-
function(veg, presabs=TRUE, d.inc=FALSE, ...){
    veg <- data.frame(veg)
    x1 <- ifelse(veg > 0, 1, 0)
    x1.tmp <- t(replicate(nrow(x1), colSums(x1)/nrow(x1)))
    if (presabs) {
        x2 <- ifelse(veg > 0, 0, 1)
        x2.tmp <- t(replicate(nrow(x2), colSums(x2)/nrow(x2)))
        x.tmp <- x1.tmp*x1 + x2.tmp*x2
    }
    else{
        x.tmp <- x1.tmp*x1
    }
    sim2all <- rowSums(x.tmp)/sum(colSums(x.tmp)!=0)
    if (d.inc) {
        sim2all <- rowSums(x.tmp)/ncol(x.tmp)
    }
    res <- data.frame(sim2all)
    rownames(res) <- rownames(veg)
    return(res)
}