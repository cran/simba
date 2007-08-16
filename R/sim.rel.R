"sim.rel" <-
function(veg, coord=NULL, dn, method="additive", ...){
    veg <- data.frame(veg)
    if (is.null(rownames(veg))){
        rownames(veg) <- c(1:nrow(veg))
    }
    x <- ifelse(veg>0, 1, 0)
    METHODS <- c("additive", "relative", "reverse")
	method <- pmatch(method, METHODS)
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
        n.spec <- as.numeric(rowSums(x))
        n.plots <- c(1:nrow(x))
        ox <- x
        tmp <- lapply(n.plots, function(x) ox[dist.nbs[ ,x]==1,])
        mean.alph <- sapply(tmp, function(x) mean(rowSums(x)))
        gamm <- sapply(tmp, function(x) ncol(x[,colSums(x)!=0]))
    }
    else {
        nnbs <- nrow(x[rowSums(x)!=0,])
        mean.alph <- mean(rowSums(x))
        gamm <- ncol(x[,colSums(x)!=0])
        n.spec <- gamm
    }
    if (method == 1) {
        sims <- gamm - mean.alph
    }
    if (method == 2) {
        sims <- gamm/mean.alph
    }
    if (method == 3) {
        sims <- mean.alph/gamm
    }
    res <- data.frame(cbind(nnbs, n.spec, sims))
    names(res)[3] <- METHODS[method]
    if (!is.null(coord)) {
        rownames(res) <- rownames(veg)
    }
    return(res)
}