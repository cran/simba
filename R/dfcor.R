"dfcor" <-
function(ox, y, method="pearson", permutations=1000, ...){
    N <- length(ox)
    vars <- names(y)
    varn <- length(vars)
    tmp <- apply(y, 2, function(x) permcor2(ox, x, method=method, permutations=permutations))
    out <- matrix(unlist(tmp), varn, 6+permutations, byrow=TRUE)[,3:5]
    out <- data.frame(out)
    names(out) <- c("corr", "sig", "nop")
    out$miss <- as.numeric(N)-as.numeric(out$nop)
    rownames(out) <- vars
    res <- list(call=match.call(), method=method, out=out, gesN=N, strata=varn, permutations=permutations)
	class(res) <- "pclist"
	res
}