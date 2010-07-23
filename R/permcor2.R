"permcor2" <- 
function(x, y, method="pearson", permutations=1000, subset=NULL, complete=TRUE, loop=FALSE, ...) {
    if (!is.null(subset)) {
        x <- as.vector(x)[subset]
        y <- as.vector(y)[subset]
    }
    if(complete) {
        ok <- complete.cases(x, y)
        x <- x[ok]
        y <- y[ok]
    }
    x <- as.vector(as.matrix(x), mode="numeric")
    y <- as.vector(as.matrix(y), mode="numeric")
    if(length(x)!=length(y)) {
        stop("the two vectors do not share the same length")
    }
	N <- length(y)
	statistic <- as.numeric(cor(x, y, method=method, ...))
	variant <- method
	if (loop) {
		perms <- rep(NA, permutations)
		for (i in 1:permutations) {
			permsy <- sample(y, N)
			perms <- cor(x, y, method=method)
			}
		}
	else {
		permsy <- sapply(1:permutations, function(x) sample(y, N))
		perms <- as.numeric(cor(x, permsy, method=method, ...))
		}
	if (statistic >= 0) {
		signif <- sum(perms >= statistic)/permutations
	}
	else {
		signif <- sum(perms <= statistic)/permutations
	}
	res <- list(call=match.call(), method=variant, statistic=statistic, signif=signif, n=N, permutations=permutations, perms=perms)	
	class(res) <-"permcor"
	res
}