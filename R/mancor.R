"mancor" <-
function(dis, classes, width=NULL, method="pearson", permutations=1000, alpha=0.05, trace=FALSE, ...) {
    ox <- dis
    oy <- classes
    if (!is.null(width)){
        oy <- floor(oy/width)
        oy.nms <- levels(as.factor(oy*width))
    }
    else {
        oy.nms <- unique(oy)
    }
    N <- length(ox)    
    strata <- unique(oy)
	stratan <- length(strata)
#	out <- data.frame(cbind(correlation=1:stratan, signif=1, nop=1))
#	if (trace) {cat(stratan, "classes: ")}
#	for (i in 1:stratan) {
#        tmp <- permcor2(x, ifelse(y==i, 1, 0), method=method, permutations=permutations, ...)
#        out[i,1] <- as.numeric(tmp$statistic)
#		out[i,2] <- as.numeric(tmp$signif)
#		out[i,3] <- length(y[y==i])
#		if (trace) {cat(paste(i," "))}
#	}
	tmp <- lapply(c(1:stratan), function(x) permcor2(ox, ifelse(oy==strata[x], 1, 0), method=method, permutations=permutations))
	statistic <- as.numeric(sapply(sapply(tmp, function(x) x[3]), function(x) x[1]))
	signif <- as.numeric(sapply(sapply(tmp, function(x) x[4]), function(x) x[1]))
	nop <- as.numeric(summary(as.factor(oy)))
	out <- cbind(nop, statistic, signif)
	out <- data.frame(out)
	rownames(out) <- oy.nms
	##bonferroni correction
	alpha <- alpha/stratan
	out$sig[out$signif >= alpha] <- "ns"
    out$sig[out$signif < alpha] <- "*"
    out$sig <- as.factor(out$sig)
	res <- list(call=match.call(), method=method, out=out, gesN=N, strata=stratan, permutations=permutations)
	class(res) <- "pclist"
	res
    }