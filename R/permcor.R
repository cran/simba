"permcor" <- 
function(x, y, subsetter, method="pearson", permutations=1000, alpha=0.05, trace=FALSE, ...) {
    N <- length(x)
	strata <- levels(as.factor(subsetter))	
	stratan <- length(strata)
	out <- data.frame(cbind(statistic=1:stratan, signif=1, nop=1))
	if (trace) {cat(stratan, "classes: ")}
	for (i in 1:stratan) {
		tmp <- permcor2(x[subsetter==(strata[i])], y[subsetter==(strata[i])], method=method, permutations=permutations, ...)
		out[i,1] <- as.numeric(tmp$statistic)
		out[i,2] <- as.numeric(tmp$signif)
		out[i,3] <- as.numeric(tmp$n)
		if (trace) {cat(paste(i,""))}
	}
	##bonferroni correction
	alpha <- alpha/stratan
	out$sig[out$signif >= alpha] <- "ns"
    out$sig[out$signif < alpha] <- "*"
    out$sig <- as.factor(out$sig)
    rownames(out) <- strata
	res <- list(call=match.call(), method=method, out=out, gesN=N, strata=stratan, permutations=permutations)
	class(res) <- "pclist"
	res
    }