"mama" <-
function(dat) {
	dat <- data.frame(dat)
	plot <- as.character(dat[,1])
	spec <- as.character(dat[,2])
	if(ncol(dat) < 3){
		if(ncol(dat) < 2)
			stop("Mama needs at least two columns (vectors)!")
		dat$pres <- 1	
		}
	pres <- dat[,3]
	dat <- data.frame(plot, spec, pres)
	wide <- reshape(dat, v.names="pres", idvar="plot", timevar="spec", direction="wide")
	wide[is.na(wide)] <- 0
	rownames(wide) <- wide[,1]
	names(wide) <- sub("pres\\.", "", names(wide))
	wide <- data.frame(wide[,-1])
	wide <- wide[order(rownames(wide)), ]
	wide <- wide[,order(names(wide))]
	res <- wide
	res
}