"mama" <-
function(dat) {
	dat <- dat[,1:3]
	names(dat) <- c("plot", "spec", "pres")
	wide <- reshape(dat, v.names="pres", idvar="plot", timevar="spec", direction="wide")
	wide[is.na(wide)] <- 0
	rownames(wide) <- wide[,1]
	names(wide) <- sub("pres\\.", "", names(wide))
	wide <- data.frame(wide[,-1])
	res <- wide
	res
}

