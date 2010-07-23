"auc" <- function(x, y, below.zero = TRUE){
	##calculates the area under curve (integral) for any given set of x- and y-coordinates.
	x <- x[!is.na(x)]
	y <- y[!is.na(x)]
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if(!below.zero){
		y.0 <- y <= 0
		y[y.0] <- 0
		}
	y1 <- y[-length(y)]
	y2 <- y[-1]
	dy <- y2 - y1
	dx <- x[-1] - x[-length(y)]
	dxy <- dx * (y1 + 0.5 * dy)
	integral <- as.numeric(sum(dxy))
    integral
}