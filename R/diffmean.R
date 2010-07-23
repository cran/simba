"diffmean" <-
function (x, y, permutations = 1000) {
    x <- as.numeric(as.vector(x))
    y <- as.numeric(as.vector(y))
    nx <- length(x)
    ny <- length(y)
    meanx <- mean(x)
    meany <- mean(y)
    diff <- as.numeric(mean(x)-mean(y))
    xy <- c(x, y)
    nxy <- length(xy)
    meanom <- mean(xy)
    varg <- (nx*var(x)+ny*var(y))/(nx+ny)
    F <- (nx*ny*(meanx-meany)^2)/((nx*var(x))+(ny*var(y)))
    permx <- sapply(1:permutations, function(x) sample(xy, nx))
    permy <- sapply(1:permutations, function(x) sample(xy, ny))
    permsxmean <- numeric(permutations)
    permsxvar <- permsxmean
    permsxmean <- apply(permx, 2, mean)
    permsxvar <- apply(permx, 2, var)
    permsymean <- numeric(permutations)
    permsyvar <- permsymean
    permsymean <- apply(permy, 2, mean)
    permsyvar <- apply(permy, 2, var)
    Fperm <- (nx*ny*(permsxmean-permsymean)^2)/((nx*permsxvar)+(ny*permsyvar))
    diffsxy <- permsxmean-permsymean
    if (diff >= 0) {	
		signif <- (1+sum(diffsxy >= diff))/(permutations+1)
	}
	else {
		signif <- (1+sum(diffsxy <= diff))/(permutations+1)
	}
	sigF <- sum(F <= Fperm)/permutations
    out <- list(call = match.call(), diff = diff, meanom=meanom,     mean.x=meanx, mean.y=meany, Fval = F, sig = signif, sigF = sigF, bootsM = diffsxy, bootsF = Fperm, permutations = permutations)
    class(out) <- "dmn"
    out
}