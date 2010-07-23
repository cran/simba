"mps.f" <-
## check for what happens when all species are present or all are 
## not present on the focal summit!
function(x, foc=NULL, d.inc=FALSE, preso=FALSE){
	if(is.null(foc)){
		stop("A focal plot has to be specified!")
	}
	x <- data.frame(x)
	if(!d.inc){
		## check and elimation of non-occurring species, without warning
		x <- x[,colSums(x)!=0]
		## check for empty plots, without warning
		x <- x[rowSums(x)!=0,]
	}
	## create selector (the species composition of the focal plot
	## determines the next steps)
	sel <- x[foc,]
	## calculate the sum of occurrence for all species that
	## occur on the focal plot
	occ <- colSums(x>0)[sel>0]
	## take care for zero occurring species (that is the focal plot is empty)
	if(length(occ)==0) occ <- 0
	## calculate the sum of non-occurrence for all species that
	## do not occur on the focal plot
	nocc <- colSums(x==0)[sel==0]
	## take care for zero non-occurring species
	if(length(nocc)==0) nocc <- 0
	## sum all sums of occurrences and non-occurrences and
	## divide by the number of species and by the number of plots
	res <- sum(c(occ, nocc))/ncol(x)/nrow(x)
	if(preso) {
		res <- sum(occ)/length(occ)/nrow(x)
	}
	names(res) <- "mps.f"
	return(res)
}