"sim.pat" <-
function(veg, coord=NULL, dn, presabs=TRUE, test=TRUE, permutations=100, ...){
    p <- permutations    
    if (is.null(coord)){
        sim.start <- sima(veg, presabs=presabs, ...)
        if(test){
#           computing random matrices (still subject to change)
            avSR <- mean(colSums(ifelse(veg > 0, 1, 0)))
#           veg.rndm <- lapply(c(1:p), function(x) makead2(ncol(veg), nrow(veg), avSR=avSR))
            veg.rndm <- lapply(c(1:p), function(x) apply(veg, 2, function(x) sample(x)))
            sim.perms <- sapply(veg.rndm, function(x) sima(x, presabs=presabs, ...)[,1])
            sig.mat <- sim.perms - sim.start
            sig.high <- (rowSums(ifelse(sig.mat<0, 0, 1))+1)/(p+(1/p))
            sig.low <- (rowSums(ifelse(sig.mat>0, 0, 1))+1)/-(p+(1/p))
#           compare againts the rowMeans of the results of the permuted runs, if it is above mean, significance is tested on the upper tail and vice versa
            sim.mean <- apply(sim.perms, 1, "mean")
            sim.test <- ifelse(sim.start<sim.mean, sig.low, sig.high)
            sig <- ifelse(abs(sim.test) < 0.05, "*", "ns")
            sig.prefix <- ifelse(sim.test < 0, "-", "+")
            sims <- data.frame(cbind(sim.start, sim.test, sig, sig.prefix))
            names(sims) <- c("sim2all", "sim.test", "sig", "sig.prefix")
        }
        else{
            sims <- sim.start
        }
    }
    
    else {
        sim.start <- simn(veg, coord=coord, dn=dn, presabs=presabs, ...)
        if(test){
#           permute the original species matrix
            veg.rndm <- lapply(c(1:p), function(x) apply(veg, 2, function(x) sample(x)))
#           calculate similarity to neighbors for each permuted matrix, write results into list
            sim.perms <- sapply(veg.rndm, function(x) simn(x, coord=coord, dn=dn, presabs=presabs, ...)[,3])
            sig.mat <- sim.perms - sim.start[,3]
            sig.high <- (rowSums(ifelse(sig.mat<0, 0, 1))+1)/(p+(1/p))
            sig.low <- (rowSums(ifelse(sig.mat>0, 0, 1))+1)/-(p+(1/p))
#           compare againts the rowMeans of the results of the permuted runs, if it is above mean, significance is tested on the upper tail and vice versa
            sim.mean <- apply(sim.perms, 1, "mean")
            sim.test <- ifelse(sim.start[,3]<sim.mean, sig.low, sig.high)
            sig <- ifelse(abs(sim.test) < 0.05, "*", "ns")
            sig.prefix <- ifelse(sim.test < 0, "-", "+")
            sims <- data.frame(cbind(sim.start, sim.test, sig, sig.prefix))
        }
        else{
            sims <- sim.start
        }
    }
    return(sims)
}