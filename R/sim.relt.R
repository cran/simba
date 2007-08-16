"sim.relt" <-
function(veg, coord=NULL, dn, method="additive", test=TRUE, permutations=100, ...){
    p <- permutations    
    sim.start <- sim.rel(veg, coord=coord, dn=dn, method=method)
    if(test){
        veg.rndm <- lapply(c(1:p), function(x) apply(veg, 2, function(x) sample(x)))
        sim.perms <- sapply(veg.rndm, function(x) sim.rel(x, coord=coord, dn=dn, method=method, ...)[,3])
        sig.mat <- sim.perms - sim.start[,3]
        if (is.null(coord)){
            sig.mat <- t(as.matrix(sig.mat))
            sim.perms <- t(as.matrix(sim.perms))
        }
        sig.high <- (rowSums(ifelse(sig.mat<0, 0, 1))+1)/(p+(1/p))
        sig.low <- (rowSums(ifelse(sig.mat>0, 0, 1))+1)/-(p+(1/p))
#    gegen rowMeans der ZufallsDinger vergleichen. wenn oberhalb, oberhalb testen, sonst unterhalb testen...
        sim.mean <- apply(sim.perms, 1, "mean")
        sim.test <- ifelse(sim.start[,3] < sim.mean, sig.low, sig.high)
        sig <- ifelse(abs(sim.test) < 0.05, "*", "ns")
        sig.prefix <- ifelse(sim.test < 0, "-", "+")
        sims <- data.frame(cbind(sim.start, sim.test, sig, sig.prefix))
    }
    else{
        sims <- sim.start
    }
    return(sims)
}