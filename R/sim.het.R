"sim.het" <-
function(mat, coord=NULL, dn, method="soerensen", test=TRUE, permutations=100, ...){
    p <- permutations    
    sim.start <- het2nbs(mat, coord=coord, dn=dn, method=method, ...)
    if(test){
        veg.rndm <- lapply(c(1:p), function(x) apply(mat, 2, function(x) sample(x)))
        sim.perms <- lapply(veg.rndm, function(x) het2nbs(x, coord=coord, dn=dn, method=method, ...))
        sim.perms.mean <- sapply(sim.perms, function(x) x[,2])
        sig.mat.mean <- sim.perms.mean - sim.start[,2]
        if (is.null(coord)){
            sig.mat.mean <- t(as.matrix(sig.mat.mean))
            sim.perms.mean <- t(as.matrix(sim.perms.mean))
        }
        sig.high.mean <- (rowSums(ifelse(sig.mat.mean<0, 0, 1))+1)/(p+(1/p))
        sig.low.mean <- (rowSums(ifelse(sig.mat.mean>0, 0, 1))+1)/-(p+(1/p))
#    gegen rowMeans der ZufallsDinger vergleichen. wenn oberhalb, oberhalb testen, sonst unterhalb testen...
        sim.mean.mean <- apply(sim.perms.mean, 1, "mean")
        sim.test.mean <- ifelse(sim.start[,2] < sim.mean.mean, sig.low.mean, sig.high.mean)
        sig.mean <- ifelse(abs(sim.test.mean) < 0.05, "*", "ns")
        sig.prefix.mean <- ifelse(sim.test.mean < 0, "-", "+")
#   hier fangen die sd-sachen an     
        sim.perms.sd <- sapply(sim.perms, function(x) x[,3])
        sig.mat.sd <- sim.perms.sd - sim.start[,3]
        if (is.null(coord)){
            sig.mat.sd <- t(as.matrix(sig.mat.sd))
            sim.perms.sd <- t(as.matrix(sim.perms.sd))
        }
        sig.high.sd <- (rowSums(ifelse(sig.mat.sd<0, 0, 1))+1)/(p+(1/p))
        sig.low.sd <- (rowSums(ifelse(sig.mat.sd>0, 0, 1))+1)/-(p+(1/p))
#    gegen rowMeans der ZufallsDinger vergleichen. wenn oberhalb, oberhalb testen, sonst unterhalb testen...
        sim.mean.sd <- apply(sim.perms.sd, 1, "mean")
        sim.test.sd <- ifelse(sim.start[,3] < sim.mean.sd, sig.low.sd, sig.high.sd)
        sig.sd <- ifelse(abs(sim.test.sd) < 0.05, "*", "ns")
        sig.prefix.sd <- ifelse(sim.test.sd < 0, "-", "+")
        
        sims <- data.frame(cbind(sim.start, sim.test.mean, sig.mean, sig.prefix.mean, sim.test.sd, sig.sd, sig.prefix.sd))
    }
    else{
        sims <- sim.start
    }
    out <- list(call = match.call(), sims = sims)
    return(out)
}