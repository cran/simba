"pcol" <-
function(x, y, z = NULL, method="pearson", permutations=1000, solo=FALSE, ...) {
    if (!is.vector(x)) {
        x <- as.matrix(x)
        x <- x[row(x) > col(x)]
        }
    if(solo) {
        out <- mancor(x, y, method=method, permutations=permutations, ...)
        attr(out, "solo") <- TRUE
    }
    else {
    if (!is.vector(y)) {
        y <- as.matrix(y)
        y <- y[row(y) > col(y)]
        }
    if (is.null(z)) {
        out <- permcor2(x, y, method=method, permutations=permutations, ...)
    }
    else {
    if (!is.vector(z)) {
        z <- as.matrix(z)
        z <- z[row(z) > col(z)]
        }
    out <- permcor(x, y, z, method=method, permutations=permutations, ...)
    }
    }
    out$call <- match.call()
    return(out)
}