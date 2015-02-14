library(RUnit)

runtests = TRUE

zerorun = function(x, k){
    # Determine whether the zeros in x occur in a run of at least k.
    # Returns a logical vector which is TRUE if the corresponding 
    # entry in x is in such a run.
    iszero = x == 0
    r1 = rle(x)
    islong = (r1$lengths >= k)
    rep(islong, r1$lengths)
}

if (runtests){
    # zerorun
    x1 = c(rep(0, 5), 1, rep(0, 4))
    checkEquals(zerorun(x1, 5), c(rep(TRUE, 5), rep(FALSE, 5)))
}
