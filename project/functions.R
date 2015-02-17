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

getstation = function(stationnumber, alldata){
    # Return a particular station from alldata sorted on time
    s = alldata[alldata$station == stationnumber, ]
    s[order(s$time), ]
}

#============================================================

# Test suite

if (runtests){
    # zerorun
    x1 = c(rep(0, 5), 1, rep(0, 4))
    checkEquals(zerorun(x1, 5), c(rep(TRUE, 5), rep(FALSE, 5)))

    alldata = data.frame(station = c(1, 1, 2), time = c(2, 1, 1))
    s1 = getstation(1, alldata)
    s1expected = data.frame(station = c(1, 1), time = c(1, 2))
    # MUST be integer row names
    row.names(s1expected) = 2:1
    checkEquals(s1, s1expected)
}
