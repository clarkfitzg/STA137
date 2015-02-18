library(RUnit)

runtests = TRUE

longrun = function(x, runtype, k){
    # Determine whether runtype in x occurs in a run of at least k.
    # Returns a logical vector which is TRUE if the corresponding 
    # entry in x is in such a run.
    if(is.na(runtype)){
        isruntype = is.na(x)
    }
    else{
        isruntype = x == runtype
    }
    r1 = rle(isruntype)
    islong = (r1$lengths >= k)
    rep(islong, r1$lengths)
}

getstation = function(stationnumber, alldata){
    # Return a particular station from alldata sorted on time
    s = alldata[alldata$station == stationnumber, ]
    s[order(s$time), ]
}

makealldates = function(dframe, by='hour'){
    # Given a data frame that has a column `time`, this function
    # returns a data frame containing equi-spaced dates.
    # All columns other than date are filled in with NA.
}

timeparts = function(dframe){
    # Given a data frame that has a column `time`, this function
    # returns a data frame including 
    # year, month, dayofweek, hour
}


#============================================================

# Test suite

if (runtests){
    # longrun
    x1 = c(rep(0, 5), 1, rep(0, 4))
    checkEquals(longrun(x1, 0, 5), c(rep(TRUE, 5), rep(FALSE, 5)))
    xwithNA = c(rep(NA, 5), 1, rep(NA, 4))
    checkEquals(longrun(xwithNA, NA, 5), c(rep(TRUE, 5), rep(FALSE, 5)))

    # makealldates
    small = data.frame(time=as.POSIXct('2000-01-01'))

    alldata = data.frame(station = c(1, 1, 2), time = c(2, 1, 1))
    s1 = getstation(1, alldata)
    s1expected = data.frame(station = c(1, 1), time = c(1, 2))
    # MUST be integer row names
    row.names(s1expected) = 2:1
    checkEquals(s1, s1expected)
}
