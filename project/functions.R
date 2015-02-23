runtests = TRUE

preclean = function(fastrak){
    # Perform all precleaning steps on the fastrak data

    # The zeros are most likely not valid readings.
    fastrak = fastrak[fastrak$count != 0, ]

    # The date range where the counts inexplicably doubled.
    a = as.POSIXct('2010-06-23')
    b = as.POSIXct('2010-08-04')
    toobig = with(fastrak, (a < time) & (time < b))

    fastrak = fastrak[!toobig, ]

    return(fastrak)
}

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

addtimeparts = function(dframe){
    # Given a data frame that has a column `time`, this function
    # returns a data frame including factors for
    # year, month, dayofweek, hour
    out = dframe
    tm = out$time
    out$year = as.factor(format(tm, '%Y'))
    out$month = as.factor(months(tm))
    out$weekday = as.factor(weekdays(tm))
    out$hour = as.factor(format(tm, '%H'))
    out
}


#============================================================

# Test suite

if (runtests){
    library(RUnit)


    # longrun
    x1 = c(rep(0, 5), 1, rep(0, 4))
    checkEquals(longrun(x1, 0, 5), c(rep(TRUE, 5), rep(FALSE, 5)))
    xwithNA = c(rep(NA, 5), 1, rep(NA, 4))
    checkEquals(longrun(xwithNA, NA, 5), c(rep(TRUE, 5), rep(FALSE, 5)))

    # addtimeparts
    smalltime = data.frame(time=as.POSIXct('2000-01-01'))
    expected_addtime = data.frame(time=smalltime$time, year=as.factor(2000)
                                  , month=as.factor('January')
                                  , weekday=as.factor('Saturday')
                                  , hour=as.factor('00')
                                  )
    checkEquals(expected_addtime, addtimeparts(smalltime))

    # getstation
    alldata = data.frame(station = c(1, 1, 2), time = c(2, 1, 1))
    s1 = getstation(1, alldata)
    s1expected = data.frame(station = c(1, 1), time = c(1, 2))
    # MUST be integer row names
    row.names(s1expected) = 2:1
    checkEquals(s1, s1expected)
}
