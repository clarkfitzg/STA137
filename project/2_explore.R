fastrak = read.csv('fastrak.csv')
dim(fastrak)
sapply(fastrak, class)
fastrak$time = as.POSIXct(fastrak$time)
sapply(fastrak, class)
length(unique(fastrak$station))
162 * 24 * 7 * 52 * 6
?tapply
head(fastrak)
?tapply
tapply(fastrak$count, fastrak$station, max)
m = tapply(fastrak$count, fastrak$station, mean)
hist(m)
m[m <10]
means = tapply(fastrak$count, fastrak$station, mean)
hist(means)
means[means < 10])
plot(means[means < 10])
# We have pretty clear separation of 12 stations which have means near 0
# Probably best to drop those from the analysis
savehistory('2_explore.R')
means[means < 1]
names(means[means < 1])
# These are the stations to drop
savehistory('2_explore.R')
maxima = tapply(fastrak$count, fastrak$station, max)
hist(maxima)
maxima[maxima < 10]
maxima[maxima < 100]
hist(means)
hist(means[means < 100])
hist(means[means < 50])
plot(means[means < 50])
# So what if we normalize the flow in each station so that they have mean 1?
names(means[means < 1])
lowmean = names(means[means < 1])
maxima[maxima < 40]
lowmax = maxima[maxima < 40])
lowmax
lowmax = names(maxima[maxima < 40])
lowmax
lowmean
?intersect
intersect(lowmean, lowmax)
# The ones with abnormally low means are also the ones with abnormally low maxima
plot(maxima[maxima < 100])
# In the maxima plot here we observe exactly the same type of behavior
# that we saw in the mean plot
length(maxima)
intersect(lowmean, lowmax)
lowboth = intersect(lowmean, lowmax)
lowboth
length(lowboth)
# So we're talking about dropping 12 of 162 series because
# visual inspection shows that they are low both in mean and in max
# clear outliers
# that leaves us with 150 series
# How big is our range?
# And do we have complete data?
ls()
lowboth = as.integer(lowboth)
lowboth
12700 %in% lowboth
head(fastrak)
fastrak2 = fastrak[!(fastrak$station %in% lowboth), ]
dim(fastrak2
)
dim(fastrak)
# So we dropped those
length(unique(fastrak2$station))
range(fastrak2$time)
# Lets see if we have complete data for 2008-2012
# Then we could fit an ANOVA model
5 * 365 * 24 * 150
# Should be that many observations
# Not accounting for leap year though
smalltime = unique(fastrak$time)[1:20]
smalltime
smalltime$year
class(smalltime)
month(smalltime)
months(smalltime)
smalltime
class(smalltime)
?POSIXct
str(smalltime)
?POSIXct
?weekdays
julian(smalltime)
?format
?format
?format.Date
smalltime
?format.Date
format(smalltime, '%Y')
ls()
fastrak2$year = format(fastrak2$time, '%Y')
head(fastrak2)
sapply(fastrak2, class)
fastrak2$year = as.integer(format(fastrak2$time, '%Y'))
sapply(fastrak2, class)
# We've extracted the year
# Let's filter down to where year is between 2008 and 2012
fastrak3 = fastrak2[(2008 <= fastrak2$year) & (fastrak2$year <= 2012), ]
dim(fastrak3)
dim(fastrak2)
5 * 365 * 24 * 150
# We expected this many observations
# But we have much fewer
# Which means we have missing data
# I may have not downloaded some months
# Since I did that part manually
fastrak3$months = months(fastrak3$time)
head(fastrak3)
?combn
?expand.grid
com = expand.grid(fastrak3$year, fastrak3$months)
?unique
# What are all combinations of year and month?
ym = unique(fastrak3[, c('year', 'months')])
ym
tapply(ym$year, ym$months, length)
# Looks like we have all the values in this period
# Thats reassuring
# It could be that some of the stations did not start in 2008- they started later
head(fastrak3)
allcounts = tapply(fastrak3$count, fastrak3$station, length)
length(allcounts)
# Wait, this should be 150?
# Maybe one of those stations came later
# ie, after 2012
# *two
plot(allcounts)
# This tells us exactly where we have complete data then
allcounts == max(allcounts))
names(allcounts == max(allcounts))
fullhistorystations = names(allcounts == max(allcounts))
length(fullhistorystations)
length(allcounts)
fullhistorystations = names(allcounts[allcounts == max(allcounts)])
length(fullhistorystations)
# That's better
# So we have 133 with full history, it appears
# Next step is to filter on those
?history
savehistory('3_explore.R')
