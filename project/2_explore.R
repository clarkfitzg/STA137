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
