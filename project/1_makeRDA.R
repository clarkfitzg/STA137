fastrak = read.csv('~/data/fastrak.csv')

fastrak$station = as.factor(fastrak$station)

# This line is unaccountably slow- takes a couple minutes
fastrak$time = as.POSIXct(fastrak$time)

fastrak$year = as.factor(format(fastrak$time, '%Y'))
fastrak$month = as.factor(months(fastrak$time))
fastrak$weekday = as.factor(weekdays(fastrak$time))
fastrak$hour = as.factor(format(fastrak$time, '%H'))

# Drop those that clearly had little to no data
# Plots justify these actions
maxima = with(fastrak, tapply(count, station, max))
smallmax = names(maxima[maxima < 20])
means = with(fastrak, tapply(count, station, mean))
smallmean = names(means[means < 1])
bothsmall = intersect(smallmax, smallmean)

fastrak = fastrak[!(fastrak$station %in% bothsmall), ]

save(fastrak, file='fastrak.Rda')
