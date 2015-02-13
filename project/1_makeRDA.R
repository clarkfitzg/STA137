fastrak = read.csv('~/data/fastrak.csv')

fastrak$station = as.factor(fastrak$station)

# This line is unaccountably slow- takes a couple minutes
fastrak$time = as.POSIXct(fastrak$time)

fastrak$year = as.factor(format(fastrak$time, '%Y'))
fastrak$month = as.factor(months(fastrak$time))
fastrak$weekday = as.factor(weekdays(fastrak$time))
fastrak$hour = as.factor(format(fastrak$time, '%H'))

save(fastrak, file='fastrak.Rda')
