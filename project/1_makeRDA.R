fastrak = read.csv('fastrak.csv')

source('functions.R')

fastrak$station = as.factor(fastrak$station)

# This line is unaccountably slow- takes a couple minutes
fastrak$time = as.POSIXct(fastrak$time)

fastrak = preclean(fastrak)

fastrak = addtimeparts(fastrak)

save(fastrak, file='fastrak.Rda')
