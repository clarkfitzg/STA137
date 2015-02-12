fastrak = read.csv('~/data/fastrak.csv')

# This is unaccountably slow- takes a couple minutes
fastrak$time = as.POSIXct(fastrak$time)
