google = read.csv('data/google_timeseries.csv')
head(google)
sapply(google, class)
plot(google[, 2])
names(google) = c('label', 'value')
google$x = 1:nrow(google)

# Use orthogonal polynomials
fit = lm(value ~ poly(x, 10), data=google)

summary(fit)

# Try dropping the zeros
# => that 4th degree polynomial becomes significant
nozero = google[google$value != 0, ]
fit2 = lm(value ~ poly(x, 10), data=nozero)
summary(lm(value ~ poly(x, 9), data=nozero))
summary(fit2)

# Moving average
attach(google)
plot(x, filter(y, filter=rep(1/9, 9), sides=2), type='l')
lines(x, filter(y, filter=rep(1/9, 9), sides=2), type='l')
