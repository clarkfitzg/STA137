load('fastrak.Rda')
library(lme4)
head(fastrak)
fit1 = lmer(sqrt(count) ~ hour + weekday + month + year + (1|station), fastrak)
summary(fit1)
s1 = summary(fit1)
a1 = anova(fit1)
a1
s1
hist(residuals(fit1))
# Lets pull the residual series into the dataframe
ls()
fastrak$residuals = residuals(fit1)
# Now we can examine the residuals for one station over a week, say
# Lets do a popular station
which.max(fastrak$count), ]
fastrak[which.max(fastrak$count), ]
# So 10401 is popular
?save
r1 = with(fastrak, residuals[station==10401 & year=2010 & month='August'])
r1 = with(fastrak, fastrak[station==10401 & year==2010 & month=='August'])
length(r1)
plot(r1)
r1 = with(fastrak, fastrak[station==10401 & year==2010 & month=='August', c('time', 'residuals')])
plot(r1$time, r1$residuals)
# But maybe this heavy traffic one is not indicative of all of them
# lets pick one that's more average
?ranef
class(fit1)
random1 = ranef(fit1)
random1
random1[abs(random1$station) < 1]
class(random1)
random1$station[abs(random1$station) < 1]
r1 = with(fastrak, fastrak[station==21200 & year==2010 & month=='August', c('time', 'residuals')])
plot(r1$time, r1$residuals)
library(dplyr)
?date
?Date
?as.Date
d1 = as.Date("%Y-%m-%d"
d1 = as.Date("2010-08-30")
d2 = as.Date("2010-08-30")
r1 = r1[(r1$time >= d1), ]
dim(r1)
d1
class(d1)
?as.POSIX
sapply(r1, class)
?as.POSIXct
d2 = as.POSIXct(d1)
d2
d2 = as.POSIXct('2010-08-21')
d2
r2 = r1[r1$time > d2, ]
dim(r2)
r2 = r1[r1$time > as.POSIXct('2010-08-25'), ]
dim(r2)
plot(r2$time, r2$residuals)
plot(r2$time, r2$residuals, type='l')
?savePlot
savePlot('weekresiduals.png')
savehistory('4_explore.R')
