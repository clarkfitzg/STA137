library(lme4)
load('fastrak.Rda')
fastrak_nozero = fastrak[fastrak$count != 0, ]
dim(fastrak_nozero)
fit3 = lmer(sqrt(count) ~ hour + weekday + month + year + (1|station) +(1|station:weekday), fastrak_nozero)
a3 = anova(fit3)
s3 = summary(fit3)
a3
savehistory('nozero.R')
s3
ls()
fastrak_nozero$residuals = residuals(fit3)
head(fastrak_nozero)
r3 = ranef(fit3)
l1 = tapply(fastrak$count, fastrak$station, length)
l1
max(l1)
max(l1, na.rm=T)
l2 = tapply(fastrak_nozeron$count, fastrak_nozeron$station, length)
l2 = tapply(fastrak_nozeron$count, fastrak_nozero$station, length)
l2 = tapply(fastrak_nozero$count, fastrak_nozero$station, length)
max(l2)
max(l2, na.rm=T)
which.max(l2, na.rm=T)
which.max(l2)
l2
l2[l2 == max(l2)]
l2[l2 == max(l2, na.rm=T)]
savehistory('5_explore.R')
savehistory('6_explore.R')
