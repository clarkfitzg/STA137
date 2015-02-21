library(lme4)
load('../fastrak.Rda')

# The zeros are most likely not valid readings.
fastrak = fastrak[fastrak$count != 0, ]

# The date range where the counts inexplicably doubled.
a = as.POSIXct('2010-06-23')
b = as.POSIXct('2010-08-04')
toobig = with(fastrak, (a < time) & (time < b))

fastrak = fastrak[!toobig, ]

fit4 = lmer(count^(1/3) ~ hour + weekday + month + year +
            (1|station) + (1|station:weekday), fastrak)

fastrak$residuals = residuals(fit4)

# Save these for later inspection
fit4output = list(anova = anova(fit4)
                  , summary = summary(fit4)
                  , randomeffects = ranef(fit4)
                  )

save(fastrak, fit4output, file='fit4.Rda')
