library(lme4)
load('../fastrak.Rda')

# The zeros are most likely not valid readings.
fastrak = fastrak[fastrak$count != 0, ]

# The date range where the counts inexplicably doubled.
a = as.POSIXct('2010-06-23')
b = as.POSIXct('2010-08-04')
toobig = with(fastrak, (a < time) & (time < b))

fastrak = fastrak[!toobig, ]

# Let's have a random effect for station nested in weekday
fit5 = lmer(count^(1/3) ~ hour*weekday + month + year +
            (1|station) + (1|station:weekday), (1|station:hour)
            , data=fastrak)

fastrak$residuals = residuals(fit5)

# Save these for later inspection
fit5output = list(anova = anova(fit5)
                  )

save(fastrak, fit5output, file='fit5.Rda')
