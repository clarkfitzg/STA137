source('../functions.R')
load('../fastrak.Rda')

fastrak = preclean(fastrak)

fit6 = lm(count^(1/3) ~ year + month + weekday*hour + station
        + station:weekday + station:hour, data=fastrak)
