library(biglm)

source('../functions.R')
load('../fastrak.Rda')

fastrak = preclean(fastrak)

# Without chunking both lm and biglm will fail when they try to
# allocate 200 GB vectors

chunksize = as.integer(1e4)

fit6 = biglm(count^(1/3) ~ month + weekday*hour + station + station:weekday + station:hour,
        data=fastrak[1:chunksize, ])

for (i in 2:floor(nrow(fastrak) / chunksize)){
    indices = 1:chunksize + (i - 1) * chunksize
    fit6 = update(fit6, fastrak[indices, ])
}
