# search through many combinations of ARMA models
library(parallel)

load('cleaned.Rda')

sr33 = sr3[sr3$group == 3, ]
sr33$X = scale(sr33$res)

X = sr33$X

# ARMA Parameters to search over:
ap = expand.grid(ar=0:10, ma=0:10)

getaic = function(ar, ma, x){
    arima(x, order=c(ar, 0, ma), optim.control=list(maxit=1000))$aic
}

# Need to run this on a machine with at least ncores available.
ncores = 20
cl = makeCluster(ncores)

aicvals = clusterMap(cl, getaic, ap$ar, ap$ma, rep(list(X), nrow(ap)))

ap$aic = unlist(aicvals)

save(ap, file='aic_results.Rda')
