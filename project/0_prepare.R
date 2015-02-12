# Wed Feb 11 20:47:23 PST 2015

# Prepare the Fastrak data
# 8 million hourly readings - cool!

files = list.files('fastrak')

readsingle = function(filename){
    read.csv(paste0('fastrak/', filename)
              , header=FALSE, col.names=c('time', 'station', 'count')
              , as.is=TRUE)
}

fastrak = do.call(rbind, lapply(files, readsingle))

fastrak$time = strptime(fastrak$time, format="%m/%d/%Y %H:%M:%S")

# Could also have done this by chunks
write.csv(fastrak, 'fastrak.csv', row.names=FALSE)
