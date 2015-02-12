# Wed Feb 11 20:47:23 PST 2015

# Prepare the Fastrak data

stations = read.csv('all_text_fastrak_locations_2015_02_01.txt'
                    , header=FALSE)

ft = read.csv('fastrak/all_text_fastrak_hour_2007_03.txt'
              , header=FALSE, col.names=c('time', 'station', 'count')
              , as.is=TRUE)

ft$time = strptime(ft$time, format="%m/%d/%Y %H:%M:%S")

bigone = ft[ft$station == 10401, ]
plot(bigone$time, bigone$count)
