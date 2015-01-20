#Set directory
#setwd('/Users/rexcheung/Dropbox/School/School Work/Winter 2015/STA 137/Discussion/Discussion2/')
#Set your own directory, either using code or manually

#Load csv file
#To download the data in the correct format, do right click -> 'save as' from smartsite. DO NOT open and copy/paste the data into excel/word, it will NOT work. 
data = read.csv('data/skiresort.csv', header = T)

#This is weekly data. For weeks that overlap two years, we treat the week belongs to the first year.
#We will use d = 52. For years with 53 weeks, we will average the values for the last two weeks. (This is not a very standard way, but it will do its job here).
tmp = data
#Year 2006-2007
tmp[156,2] = ceiling((tmp[156,2] + tmp[157,2])/2)
tmp = tmp[-157,]
#Year 2012-2013
tmp[468,2] = ceiling((tmp[468,2] + tmp[469,2])/2)
tmp = tmp[-469,]
rownames(tmp) = 1:nrow(tmp) #Reindexing the rows
data = tmp
data = data[-c(573:574),]
#To make things easier, I removed the last two data to make every year has full data

plot.ts(data[,2], xlab = 'Week', ylab = '', main = 'Time Series Plot for the search term "ski resort"')
#Note the variance of the data is changing

#Use the methods we learn in class to deseasonalize the data
#Method 1: small trend estimation
#As seen in lecture and hw 5, we can write the data in a matrix and the calculation will be easier
data.matrix = matrix(c(data[,2]), ncol = 52, byrow = T)
#If there is not enough data to form a complete matrix, fill it with NA values
#Estimate the small trend
mj = rowMeans(data.matrix, na.rm = T)
mj.matrix = matrix(rep(mj, 52), ncol = 52)
#This will create a 'mean matrix' with the same value for each row
detrended = data.matrix - mj.matrix
plot(as.numeric(t(detrended)), type = 'l', main = 'Method 1: Detrended data', ylab = '', xlab = 'Weeks')

## QUESTION- We can take log transform to stabilize variance. 
# Is it possible to do this on something like the above plot, if the values
# are positive?

#Estimate the seasonality using the detrended data
sk = colMeans(detrended, na.rm = T)
sk.matrix = matrix(rep(sk, 11), ncol = 52, byrow = T)
deseasonalized = detrended - sk.matrix
plot(as.numeric(t(deseasonalized)), type = 'l', main = 'Method 1: Deseasonalized data', ylab = '', xlab = 'Weeks')
#We see that after removing seasonality and trends, we still see some pattern in the data.
#This can be a sign of nonstationality. 

#Method 2: Moving average estimation
#d is even, so we need to slightly modify our Wt.
#With d = 52, we have q = 26, N = 11
#Step 1: Filtering
two.sided.filter = filter(data[,2], sides = 2, c(0.5, rep(1, 51), 0.5)/52)
filter.matrix = matrix(two.sided.filter, ncol = 52, byrow = T) #Put the filtered values into a matrix
mu.matrix = data.matrix - filter.matrix #This is the detrended matrix
plot(as.numeric(t(mu.matrix)), type = 'l', main = 'Method 2: Filtered data', ylab = '', xlab = 'Weeks')
#Step 2: Seasonal estimation
mu.k = colMeans(mu.matrix, na.rm = T)
sk = mu.k - mean(mu.k)
sk.matrix2 = matrix(rep(sk, 11), ncol = 52, byrow = T)
deseasonalized2 = mu.matrix - sk.matrix2
plot(as.numeric(t(deseasonalized2)), type = 'l', main = 'Method 2: Deseasonalized data', ylab = '', xlab = 'Weeks')

#Step 3: Trend reestimation
#I want to pull down the variance, so I will use two sided filtering with small q
deseasonalized2 = as.numeric(t(deseasonalized2))
filter.method2 = filter(deseasonalized2, sides = 2, rep(1,5)/5)
lines(filter.method2, col = 'red')
plot.ts(deseasonalized2-filter.method2, main = 'Residuals from method 2')

#Method 3: Differencing at lag d
diff.data = diff(data[,2], lag = 52)
plot(diff.data, type = 'l', main = 'Method 3: Differenced data at lag = 52')
#Detrend using filtering again
filter.method3 = filter(diff.data, sides = 2, rep(1,5)/5)
lines(filter.method3, col = 'red')
plot.ts(diff.data - filter.method3, main = 'Residual from method 3')
