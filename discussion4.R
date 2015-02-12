lake = read.table('data/lake.dat')
plot.ts(lake)

#We will do residual assessment with the lake data set
#First we fit a linear trend and quadratic trend, then compare the residuals
lake = lake[,1]
n = length(lake)
#Linear trend
t1 = 1:n
fit1 = lm(lake ~ t1)
resid1 = lake - fit1$fitted.values
#Recall the adjusted R^2 for the linear fit is 0.2649

#Quadratic trend
t2 = t1^2
fit2 = lm(lake ~ t1 + t2)
resid2 = lake - fit2$fitted.values
#Recall the adjusted R^2 for the quadratic fit is 0.3959 

#Plot the two residuals
par(mfrow = c(2,1))
plot.ts(resid1, main = 'Residuals from linear trend')
plot.ts(resid2, main = 'Residuals from quadratic trend')
par(mfrow = c(1,1))

############################################################################
#########################
#Method 1: The sample ACF
#########################
resid1.acf = acf(resid1)
acf(resid1, lag=10)
resid2.acf = acf(resid2)

#Check how many within bound
resid1.acf = resid1.acf$acf[2:20]  #The first acf value is of lag 0, which we ignore here
resid2.acf = resid2.acf$acf[2:20]  #The first acf value is of lag 0, which we ignore here
bounds = c(-1.96/sqrt(n), 1.96/sqrt(n))
sum(resid1.acf < bounds[2] & resid1.acf > bounds[1]) #16/19 are within the bounds
sum(resid2.acf < bounds[2] & resid2.acf > bounds[1]) #11/19 are within the bounds

#Both plots shows there's still some dependency at lag 3, and from residual 2 we see there might be some weak hidden preiodicity at far lags.
#The white noise assumption might be rejected

#Here is what the acf plot looks like for i.i.d data
x = rnorm(100)
x.acf = acf(x)


###############################
#Method 2: The Portmanteau test
###############################
#Using the built-in function
teststat.1 = numeric(20)
pvals.1 = numeric(20)
teststat.2 = numeric(20)
pvals.2 = numeric(20)
for(i in 1:20){
  test1 = Box.test(resid1, lag = i, type = 'Ljung')
  teststat.1[i] = test1$statistic  #The test statistics
  pvals.1[i] = test1$p.value       #The p-value
  
  test2 = Box.test(resid2, lag = i, type = 'Ljung')
  teststat.2[i] = test2$statistic
  pvals.2[i] = test2$p.value
}
#Comparing p-values
pvals.1 < 0.05
pvals.2 < 0.05

#The Box.test() is the R built-in Portmanteau test. The default for the type argument corresponds to the one from lecture notes.

#...or manually calculating the test statistics
#Residual 1
Q.1 = cumsum(resid1.acf^2) * n
pval1.manual = pchisq(Q.1, df = 1:19, lower.tail = F) #pchisq returns the probability on the lower tail of the chisq distribution of certain df for a given quantile value
#Setting lower.tail = T will give the probability on the right tail, which is what we what
pval1.manual < 0.05

#Residual 2
Q.2 = cumsum(resid2.acf^2) * n
pval2.manual = pchisq(Q.2, df = 1:19, lower.tail = F)
pval2.manual < 0.05
#Alternatively comparing the test statistics:
Q.1 > qchisq(0.95, df = 1:19)
Q.2 > qchisq(0.95, df = 1:19)

#Both version of calculation shows that the hypothesis of i.i.d. residuals is rejected at level 0.05. 

#Here's an example of what will the result be for i.i.d data
x.acf = x.acf$acf[2:20]
X = cumsum(x.acf^2)*100
X > qchisq(0.95, df = 1:19)
#It will reject the

####################
#Method 3: Rank test
####################
mu.pi = 1/4*n*(n-1)
sigma.pi = sqrt(1/72*n*(n-1)*(2*n+5))
alpha = 0.05
z = qnorm(1 - alpha/2)
#Find Pi for residual 1
Pi.1 = 0
for(j in 1:(n-1)){
  for(i in (j+1):n){
    if(resid1[i] > resid1[j]) Pi.1 = Pi.1 + 1
  }
}
P1 = abs(Pi.1 - mu.pi)/sigma.pi
P1 > z  #Comparing test statistics with critical value
#Find Pi for residual 2
Pi.2 = 0
for(j in 1:(n-1)){
  for(i in (j+1):n){
    if(resid2[i] > resid2[j]) Pi.2 = Pi.2 + 1
  }
}
P2 = abs(Pi.2 - mu.pi)/sigma.pi
P2 > z  #Comparing test statistics with critical value

#Both results in a false at alpha = 0.05, thus the assumption that the residuals are i.i.d. cannot be rejected. 
#This says there is no more trend in the data
#If we apply the test to the original data...
lake.1 = 0
for(j in 1:(n-1)){
  for(i in (j+1):n){
    if(lake[i] > lake[j]) lake.1 = lake.1 + 1
  }
}
l1 = abs(lake.1 - mu.pi)/sigma.pi
l1 > z
#We will reject the test, which says there's some trend in the data

#################
#Method 4: QQPlot
#################
q1 = qqnorm(resid1)
qqline(resid1)
cor(q1$x, q1$y)^2

q2 = qqnorm(resid2)
qqline(resid2)
cor(q2$x, q2$y)^2
#Evidence shows that the residuals are relatively normal (Use the critical value from P(R^2 < 0.987) = 0.05)
