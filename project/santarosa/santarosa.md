# Santa Rosa

Fastrak is the California highway toll system.
The cars that drive through the toll have been counted.
We've downloaded the data for hourly counts from each Fastrak stations for
the past 7 years. There are about 8 million rows.
We examine the time series for station 4300, Santa Rosa, because it looks
to be more consistent than the others.



```r
# To produce the report
library(knitr)

# semitransparent plotting with alpha
library(scales)

# tested helper functions
source('../functions.R')

# contains the `fastrak` data frame with 8 million rows
load('../fastrak.Rda')
```

Here is the data of interest.


```r
sr1 = getstation(4300, fastrak)

kable(head(sr1))
```



|    |time                |station | count|year |month |weekday |hour |  residuals|
|:---|:-------------------|:-------|-----:|:----|:-----|:-------|:----|----------:|
|25  |2007-03-23 03:00:00 |4300    |    32|2007 |March |Friday  |03   | -2.4031615|
|170 |2007-03-23 04:00:00 |4300    |    35|2007 |March |Friday  |04   | -2.9898080|
|315 |2007-03-23 05:00:00 |4300    |   103|2007 |March |Friday  |05   | -1.5040558|
|460 |2007-03-23 06:00:00 |4300    |   231|2007 |March |Friday  |06   | -0.7732094|
|605 |2007-03-23 07:00:00 |4300    |   357|2007 |March |Friday  |07   | -0.8023823|
|750 |2007-03-23 08:00:00 |4300    |   411|2007 |March |Friday  |08   | -1.5260884|

```r
with(sr1, plot(time, count, col=alpha('black', 0.1)))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

We observe a couple problems. In the 150 stations all the data shows the 
pattern of having nearly double the count in July of 2010. Additionally we
have missing data encoded as 0's.

3 or more zeros means that the no cars drove by for at least a 3 hour 
period. This is extremely unlikely if the toll was operational.

We can fix this by identifying the runs of 3 or more zeros and changing the
corresponding counts to NA. 


```r
# Before:
sum(is.na(sr1$count))
```

```
## [1] 0
```

```r
# Using custom function
zr1 = zerorun(sr1$count, 3)

sr1$count[zr1] = NA

# After:
sum(is.na(sr1$count))
```

```
## [1] 3969
```

So this eliminates about 4000 points.

Now we'll visually identify where the abnormally large period in July 2010
begins and ends, and then code it as NA.


```r
a = as.POSIXct('2010-06-10')
b = as.POSIXct('2010-08-20')
ab = (a < sr1$time) & (sr1$time < b)
with(sr1[ab, ], plot(time, count, col=alpha('black', 0.5)))

# Lines marking the cutoff dates
startbig = as.POSIXct('2010-06-23')
endbig = as.POSIXct('2010-08-04')
abline(v=c(startbig, endbig), lwd=2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
# Transform to NA's
toobig = (startbig < sr1$time) & (sr1$time < endbig)
sr1$time[toobig] = NA
```

Let's inspect the data to see the effect of the cleaning.


```r
with(sr1, plot(time, count, col=alpha('black', 0.1)))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

