library(igraph)
library(ggplot2)
library(tseries)
library("TTR")
library("forecast")
library(forecast)
visitors=read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab3/visitors.csv", sep = ",", header = TRUE)
visitors

#vts <- ts(visitors, frequency=4, start=c(1998,4))
df <- data.frame(visitors)
new_ts <- within(df, rm(Date))
visitors_timeseries <- ts(new_ts, frequency=4, start=c(1998,4))
visitors_timeseries
#class of time series object
class(visitors_timeseries)

Australia <- within(df, rm(Date,China,Japan,United.Kingdom))
Australia_ts <- ts(Australia, frequency=4, start=c(1998,4))
Australia_ts

China <- within(df, rm(Date,Australia,Japan,United.Kingdom))
China_ts <- ts(China, frequency=4, start=c(1998,4))
China_ts

Japan <- within(df, rm(Date,China,Australia,United.Kingdom))
Japan_ts <- ts(Japan, frequency=4, start=c(1998,4))
Japan_ts

United.Kingdom <- within(df, rm(Date,China,Japan,Australia))
United.Kingdom_ts <- ts(United.Kingdom, frequency=4, start=c(1998,4))
United.Kingdom_ts


#start year of the data
start(visitors_timeseries)

end(visitors_timeseries)
#frequency of data no_of_times in a year
frequency(visitors_timeseries)
#summary od data
summary(visitors_timeseries)

#plotting using time series object
plot.ts(visitors_timeseries)

#Taking log
log_visitors_timeseries <- log(visitors_timeseries)

#plot of log of ts object
plot.ts(log_visitors_timeseries)

#frequency(per year) wise data
visitors_timeseries_freq <- ts(visitors_timeseries, frequency=4, start=c(1998,4))
visitors_timeseries_freq

numrecords <- nrow(visitors)
numcountries <- ncol(visitors) - 1
#visitors[ , 1] <- as.character(visitors[ , 1])

#make all the country columns as numeric
for(i in 2:numcountries + 1) {
  visitors[ , i] <- as.numeric(visitors[ , i])
}

#visitors$Date <- as.Date(visitors$Date, "%y")

#know class of dates
visitors$Date
class(visitors$Date)


#Time series plot for all the 4 countries
for(symbol in 1:numcountries + 1) {
  
  # The ts function of R helps us to
  # construct a time series
  plot(ts(visitors[ , symbol],
          start=c(1998, 4), end=c(2012, 1),
          frequency=4),
       main=paste("quarterly visitors for:", 
                  colnames(visitors)[symbol]),
       xlab="Year", ylab=paste("country:", 
                               colnames(visitors)[symbol]),
       col="navy")
  
}

#c <- colnames(visitors)[2]
#class(c)
#p <- tail(visitors[[c]],1)


tail(visitors$Australia,1)
visitors[2:5,]

#Yearly mean plot
for(symbol in 1:numcountries + 1) {
    x <- c()
    column <- colnames(visitors)[symbol]
    p <- head(visitors[[column]],1)
    x <- c(x,p)
    for(i in seq(from=2, to=53, by=4)){
      j <- i+3
      yearly_visitors <- visitors[i:j,]
      #print (yearly_visitors)
      m=mean(yearly_visitors$Australia)
      #print (m)
      x <- c(x,m)
    }
    column <- colnames(visitors)[symbol]
    q <- tail(visitors[[column]],1)
    x <- c(x,q)
    print (x)
    
    mean_list <- c()
    for(k in 1998:2012+1){
      mean_list <- c(mean_list,k)
    }
    print (mean_list)
    ml <- mean_list
    
    #TS plot for yearly mean of visitors
    plot(ts(x,ml),
         main=paste("yearly mean of visitors for:", 
                    colnames(visitors)[symbol]),
         xlab="Year", ylab=paste("mean:", 
                                 colnames(visitors)[symbol]),
         col="navy")
    
    #Box plot for yearly mean of visitors
    ggplot() + geom_boxplot(mapping = aes(x = ml ,y = x)) + labs(x = "Year", y =  "Mean")
    
}


#Decomposition
Australia_ts.stl = stl(Australia_ts[,1], s.window="periodic")
plot(Australia_ts.stl)
head(Australia_ts)

#HOLTWINTER MODEL

#alpha is the exponential in the moving average model, beta controls how trend is up and gamma controls how the add updation of the seasonal value
Aust_ts <- ts(Australia_ts, frequency=4, start=c(1998,4),end=c(2008,3))
Australia_mean <- HoltWinters(Aust_ts, #alpha = 0.8, beta = 0.1,
                               gamma = FALSE)
#n.ahead below represent the 25% of my data as i have 54 rows
Australia.pred <- predict(Australia_mean,n.ahead=14,prediction.interval = TRUE)
plot.ts(Australia_ts, xlim = c(1998,2020), ylim = c(0,60000))
lines(Australia_mean$fitted[,1],col="green")
lines(Australia.pred[,1], col="blue")
lines(Australia.pred[,2], col="red")
lines(Australia.pred[,3], col="red")

#rms value of actual and predicted data
sqrt(Australia_mean$SSE)


#Aroma model
time(visitors_timeseries)

plot(visitors_timeseries)
abline(reg=lm(visitors_timeseries)~time(visitors_timeseries))
plot(aggregate(visitors_timeseries, FUN = mean))

boxplot(visitors_timeseries~cycle(visitors_timeseries))

plot(log(visitors_timeseries))

plot(diff(log(visitors_timeseries)))


acf(visitors_timeseries)
pacf(visitors_timeseries)
log(visitors_timeseries)
diff(log(visitors_timeseries))





boxplot(Australia_ts~cycle(Australia_ts))
plot(Australia_ts)
#same mean
plot(log(Australia_ts))
#mean become constant with time 
plot(diff(log(Australia_ts)))

Aust_ts <- ts(Australia_ts, frequency=4, start=c(1998,4),end=c(2008,3))

acf(diff(log(Aust_ts))) #q=0
pacf(diff(log(Aust_ts))) #p=-1
plot(diff(log(Aust_ts)))

#c(p,d,q)
fit=arima(log(Aust_ts),c(-1,1,0),seasonal = list(order=c(-1,1,0),period=4))
fit
pred = predict(fit, n.ahead = 1*4)
pred1 = round(2.718^pred$pred,0)

ts.plot(Aust_ts, pred1, log= "y", lty = c(1,3))

#comparision
data1 = round(head(pred1, 4),0)
data2 = round(tail(Aust_ts, 4),0)
plot(data1, col="red", type = "l")
lines(data2, col = "blue")

##rms value of actual and predicted data
sqrt(fit$SSE)




