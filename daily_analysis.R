library(xts)
library(tseries)
library(plyr)


## Load daily tweet data 
df <- read.csv('trump_daily_tweets.csv')

df <- df[2:nrow(df),]
ts <- xts(df[,2], order.by=as.Date(df[,1]))

trump_daily_series = df[,2]


plot(ts,type='l')

hist(ts)

adf.test(df[,2],k=7)


## Log weekly tweets 
log_daily_tweets = log( 1+trump_daily_series )

plot(log_daily_tweets,type='l')
adf.test(log_daily_tweets,nlag=7)


acf( log_daily_tweets, lag.max = 100)
pacf( log_daily_tweets, lag.max = 100)


## Differencing Log weekly tweets 
log_daily_diff = diff(trump_daily_series)

plot(log_daily_diff,type='l')


acf( log_daily_diff, lag.max = 100)
pacf( log_daily_diff, lag.max = 100)

seasonal = list(order = c(0,0,1), period = 30)
fitARIMA <- arima(log_daily_diff, order=c(0,0,1), method="ML")
fitARIMA

acf(fitARIMA$residuals,lag.max = 10)
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)


Box.test(fitARIMA$residuals, type = "Ljung-Box", lag = 5, fitdf = 1)




