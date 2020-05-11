library(xts)
library(aTSA)
library(plyr)
library(lmtest)

acf.bartlett <- function( acf.result, n.obs, lags = 20, title='Autocorrelation Function',lower.lim = -0.25){
  
  plot(acf.result$acf[1:lags], 
       type="h", 
       main=title, 
       xlab="Lag",     
       ylab="ACF", 
       ylim=c(lower.lim,1), # this sets the y scale to -1 to 1
       las=1,
       xaxt="n")
  abline(h=0)
  # Add labels to the x-axis
  x <- c(1:lags)
  y <- c(0:(lags-1))
  axis(1, at=x, labels=y)
  
  bartlett.bound(acf.result, n.obs )
  
}


bartlett.bound <- function( z, n ){
  # Create a vector to store Bartlett's standard errors
  
  bart.error <- c() 
  # Use a loop to calculate Bartlett's standard errors
  
  bart.error[1] <- 1 /  n^0.5
  
  for (k in 2:n) {
    ends <- k
    bart.error[k] <- ( (1 + sum( 2*z$acf[ 2:(ends) ] ^ 2) )  ) ^ 0.5 * (n^ -0.5 ) 
  }
  
  
  # Create upper bound of interval (two standard errors above zero)
  upper.bart <- 1.96*bart.error
  # Create lower bound of interval (two standard errors below zero)
  lower.bart <- 1.96*-bart.error
  # Add intervals based on Bartlett's approximations to ACF plot
  lines((2:(length(upper.bart)+1)),upper.bart, lty=2, col="blue"); 
  lines((2:(length(upper.bart)+1)),lower.bart, lty=2, col="blue")
  
}


## Load daily tweet data 
df <- read.csv('trump_weekly_tweets.csv')
df <- df[2:(nrow(df)-1),]

total.length = nrow(df)
## Data for Forecast
forcast.values = df[ (nrow(df)-4) : nrow(df), ]

## ----------------------------------------
## Original Data Series Analysis
## ----------------------------------------
trump_weekly_series = matrix( df[1:(total.length-5),2], nrow = length(df[1:(total.length-5),2]))
rownames(trump_weekly_series) <- df[1:(total.length-5),1]


plot(trump_weekly_series,type='l')

weekly_ts <- xts(trump_weekly_series, order.by=as.Date(row.names(trump_weekly_series)))

plot(weekly_ts,main=NA)
hist(weekly_ts, main=NA,xlab = NA)

adf.test(weekly_ts,nlag = 10)


## ----------------------------------------
## Log weekly tweets 
## ----------------------------------------

log_weekly_tweets = log( trump_weekly_series )

log_weekly_ts <- xts(log_weekly_tweets, order.by=as.Date(row.names(log_weekly_tweets)))
plot(log_weekly_ts,type='l', main=NA)

hist(log_weekly_tweets,breaks = 9,main=NA)


qqnorm(log_weekly_tweets)
qqline(log_weekly_tweets)


a <- acf( log_weekly_tweets,lag.max = 20 )
acf.bartlett(a, length(log_weekly_tweets),lags = 20, title = NA)

p <- pacf( log_weekly_tweets,lag.max = 20 )

plot(p, main = NA)

adf.test( log_weekly_tweets, nlag =5 )

## Null not rejected at lag 5. Hence, it might have a unit root. 


## ----------------------------------------
## Differencing Log weekly tweets 
## ----------------------------------------
lg.tw.d1 = diff(log_weekly_tweets)

ts <- xts(lg.tw.d1, order.by=as.Date(row.names(lg.tw.d1)))

plot(ts,main=NA)

plot(lg.tw.d1,type='l')
adf.test(lg.tw.d1,nlag=7)

a.d1 <- acf(ts,lag.max = 30)

acf.bartlett(a.d1, n.obs = length(ts), lags=15, lower.lim=-0.5,title = NA)
p.d1 <- pacf(ts,lag.max = 30)

plot(p.d1, main=NA)
## ----------------------------------------
### Log, ARIMA (0,1,1)
## ----------------------------------------

model <- arima( log_weekly_tweets, order=c(0,1,1),method = 'ML')
model
coeftest(model)

stargazer(model)

mododel.residual <- model$residuals
plot( mododel.residual, type='l' )
z <- acf(mododel.residual,lag.max = 20)

acf.bartlett( z, length(mododel.residual), title=NA)

qqnorm(mododel.residual, main=NA)
qqline(mododel.residual)
shapiro.test(mododel.residual)

## ----------------------------------------
### Log, ARIMA (0,1,2)
## ----------------------------------------
model.2 <- arima( log_weekly_tweets, order=c(0,1,2),method = 'ML')
model.2

model.3 <- arima( log_weekly_tweets, order=c(0,1,3),method = 'ML')
model.3

stargazer(model, model.2, model.3)

mododel.residual <- model$residuals
plot( mododel.residual, type='l' )
z <- acf(mododel.residual,lag.max = 20)

acf.bartlett( z, length(mododel.residual), title='ACF Plot for Residuals')

qqnorm(mododel.residual)
qqline(mododel.residual)

for (i in (3:10)){
  print( Box.test(mododel.residual, type = "Ljung-Box", lag = i, fitdf = 2)  )
  
  ## 16 
}


model.2.residual <- model.2$residuals
plot( model.2.residual, type='l' )
z <- acf(model.2.residual,lag.max = 20)

acf.bartlett( z, length(model.2.residual), title=NA)

qqnorm(model.2.residual)
qqline(model.2.residual)
shapiro.test(model.2.residual)

## ----------------------------------------
### Log, ARIMA (4,1,0)
## ----------------------------------------
model <- arima( log_weekly_tweets, order=c(4,1,0),method = 'ML', include.drift = True)
model
coeftest(model)

mean(diff(log_weekly_tweets))


mododel.residual <- model$residuals
for (i in (5:20)){
  print( Box.test(mododel.residual, type = "Ljung-Box", lag = i, fitdf = 4)  )

}

plot( mododel.residual, type='l' )
z <- acf(mododel.residual,lag.max = 20)

acf.bartlett( z, length(mododel.residual), title=NA)

model.5 <- arima( log_weekly_tweets, order=c(5,1,0),method = 'ML')
model.5
coeftest(model.5)

stargazer(model, model.5)

qqnorm(mododel.residual,main = NA)
qqline(mododel.residual)

## ----------------------------------------
## prediction
## ----------------------------------------

# log_weekly_ts <- xts(log_weekly_tweets, order.by=as.POSIXct( as.Date(row.names(log_weekly_tweets))))
model.of.choice <- arima( log_weekly_tweets, order=c(0,1,2),method = 'ML')
model.of.choice
predictions <- predict(model.of.choice, n.ahead = 5)

exp(predictions$pred + 0.5*predictions$se *predictions$se   )
forcast.values[,2]

predictions$pred
log(forcast.values[,2])

f <- forecast(model.of.choice, lead = 5)
stargazer(f)




