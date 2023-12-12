setwd("/Users/stephenreagin/Desktop/USD/USD_GitHub/ADS_506_applied_time_series_analysis/final_project/")

library(forecast)
#install.packages('fpp3')
library(fpp3)
library(xts)

transactions_df <- read.csv("daily_total_transactions.csv")
transactions_df$date <- as.Date(transactions_df$date)

num_transactions.ts <- ts(transactions_df$transactions, start=c(2020,295), freq=365)

plot(num_transactions.ts, main='Daily transactions total')

plot(diff(num_transactions.ts,differences = 7))

Acf(num_transactions.ts, lag.max = 60)

#creating time series vectors
total_transactions.xts <- xts(transactions_df$transactions, order.by = transactions_df$date)
isHoliday.xts <- xts(transactions_df$isHoliday, order.by = transactions_df$date)
precip.xts <- xts(transactions_df$PRCP, order.by = transactions_df$date)
tavg.xts <- xts(transactions_df$TAVG, order.by = transactions_df$date)


weekly_transactions.xts <- apply.weekly(total_transactions.xts, sum)
weekly_precip.xts <- apply.weekly(precip.xts, sum)
weekly_tavg.xts <- apply.weekly(tavg.xts, mean)


#weekly transactions with rain and holidays
plot(weekly_transactions.xts, main='Weekly Transactions over Time')
lines(weekly_precip.xts * 1000 + 20000, col='blue', lty=2)
points(isHoliday.xts * 100000-1, pch = 20, col='purple')

#weekly transactions with rain
plot(weekly_transactions.xts, main='Weekly Transactions over Time')
lines(weekly_precip.xts * 1000 + 20000, col='blue', lty=2)

#weekly transactions with temperature
plot(weekly_transactions.xts, main='Weekly Transactions over Time')
lines(weekly_tavg.xts * 5000, col='red', lty=2)


#weekly transactions with holidays
plot.xts(weekly_transactions.xts, main='Weekly Transactions over Time',ylim=c(0,150000))
lines(weekly_precip.xts * 1000 + 20000, col='blue', lty=2, lwd=1)
points(isHoliday.xts * 100000-1, pch = 20, col='purple')
addLegend("topleft",c('Weekly transactions', "Rainfall",'Holiday'), lty=c(1,2,3),
          col=c('black','blue','purple'), lwd=c(1,1,2))


Acf(weekly_transactions.xts, main = 'ACF of weekly transactions')
Acf(weekly_precip.xts, main="ACF of weekly rain")


cor(x=weekly_precip.xts,y=weekly_transactions.xts)


