setwd("/Users/stephenreagin/Desktop/USD/USD_GitHub/ADS_506_applied_time_series_analysis/final_project/")

library(forecast)
#install.packages('GGally')
library(fpp3)
library(tidyverse)
library(dplyr)

transactions_df <- read.csv("daily_total_transactions.csv")
transactions_df$date <- as.Date(transactions_df$date)

transactions_tsibble <- as_tsibble(transactions_df, index=date)

#training and validation data splits
train_transactions_tsibble <- transactions_tsibble |> 
  slice(0:(length(transactions_df$transactions)-90))
valid_transactions_tsibble <- transactions_tsibble |> slice(n()-89:0)

plot(x=transactions_tsibble$date, y=transactions_tsibble$transactions,
     xlab="Date",ylab="Daily Transactions", main='Daily Transactions over Time')

transactions_tsibble %>% model(STL(transactions ~ trend(window=60) + 
                                     season(period=7))) %>% components() |>  autoplot()


######## remove holidays and Sundays
nonspecial_df <- subset(transactions_tsibble, (isSunday == 0) & (isHoliday == 0))
special_df <- subset(transactions_tsibble, (isSunday == 1) | (isHoliday == 1))

nonspecial_df$X <- 1:length(nonspecial_df$X)
nonspecial_df <- as_tsibble(nonspecial_df, index=date) 
#7.4 needs index=date
#8.1 needs index=X

train_nonspecial_df <- nonspecial_df |> slice(0:(length(nonspecial_df$transactions)-90))
valid_nonspecial_df <- nonspecial_df |> slice(n()-89:0)

plot(x=nonspecial_df$date, y=nonspecial_df$transactions)

nonspecial_df %>% model(STL(transactions ~ trend(window=90) + 
  season(period=6))) %>% components() |>  autoplot()



###############################
#### SECTION 1: All daily data

#### Section 1.1 : Not decomposed

#### Section 1.1a : Linear Regression Model

#fit the model
fit_lm <- train_transactions_tsibble |> model(lm = TSLM(transactions ~ TAVG + 
                PRCP + TMIN + isSunday + isHoliday + trend() + season()))
#forecast using validation data
fc_lm <- forecast(fit_lm, new_data = valid_transactions_tsibble)

transactions_tsibble |> autoplot(transactions) + autolayer(fc_lm)
train_transactions_tsibble |> autoplot(transactions) + autolayer(fc_lm)

sum(fc_lm$.mean) / sum(valid_transactions_tsibble$transactions)

#### SECTION 1: All daily data
#### Section 1.1 : Not decomposed
#### Section 1.1b : ARIMA model AR(1)

fit <- train_transactions_tsibble |> model(ARIMA(transactions ~ pdq(1,0,0)))
report(fit)

fc <- fit |> forecast(h=90) 
fc |> autoplot(train_transactions_tsibble, level=10)

sum(fc$.mean) / sum(valid_transactions_tsibble$transactions)

#### SECTION 1: All daily data
#### Section 1.2 - STL decomposition

stl <- train_transactions_tsibble |> model(STL(transactions ~ trend(window=30) + 
                                                 season(window='periodic'), robust=TRUE)) 

stl_cmp <- stl |>  components()
stl_cmp |> autoplot()
stl_cmp

# trend + remainder = season_adjust 
#                   = stl_cmp$transactions - stl_cmp$season_week - stl_cmp$season_year
# season_week + season_year + trend + remainder = transactions

train_transactions_tsibble$lm_trend <- stl_cmp$trend

stl_cmp$season_week[1:7]
#8/29 - 11/26.   576 668
stl_cmp$season_year[576:668][1:90]

weekly_season <- c(2621.935,   2797.813,   3137.400,   3837.607,   1906.287,-15699.924,   1398.882)
add_weekly_season <- rep(weekly_season, 13)[1:90]
add_yearly_season <- stl_cmp$season_year[576:668][1:90]



#### SECTION 1: All daily data
#### Section 1.2 - STL decomposition
#### Section 1.2a : Linear Model

fit_lm <- train_transactions_tsibble |> model(lm = TSLM(lm_trend ~ TAVG + 
                                        PRCP + TMIN + isSunday + isHoliday + trend() + season()))
#forecast using validation data
fc_lm <- forecast(fit_lm, new_data = valid_transactions_tsibble)
fc_lm$.mean <- fc_lm$.mean + add_weekly_season + add_yearly_season

train_transactions_tsibble |> autoplot(lm_trend) + autolayer(fc_lm)

sum(fc_lm$.mean) / sum(valid_transactions_tsibble$transactions)

#### SECTION 1: All daily data
#### Section 1.2 - STL decomposition
#### Section 1.2b : AR(1) Model

fit <- train_transactions_tsibble |> model(ARIMA(lm_trend))
report(fit)

fc <- fit |> forecast(h=90) 
fc |> autoplot(train_transactions_tsibble, level=10)

sum(fc$.mean) / sum(valid_transactions_tsibble$transactions)



###############################
#### SECTION 1: Nonspecial days

#### Section 1.1 : Not decomposed

#### Section 1.1a : Linear Regression Model

fit_lm <- nonspecial_df |> model(lm = TSLM(transactions ~ TAVG + PRCP + TMIN + TMAX + 
                                             trend() + season()))
fc_lm <- forecast(fit_lm, new_data = valid_nonspecial_df)

nonspecial_df |> autoplot(transactions, color='darkgray')  + 
  autolayer(fc_lm, level=0) +
  labs(title='Transactions volume 2021 - 2023, with 90-day prediction (linear regression)', 
       x='Date', ylab='Transactions') 

sum(fc_lm$.mean) / sum(valid_nonspecial_df$transactions)

sum(fc_lm$.mean)
sum(valid_nonspecial_df$transactions)

#### SECTION 1: All daily data
#### Section 1.1 : Not decomposed
#### Section 1.1b : ARIMA model AR(1)

fit <- train_nonspecial_df |> model(ARIMA(transactions ~ pdq(1,0,0)))
report(fit)

fc <- fit |> forecast(h=90) 
fc |> autoplot(train_transactions_tsibble, level=10)

sum(fc$.mean) / sum(valid_transactions_tsibble$transactions)

#### SECTION 1: All daily data
#### Section 1.2 - STL decomposition

stl <- train_transactions_tsibble |> model(STL(transactions ~ trend(window=30) + 
                                                 season(window='periodic'), robust=TRUE)) 

stl_cmp <- stl |>  components()
stl_cmp |> autoplot()
stl_cmp

# trend + remainder = season_adjust 
#                   = stl_cmp$transactions - stl_cmp$season_week - stl_cmp$season_year
# season_week + season_year + trend + remainder = transactions

train_transactions_tsibble$lm_trend <- stl_cmp$trend

stl_cmp$season_week[1:7]
#8/29 - 11/26.   576 668
stl_cmp$season_year[576:668][1:90]

weekly_season <- c(2621.935,   2797.813,   3137.400,   3837.607,   1906.287,-15699.924,   1398.882)
add_weekly_season <- rep(weekly_season, 13)[1:90]
add_yearly_season <- stl_cmp$season_year[576:668][1:90]



#### SECTION 1: All daily data
#### Section 1.2 - STL decomposition
#### Section 1.2a : Linear Model

fit_lm <- train_transactions_tsibble |> model(lm = TSLM(lm_trend ~ TAVG + 
                                                          PRCP + TMIN + isSunday + isHoliday + trend() + season()))
#forecast using validation data
fc_lm <- forecast(fit_lm, new_data = valid_transactions_tsibble)
fc_lm$.mean <- fc_lm$.mean + add_weekly_season + add_yearly_season

train_transactions_tsibble |> autoplot(lm_trend) + autolayer(fc_lm)

sum(fc_lm$.mean) / sum(valid_transactions_tsibble$transactions)

#### SECTION 1: All daily data
#### Section 1.2 - STL decomposition
#### Section 1.2b : AR(1) Model

fit <- train_transactions_tsibble |> model(ARIMA(lm_trend))
report(fit)

fc <- fit |> forecast(h=90) 
fc |> autoplot(train_transactions_tsibble, level=10)

sum(fc$.mean) / sum(valid_transactions_tsibble$transactions)

