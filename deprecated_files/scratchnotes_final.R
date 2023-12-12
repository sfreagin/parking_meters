setwd("/Users/stephenreagin/Desktop/USD/USD_GitHub/ADS_506_applied_time_series_analysis/final_project/")

library(forecast)
#install.packages('GGally')
library(fpp3)
library(tidyverse)
library(dplyr)

transactions_df <- read.csv("daily_total_transactions.csv")
transactions_df$date <- as.Date(transactions_df$date)

transactions_tsibble <- as_tsibble(transactions_df, index=date)

transactions_tsibble %>% group_by_key()


######## remove Sundays
nonSunday_df <- subset(transactions_tsibble, isSunday == 0)
Sunday_df <- subset(transactions_tsibble, isSunday == 1)

nonSunday_df$X <- 1:length(nonSunday_df$X)
nonSunday_df <- as_tsibble(nonSunday_df,  index=X) 
#7.4, needs index=date
#8.1 needs index=X

train_nonSunday_df <- nonSunday_df |> slice(0:(length(nonSunday_df$transactions)-90))
valid_nonSunday_df <- nonSunday_df |> slice(n()-89:0)

plot(x=nonSunday_df$date, y=nonSunday_df$transactions)

as_tsibble(nonSunday_df, index = X) %>%
  model(STL(transactions ~ trend(window=26) + season(period=6))) %>%
  components() |>
  autoplot()



######## remove holidays and Sundays
nonspecial_df <- subset(transactions_tsibble, (isSunday == 0) & (isHoliday == 0))
special_df <- subset(transactions_tsibble, (isSunday == 1) | (isHoliday == 1))

nonspecial_df$X <- 1:length(nonspecial_df$X)
nonspecial_df <- as_tsibble(nonspecial_df, index=X) 
#7.4 needs index=date
#8.1 needs index=X

train_nonspecial_df <- nonspecial_df |> slice(0:(length(nonspecial_df$transactions)-90))
valid_nonspecial_df <- nonspecial_df |> slice(n()-89:0)
tail(valid_nonspecial_df)
plot(x=nonspecial_df$date, y=nonspecial_df$transactions)

nonspecial_STL <- nonspecial_df %>%
  model(STL(transactions ~ trend(window=25) + season(period=6))) %>%
  components() |>
  autoplot()

nonspecial_STL 



######## full DF
transactions_tsibble |> features(transactions, list(mean = mean)) |> arrange(mean)
transactions_tsibble |> features(transactions, quantile)
transactions_tsibble |> features(transactions, feat_acf)
transactions_tsibble |> features(transactions, feat_stl)

transactions_tsibble |> features(transactions, feat_stl) |>
  ggplot(aes(x = trend_strength, y=seasonal_strength_week)) + geom_point()

#xyz <- transactions_tsibble |> features(transactions, feat_stl)
#xyz |> filter(seasonal_strength_week == max(seasonal_strength_week)) |>
#  left_join(transactions_tsibble, by)
#tourism |> features(Trips, feat_stl)

# 5.1
transactions_tsibble |> filter(isSunday==0) |> autoplot(transactions)
fit <- transactions_tsibble |> model(trend_model = TSLM(transactions ~ trend()))
fit |> forecast(h=90)
fit |> forecast(h=90) |> filter(isSunday == 0) |> autoplot(transactions_tsibble)

#5.2
transactions_tsibble |> filter(isSunday == 0) |> model(MEAN(transactions)) |>
  forecast(h=90) |> autoplot(transactions_tsibble)

transactions_tsibble |> model(NAIVE(transactions)) |> forecast(h=90) |> 
  autoplot(transactions_tsibble)

transactions_tsibble |> model(SNAIVE(transactions)) |> forecast(h=90) |> 
  autoplot(transactions_tsibble)

transactions_tsibble |> model(RW(transactions ~ drift())) |> forecast(h=90) |> 
  autoplot(transactions_tsibble)

#5.3
augment(fit)

#5.4
autoplot(transactions_tsibble, transactions)

preds <- transactions_tsibble |> model(NAIVE(transactions)) |> augment() 
preds |> autoplot(.resid)
preds |> ggplot(aes(x = .resid)) + geom_histogram()
preds |> ACF(.resid) |> autoplot()

transactions_tsibble |> model(NAIVE(transactions)) |> gg_tsresiduals()

preds |> features(.resid, box_pierce, lag=14)
preds |> features(.resid, ljung_box, lag=14)

fit <- transactions_tsibble |> model(RW(transactions ~ drift()))
tidy(fit)

augment(fit) |> features(.resid, ljung_box, lag=14)

#5.5
transactions_tsibble |> model(NAIVE(transactions)) |> forecast(h=90) |> hilo()
transactions_tsibble |> model(NAIVE(transactions)) |> forecast(h=90) |> autoplot(transactions_tsibble)

fit <- transactions_tsibble |> model(NAIVE(transactions))
sim <- fit |> generate(h=90, time=5, bootstrap = TRUE)
sim

transactions_tsibble |> ggplot(aes(x = date)) + geom_line(aes(y=transactions)) +
  geom_line(aes(y = .sim, color = as.factor(.rep)), data=sim)

fc <- fit |> forecast(h=30, bootstrap = TRUE)
autoplot(fc, transactions_tsibble)

transactions_tsibble |> model(NAIVE(transactions)) |> 
  forecast(h=90, bootstrap=TRUE, times = 1000) |> hilo()

#5.7
dcmp <- transactions_tsibble |> model(STL(transactions ~ trend(window=30), robust=TRUE)) |>
  components() |> select(-.model)
dcmp |> model(NAIVE(season_adjust)) |> forecast(h=90) |> autoplot(dcmp, level=50)

fit_dcmp <- transactions_tsibble |> model(stlf = decomposition_model(
  STL(transactions ~ trend(window=7), robust=TRUE), NAIVE(season_adjust)))
fit_dcmp |> forecast(h=90) |> autoplot(transactions_tsibble,level=10)
fit_dcmp |> gg_tsresiduals()

#5.8
transactions_tsibble |> slice(n()-89:0)

train_transactions_tsibble <- transactions_tsibble |> 
  slice(0:(length(transactions_df$transactions)-90))

train_fit <- train_transactions_tsibble |> model( 
  'Mean' = MEAN(transactions),
  'Naive' = NAIVE(transactions),
  'Seasonal Naive' = SNAIVE(transactions,),
  'Drift' =  RW(transactions ~ drift()))

train_fc <- train_fit |> forecast(h=90)
train_fc |> autoplot(transactions_tsibble, level=10)
train_fc |> autoplot(train_transactions_tsibble, level=10)

accuracy(train_fc, transactions_tsibble)

#5.10
cv_df <- transactions_tsibble |> stretch_tsibble(.init=800, .step=7) |> relocate(date, .id)

cv_df |> model(RW(transactions ~ drift())) |> forecast(h=30) |> accuracy(transactions_tsibble)
transactions_tsibble |> model(RW(transactions ~ drift())) |> accuracy()

#7.1
transactions_tsibble |> ggplot(aes(x=TAVG, y=transactions)) + geom_point() + 
  geom_smooth(method = 'lm', se=FALSE) + labs(title="Daily Transactions as a function of Temperature (C)")

transactions_tsibble |> ggplot(aes(x=PRCP, y=transactions)) + geom_point() + 
  geom_smooth(method = 'lm', se=FALSE)

transactions_tsibble |> model(TSLM(transactions ~ TAVG)) |> report()
transactions_tsibble |> model(TSLM(transactions ~ PRCP)) |> report()

transactions_tsibble |> GGally::ggpairs(columns=3:7)

#7.2
fit_mr <- transactions_tsibble |> model(tslm = TSLM(transactions ~ PRCP + TAVG + TMIN + isSunday + isHoliday))
report(fit_mr)

augment(fit_mr) |> ggplot(aes(x=date)) + geom_line(aes(y=transactions, color='Transactions')) +
  geom_line(aes(y=.fitted, color='Fitted')) #+  scale_color_manual(values=c(Transactions='black'))

augment(fit_mr) |> ggplot(aes( x=transactions, y=.fitted)) + geom_point() +
  geom_abline(intercept=0, slope=1)

#7.3
fit_mr |> gg_tsresiduals()
augment(fit_mr) |> features(.resid, ljung_box, lag=14)

transactions_tsibble |> left_join(residuals(fit_mr), by='date') |>
  pivot_longer(PRCP:transactions, names_to = 'regressor', values_to = 'x') |>
  ggplot(aes(x=x, y=.resid)) + geom_point() + facet_wrap(. ~ regressor, scales='free_x')

augment(fit_mr) |> ggplot(aes(x=.fitted, y=.resid)) + geom_point()

#7.4
fit <- transactions_tsibble |> model(TSLM(transactions ~ trend() + season()))
report(fit)

augment(fit) |> ggplot(aes(x=date)) + geom_line(aes(y=transactions, color='Data')) +
  geom_line(aes(y=.fitted, color = 'Fitted'))

augment(fit) |> ggplot(aes(x=transactions, y=.fitted)) + geom_point() +
  geom_abline(intercept=0, slope=1)

fourier_fit <- transactions_tsibble |> model(TSLM(transactions ~ trend() + fourier(K=3)))
report(fourier_fit)

augment(fourier_fit) |> ggplot(aes(x=date)) + geom_line(aes(y=transactions, color='Data')) +
  geom_line(aes(y=.fitted, color = 'Fitted'))

#7.5
glance(fit)
glance(fit) |> select(adj_r_squared, CV, AIC, AICc, BIC)

#7.6
train_transactions_tsibble <- transactions_tsibble |> 
  slice(0:(length(transactions_df$transactions)-90))
valid_transactions_tsibble <- transactions_tsibble |> slice(n()-89:0)

fit <- train_transactions_tsibble |> model(TSLM(transactions ~ trend() + season()))
fc <- fit |> forecast(h=90)
fc |> autoplot(train_transactions_tsibble)
accuracy(fc, transactions_tsibble)

fit_lm <- train_transactions_tsibble |> model(lm = TSLM(transactions ~ TAVG + 
          PRCP + TMIN + TMAX + isSunday + isHoliday + trend() + season()))
fc_lm <- forecast(fit_lm, new_data = valid_transactions_tsibble)

transactions_tsibble |> autoplot(transactions) + autolayer(fc)
train_transactions_tsibble |> autoplot(transactions) + autolayer(fc)

sum(fc_lm$.mean) / sum(valid_transactions_tsibble$transactions)

#8.1
fit <- transactions_tsibble |> 
  model(ETS(transactions ~ error("A") + trend("N") + season("N")))

fc <- fit |> forecast(h=90)
fc |> autoplot(transactions_tsibble) + 
  geom_line(aes(y=.fitted), color='blue',data = augment(fit))

#8.2
transactions_tsibble |> model(
  "Holt's method" = ETS(transactions ~ error("A") + trend("A") + season("N")),
  "Damped Holt's method" = ETS(transactions ~ error("A") +
                                 trend("Ad", phi = 0.9) + season("N"))) |>
  forecast(h=90) |> autoplot(transactions_tsibble, level=NULL)

#8.3
fit <- train_transactions_tsibble |> model(
  additive = ETS(transactions ~ error("A") + trend("A") + season("A")),)
#  multiplicative = ETS(transactions ~ error("M") + trend("A") + season("M")))

fc <- fit |> forecast(h=90)
fc |> autoplot(train_transactions_tsibble, level=0)

#9.1
transactions_tsibble |> ACF(transactions) |> autoplot()
transactions_tsibble |> ACF(difference(transactions)) |> autoplot()

#9.5
fit <- train_transactions_tsibble |> model(ARIMA(transactions))
report(fit)

fc <- fit |> forecast(h=90) 
fc |> autoplot(train_transactions_tsibble)

sum(fc$.mean) / sum(valid_transactions_tsibble$transactions)

#9.9
transactions_tsibble |> gg_tsdisplay(difference(transactions, 7),
                                     plot_type = 'partial', lag=60)

transactions_tsibble |> gg_tsdisplay(difference(transactions, 7) |> 
                                       difference(), plot_type = 'partial', lag=60)

fit <- train_transactions_tsibble |> model(
  arima012011 = ARIMA(transactions ~ pdq(0,1,2) + PDQ(0,1,1)),
  arima210011 = ARIMA(transactions ~ pdq(2,1,0) + PDQ(0,1,1)),
  auto = ARIMA(transactions, stepwise = FALSE, approx = FALSE))

fit |> pivot_longer(everything(), names_to = "Model Name", values_to = 'Order')
glance(fit) |> arrange(AICc) |> select(.model:BIC)

fit |> forecast(h=90) |> autoplot(train_transactions_tsibble, level=0)

#3.6
stl <- train_transactions_tsibble |> model(STL(transactions ~ trend(window=30) + 
                                  season(window='periodic'), robust=TRUE)) 

stl_cmp <- stl |>  components()
stl_cmp |> autoplot()
stl_cmp


######## STOP ########
######## STOP ########
######## STOP ########
######## STOP ########

######## no Sundays DF
nonSunday_df |> features(transactions, list(mean = mean)) |> arrange(mean)
nonSunday_df |> features(transactions, quantile)
nonSunday_df |> features(transactions, feat_acf)
nonSunday_df |> features(transactions, feat_stl)

#5.1
nonSunday_df |> filter(isHoliday == 0) |> autoplot(transactions)
fit <- nonSunday_df |> model(trend_model = TSLM(transactions ~ trend()))
fit |> forecast(h = 90)
fit |> forecast(h=90) |> autoplot(nonSunday_df)

#5.2
nonSunday_df |> model(MEAN(transactions)) |> forecast(h=90) |>  autoplot(nonSunday_df)
nonSunday_df |> model(NAIVE(transactions)) |> forecast(h=90) |>  autoplot(nonSunday_df)
nonSunday_df |> model(SNAIVE(transactions ~ lag(6))) |> forecast(h=90) |>  autoplot(nonSunday_df)
nonSunday_df |> model(RW(transactions ~ drift())) |> forecast(h=90) |>  autoplot(nonSunday_df)

#5.3
augment(fit)

#5.4
autoplot(nonSunday_df, transactions) #+ labs(y='Num transactions', x='time')

preds <- nonSunday_df |> model(NAIVE(transactions)) |> augment()
autoplot(preds, .resid)
preds |> ggplot(aes(x = .resid)) + geom_histogram()
preds |> ACF(.resid) |> autoplot()

nonSunday_df |> model(NAIVE(transactions)) |> gg_tsresiduals()

preds |> features(.resid, box_pierce, lag = 12)
preds |> features(.resid, ljung_box, lag = 12)

fit <- nonSunday_df |> model(RW(transactions ~ drift()))
tidy(fit)

#5.5
nonSunday_df |> model(NAIVE(transactions)) |> forecast(h = 90) |> hilo()
nonSunday_df |> model(NAIVE(transactions)) |> forecast(h = 90) |>
  autoplot(nonSunday_df)

fit <- nonSunday_df |> model(NAIVE(transactions))
sim <- fit |> generate(h=90, times=5, bootstrap=TRUE)

nonSunday_df |> ggplot(aes(x= X)) + geom_line(aes(y= transactions)) +
  geom_line(aes(y=.sim, color=as.factor(.rep)), data=sim)

fc <- fit |> forecast(h=90, bootstrap = TRUE)
autoplot(fc, nonSunday_df)

nonSunday_df |> model(NAIVE(transactions)) |> forecast(h=90, bootstrap=TRUE, times = 1000) |>
  hilo()

#5.7
dcmp <- nonSunday_df |> model(STL(transactions ~ trend(window=30), robust=TRUE)) |> 
  components() |> select(-.model)
dcmp |> model(NAIVE(season_adjust)) |> forecast(h=90) |> autoplot(dcmp, level=25)

fit_dcmp <- nonSunday_df |> model(stlf = decomposition_model(
  STL(transactions ~ trend(window=30), robust=TRUE), NAIVE(season_adjust)))
fit_dcmp |> forecast(h=90) |> autoplot(nonSunday_df)
fit_dcmp |> gg_tsresiduals()

#5.8
nonSunday_df |> slice(n()-89:0)

train_nonSunday_df <- nonSunday_df |> 
  slice(0:(length(nonSunday_df$transactions)-90))

train_fit <- train_nonSunday_df |> model(
  'Mean' = MEAN(transactions),
  'Naive' = NAIVE(transactions),
  'Seasonal naive' = NAIVE(transactions),
  'Drift' = RW(transactions ~ drift())
)

train_fc <- train_fit |> forecast(h=90)

train_fc |> autoplot(nonSunday_df, level=20)
train_fc |> autoplot(train_nonSunday_df, level=20) +
  guides(color=guide_legend(title='Forecast'))

accuracy(train_fc, nonSunday_df)

#5.10
cv_df <- nonSunday_df |> stretch_tsibble(.init=800, .step=6) |> relocate(date, .id)
cv_df |> model(RW(transactions ~ drift())) |> forecast(h=30) |> accuracy(nonSunday_df)

nonSunday_df |> model(RW(transactions ~ drift())) |> accuracy()

#7.1
nonSunday_df |> ggplot(aes(x=TAVG, y=transactions)) + geom_point() + 
  geom_smooth(method = 'lm', se=FALSE)

nonSunday_df |> ggplot(aes(x=PRCP, y=transactions)) + geom_point() + 
  geom_smooth(method = 'lm', se=FALSE)

nonSunday_df |> model(TSLM(transactions ~ TAVG)) |> report()
nonSunday_df |> GGally::ggpairs(columns=3:7)

#7.2
fit_mr <- nonSunday_df |> model(tslm = TSLM(transactions ~ PRCP + TAVG + TMIN))
report(fit_mr)

augment(fit_mr) |> ggplot(aes(x=X)) + geom_line(aes(y=transactions, color='Transactions')) +
  geom_line(aes(y=.fitted, color='Fitted'))

augment(fit_mr) |> ggplot(aes(x=transactions, y=.fitted)) + geom_point() +
  geom_abline(intercept=0, slope=1)

#7.3
fit_mr |> gg_tsresiduals()
augment(fit_mr) |> features(.resid, ljung_box, lag=12)

nonSunday_df |> left_join(residuals(fit_mr), by='X') |> 
  pivot_longer(PRCP:transactions, names_to = 'regressor', values_to = 'x') |>
  ggplot(aes(x=x, y=.resid)) + geom_point() + facet_wrap(. ~ regressor, scales='free_x')

augment(fit_mr) |> ggplot(aes(x=.fitted, y=.resid)) + geom_point()

#7.4
fit <- nonSunday_df |> model(TSLM(transactions ~ trend() + season()))
report(fit)

augment(fit) |> ggplot(aes(x=date)) + geom_line(aes(y=transactions, color='Data')) +
  geom_line(aes(y=.fitted, color = 'Fitted'))

augment(fit) |> ggplot(aes(x=transactions, y=.fitted)) + geom_point() +
  geom_abline(intercept=0, slope=1)

fourier_fit <- nonSunday_df |> model(TSLM(transactions ~ trend() + fourier(K=2)))
report(fourier_fit)

augment(fourier_fit) |> ggplot(aes(x=date)) + geom_line(aes(y=transactions, color='Data')) +
  geom_line(aes(y=.fitted, color = 'Fitted'))

#7.5
glance(fit)
glance(fit) |> select(adj_r_squared, CV, AIC, AICc, BIC)

#7.6
train_nonSunday_df <- nonSunday_df |> slice(0:(length(nonSunday_df$transactions)-90))
valid_nonSunday_df <- nonSunday_df |> slice(n()-89:0)

fit <- train_nonSunday_df |> model(TSLM(transactions ~ trend()  + season()))
fc <- fit |> forecast(h=90)
fc |> autoplot(train_nonSunday_df)
accuracy(fc, nonSunday_df)

fit_lm <- train_nonSunday_df |> model(lm = TSLM(transactions ~ TAVG + PRCP + TMIN + TMAX + 
                                                  trend() + season()))
fc_lm <- forecast(fit_lm, valid_nonSunday_df)

train_nonSunday_df |> autoplot(transactions) + autolayer(fc_lm, level=0)
accuracy(fc_lm, valid_nonSunday_df)

sum(fc_lm$.mean) / sum(valid_nonSunday_df$transactions)

#8.1
fit <- nonSunday_df |> 
  model(ETS(transactions ~ error("A") + trend("N") + season("N")))

fc <- fit |> forecast(h=90)
fc |> autoplot(nonSunday_df) + 
  geom_line(aes(y=.fitted), color='blue',data = augment(fit))

#8.2
nonSunday_df |> model(
  "Holt's method" = ETS(transactions ~ error("A") + trend("A") + season("N")),
  "Damped Holt's method" = ETS(transactions ~ error("A") +
                                 trend("Ad", phi = 0.9) + season("N"))) |>
  forecast(h=90) |> autoplot(nonSunday_df, level=NULL)

#8.3
fit <- train_nonSunday_df |> model(
  additive = ETS(transactions ~ error("A") + trend("A") + season("A")),)
#  multiplicative = ETS(transactions ~ error("M") + trend("A") + season("M")))

fc <- fit |> forecast(h=90)
fc |> autoplot(train_nonSunday_df, level=0)

#9.1
nonSunday_df |> ACF(transactions) |> autoplot()
nonSunday_df |> ACF(difference(transactions)) |> autoplot()

#9.5
fit <- train_nonSunday_df |> model(ARIMA(transactions))
report(fit)

fc <- fit |> forecast(h=90) 
fc |> autoplot(train_nonSunday_df, level=10)

sum(fc$.mean) / sum(valid_nonSunday_df$transactions)

fit2 <- train_nonSunday_df |> model(ARIMA(transactions ~ pdq(1,1,1)))
report(fit2)

fc2 <- fit2 |> forecast(h=90) 
fc2 |> autoplot(train_nonSunday_df, level=10)

sum(fc2$.mean) / sum(valid_nonSunday_df$transactions)

#9.9
nonSunday_df |> gg_tsdisplay(difference(transactions, 6),
                                     plot_type = 'partial', lag=25)

nonSunday_df |> gg_tsdisplay(difference(transactions, 6) |> 
                                       difference(), plot_type = 'partial', lag=25)

fit <- train_nonSunday_df |> model(
  arima012011 = ARIMA(transactions ~ pdq(0,1,2) + PDQ(0,1,1)),
  arima210011 = ARIMA(transactions ~ pdq(2,1,0) + PDQ(0,1,1)),
  auto = ARIMA(transactions, stepwise = FALSE, approx = FALSE))

fit |> pivot_longer(everything(), names_to = "Model Name", values_to = 'Order')
glance(fit) |> arrange(AICc) |> select(.model:BIC)

fit |> forecast(h=90) |> autoplot(train_nonSunday_df, level=10)

#3.6
stl <- train_nonSunday_df |> model(STL(transactions ~ trend(window=365) + season(window='periodic'),
                                        robust=TRUE)) 

stl_cmp <- stl |>  components()
stl_cmp |> autoplot()





######## STOP ########
######## STOP ########
######## STOP ########
######## STOP ########

######## no Sundays or holidays DF

nonspecial_df |> features(transactions, list(mean = mean)) |> arrange(mean)
nonspecial_df |> features(transactions, quantile)
nonspecial_df |> features(transactions, feat_acf)
nonspecial_df |> features(transactions, feat_stl)

#5.1
nonspecial_df |> autoplot(transactions)
fit <- nonspecial_df |> model(trend_model = TSLM(transactions ~ trend()))
fit |> forecast(h = 90)
fit |> forecast(h=90) |>  autoplot(nonspecial_df)

#5.2
nonspecial_df |> model(MEAN(transactions)) |> forecast(h=90) |> autoplot(nonspecial_df)
nonspecial_df |> model(NAIVE(transactions)) |> forecast(h=90) |> autoplot(nonspecial_df)
nonspecial_df |> model(SNAIVE(transactions ~ lag(6))) |> forecast(h=90) |> autoplot(nonspecial_df)
nonspecial_df |> model(RW(transactions ~ drift())) |> forecast(h=90) |> autoplot(nonspecial_df)

#5.3
augment(fit)

#5.4
autoplot(nonspecial_df, transactions)

preds <- nonspecial_df |> model(NAIVE(transactions)) |> augment()
autoplot(preds, .resid)
preds |> ggplot(aes(x=.resid)) + geom_histogram()
preds |> ACF(.resid) |> autoplot()

nonspecial_df |> model(NAIVE(transactions)) |> gg_tsresiduals()

preds |> features(.resid, box_pierce, lag=12)
preds |> features(.resid, ljung_box, lag=12)

fit <- nonspecial_df |> model(RW(transactions ~ drift()))
tidy(fit)
augment(fit) |> features(.resid, ljung_box, lag=12)

#5.5
nonspecial_df |> model(NAIVE(transactions)) |> forecast(h=90) |> hilo()
nonspecial_df |> model(NAIVE(transactions)) |> forecast(h=90) |> autoplot(nonspecial_df)

fit <- nonspecial_df |> model(NAIVE(transactions))
sim <- fit |> generate(h=90, times = 5, bootstrap=TRUE)
sim

nonspecial_df |> ggplot(aes(x= X)) + geom_line(aes(y= transactions)) +
  geom_line(aes(y=.sim, color=as.factor(.rep)), data=sim)

fc <- fit |> forecast(h=90, bootstrap=TRUE)
autoplot(fc, nonspecial_df)

nonspecial_df |> model(NAIVE(transactions)) |> forecast(h=90, bootstrap=TRUE, times=1000) |>
  hilo()

#5.7
dcmp <- nonspecial_df |> model(STL(transactions ~ trend(window=30), robust=TRUE)) |>
  components() |> select(-.model)
dcmp |> model(NAIVE(season_adjust)) |> forecast(h=90) |> autoplot(dcmp, level=50)

fit_dcmp <- nonspecial_df |> model(stlf = decomposition_model(
  STL(transactions ~ trend(window=30), robust=TRUE), NAIVE(season_adjust)))
fit_dcmp |> forecast(h=90) |> autoplot(nonspecial_df)
fit_dcmp |> gg_tsresiduals()

#5.8
nonspecial_df |> slice(n()-89:0)

train_nonspecial_df <- nonspecial_df |> 
  slice(0:(length(nonspecial_df$transactions)-90))

train_fit <- train_nonspecial_df |> model(
  'Mean' = MEAN(transactions),
  'Naive' = NAIVE(transactions),
  'Seasonal naive' = NAIVE(transactions),
  'Drift' = RW(transactions ~ drift())
)

train_fc <- train_fit |> forecast(h=90)
train_fc |> autoplot(nonspecial_df, level=20)
train_fc |> autoplot(train_nonspecial_df, level=20)

accuracy(train_fc, nonspecial_df)

#5.10
cv_df <- nonspecial_df |> stretch_tsibble(.init = 800, .step = 6) |> relocate(date, .id)
cv_df |> model(RW(transactions ~ drift())) |> forecast(h=30) |> accuracy(nonspecial_df)

nonspecial_df |> model(RW(transactions ~ drift())) |> accuracy()

#7.1
nonspecial_df |> ggplot(aes(x=TAVG, y=transactions)) + geom_point() + 
  geom_smooth(method = 'lm', se=FALSE) + labs(title = "Daily Transactions as a Function of Temperature (Sundays & Holidays removed)")

nonspecial_df |> ggplot(aes(x=PRCP, y=transactions)) + geom_point() + 
  geom_smooth(method = 'lm', se=FALSE)

nonspecial_df |> model(TSLM(transactions ~ TAVG)) |> report()
nonspecial_df |> model(TSLM(transactions ~ PRCP)) |> report()

nonspecial_df |> GGally::ggpairs(columns=3:7)

#7.2
fit_mr <- nonspecial_df |> model(tslm = TSLM(transactions ~ TAVG + TMIN + PRCP))
report(fit_mr)

augment(fit_mr) |> ggplot(aes(x=X)) + geom_line(aes(y=transactions,color="Transactions")) +
  geom_line(aes(y=.fitted, color='Fitted'))

augment(fit_mr) |> ggplot(aes(x=transactions, y=.fitted)) + geom_point() +
  geom_abline(intercept=0, slope=1)

#7.3
fit_mr |> gg_tsresiduals()
augment(fit_mr) |> features(.resid, ljung_box, lag=6)

nonspecial_df |> left_join(residuals(fit_mr), by='X') |>
  pivot_longer(PRCP:transactions, names_to = 'regressor', values_to='x') |>
  ggplot(aes(x=x, y=.resid)) + geom_point() + facet_wrap(. ~ regressor, scales='free_x')

augment(fit_mr) |> ggplot(aes(x=.fitted, y=.resid)) + geom_point()

#7.4
fit <- nonspecial_df |> model(TSLM(transactions ~ trend() + season()))
report(fit)

augment(fit) |> ggplot(aes(x=date)) + geom_line(aes(y=transactions, color='Data')) +
  geom_line(aes(y=.fitted, color = 'Fitted'))

augment(fit) |> ggplot(aes(x=transactions, y=.fitted)) + geom_point() +
  geom_abline(intercept=0, slope=1)

fourier_fit <- nonspecial_df |> model(TSLM(transactions ~ trend() + fourier(K=1)))
report(fourier_fit)

augment(fourier_fit) |> ggplot(aes(x=date)) + geom_line(aes(y=transactions, color='Data')) +
  geom_line(aes(y=.fitted, color = 'Fitted'))

augment(fourier_fit) |> ggplot(aes(x=transactions, y=.fitted)) + geom_point() +
  geom_abline(intercept=0, slope=1)

#7.5
glance(fit)
glance(fit) |> select(adj_r_squared, CV, AIC, AICc, BIC)

#7.6
train_nonspecial_df <- nonspecial_df |> slice(0:(length(nonspecial_df$transactions)-90))
valid_nonspecial_df <- nonspecial_df |> slice(n()-89:0)

fit <- train_nonspecial_df |> model(TSLM(transactions ~ trend() + season()))
fc <- fit |> forecast(h=90)

fc |> autoplot(train_nonspecial_df)
accuracy(fc, nonspecial_df)

fit_lm <- nonspecial_df |> model(lm = TSLM(transactions ~ TAVG + PRCP + TMIN + TMAX + 
                                             trend() + season()))
fc_lm <- forecast(fit_lm, new_data = valid_nonspecial_df)

nonspecial_df |> autoplot(transactions, ) + autolayer(fc_lm, level=0) +
  labs(title='Number of transactions 2021 - 2023', x='Date')

sum(fc_lm$.mean) / sum(valid_nonspecial_df$transactions)

#8.1
fit <- nonspecial_df |> 
  model(ETS(transactions ~ error("A") + trend("N") + season("N")))

fc <- fit |> forecast(h=90)
fc |> autoplot(nonspecial_df) + 
  geom_line(aes(y=.fitted), color='blue',data = augment(fit))

#9.1
nonspecial_df |> ACF(transactions) |> autoplot()
nonspecial_df |> ACF(difference(transactions)) |> autoplot()

#9.5
fit <- train_nonspecial_df |> model(ARIMA(transactions))
report(fit)

fc <- fit |> forecast(h=90) 
fc |> autoplot(train_nonspecial_df, level=10)

sum(fc$.mean) / sum(valid_nonspecial_df$transactions)

fit2 <- train_nonspecial_df |> model(ARIMA(transactions ~ pdq(1,1,1)))
report(fit2)

fc2 <- fit2 |> forecast(h=90) 
fc2 |> autoplot(train_nonspecial_df, level=10)

sum(fc2$.mean) / sum(valid_nonspecial_df$transactions)

#9.9
nonspecial_df |> gg_tsdisplay(difference(transactions, 6),
                             plot_type = 'partial', lag=25)

nonspecial_df |> gg_tsdisplay(difference(transactions, 6) |> 
                               difference(), plot_type = 'partial', lag=25)

fit <- train_nonspecial_df |> model(
  arima012011 = ARIMA(transactions ~ 0 + pdq(1,2,2) + PDQ(0,1,1)),
  arima210011 = ARIMA(transactions ~ 0 + pdq(2,1,0) + PDQ(0,1,1)),
  auto = ARIMA(transactions, stepwise = FALSE, approx = FALSE))

fit |> pivot_longer(everything(), names_to = "Model Name", values_to = 'Order')
glance(fit) |> arrange(AICc) |> select(.model:BIC)

fc <- fit |> forecast(h=90) 
fc |> autoplot(train_nonspecial_df, level=10) + 
  labs(x="Date", y ='Transactions', title='ARIMA Model 90-day forecasts')

sum(fc2$.mean) / sum(valid_nonspecial_df$transactions)
sum(fc2$.mean)
sum(valid_nonspecial_df$transactions)

#3.6
stl <- train_nonspecial_df |> model(STL(transactions ~ trend(window=30) + season(window='periodic'),
                                 robust=TRUE)) 

stl_cmp <- stl |>  components()
stl_cmp |> autoplot()




######## STOP ########
######## STOP ########
######## STOP ########
######## STOP ########
######## STOP ########
######## STOP ########




######## FOLLOWING THE PROFESSOR

# ridership = nonspecial_df
# ridership_trn == nonspecial_trn
# Ridership == transactions
# ridership_tslm_fit == nonspecial_tslm_fit
# ridership_tslm_fc == nonspecial_tslm_fc
# ridership_tslm2_fit == nonspecial_tslm2_fit



nonspecial_trn <- head(nonspecial_df,length(nonspecial_df$transactions) - 90)
nonspecial_valid <- tail(nonspecial_df, 90)


#ridership_tslm_fit <- ridership_trn |> 
#  model(
#    tslm = TSLM(Ridership ~ trend())
#  ) 

nonspecial_tslm_fit <- nonspecial_trn |> 
  model(
    tslm = TSLM(transactions ~ trend())
  )

nonspecial_tslm_fit |> report()


#### UPDATED FIGURE 6.2
#ridership_tslm_fit |>
#  augment() |> 
#  autoplot(.fitted) +
#  autolayer(ridership_trn, Ridership) +
#  labs(y = "Ridership")

nonspecial_tslm_fit |>
  augment() |> 
  autoplot(.fitted) +
  autolayer(nonspecial_trn, transactions) +
  labs(y = "Transactions")


#### UPDATED FIGURE 6.3
#ridership_tslm_fc <- ridership_tslm_fit |>
#  forecast(h = 36) 

#ridership_tslm_fc |> 
#  autoplot(level = NULL, ridership_trn |> append_row(36 + 21), linetype = "longdash") |> 
#  ridership_annotation() +
#  geom_line(data = ridership_tslm_fit |> augment(), aes(y = .fitted), color = "blue") 

nonspecial_tslm_fc <- nonspecial_tslm_fit |>
  forecast(h = 90)

nonspecial_tslm_fc |>
  autoplot(level = NULL, nonspecial_trn) +
  geom_line(data = nonspecial_tslm_fit |> augment(), aes(y = .fitted), color='blue')


#### UPDATED TABLE 6.1
#ridership_tslm_fit |> report()

nonspecial_tslm_fit |> report()



#### UPDATED TABLE 6.2
#ridership_tslm2_fit <- ridership_trn |> 
#  model(
#    tslm2 = TSLM(Ridership ~ trend() + I(trend()^2))
#  )
#ridership_tslm2_fit |> report()

nonspecial_tslm2_fit <- nonspecial_trn |>
  model(
    tslm2 = TSLM(transactions ~ trend() + I(trend()^2))
  )

nonspecial_tslm2_fit |> report()


#### UPDATED TABLE 6.3 - MODEL WITH SEASONALITY
#ridership_tslm_season_fit <- ridership_trn |> 
#  model(
#    tslm_season = TSLM(Ridership ~ season())
#  )
#
#ridership_tslm_season_fit |> report()


nonspecial_tslm_season_fit <- nonspecial_trn |>
  model(
    tslm_season = TSLM(transactions ~ season())
  )

nonspecial_tslm_season_fit |> report()


#### UPDATED FIGURE 6.6
#ridership_tslm_season_fc <- ridership_tslm_season_fit |> 
#  forecast(h = 36) 

#ridership |> 
#  append_row(21) |>
#  autoplot(Ridership, alpha = 0.3) |> 
#  ridership_annotation() +
#  geom_line(data = ridership_tslm_season_fit |> augment(), aes(y = .fitted), color = "blue") +
#  autolayer(ridership_tslm_season_fc, level = NULL, color = "blue") +
#  labs(y = "Ridership")

nonspecial_tslm_season_fc <- nonspecial_tslm_season_fit |>
  forecast(h=90)

nonspecial_df |> append_row(90) |>
  autoplot(transactions, alpha=0.3) + 
  geom_line(data=nonspecial_tslm_season_fit |> augment(),  color='blue') 




#### UPDATED  TABLE 6.4
#ridership_tslm_both_fit <- ridership_trn |> 
#  model(
#    tslm3 = TSLM(Ridership ~ trend() + I(trend()^2) + season())
#  )

#ridership_tslm_both_fit |> report()


nonspecial_tslm_both_fit <- nonspecial_trn |>
  model(
    tslm3 = TSLM(transactions ~ trend() + I(trend()^2) + season())
  )

nonspecial_tslm_both_fit |> report()









#### from textbook
nTrain <- length(transactions_df$transactions) - 90
yTrain <- ts(transactions_df$transactions[1:nTrain], start=c(2020,295), freq=365)
xTrain <- data.frame(my_transactions = transactions_df$transactions[1:nTrain])
plot(stl(yTrain, s.window='periodic'))
stlm.reg.fit <- stlm(yTrain, s.window='periodic', xreg=as.matrix(xTrain), method='arima')
stlm.reg.fit$model
plot(stlm.reg.fit$residuals)



#### adapted from textbook
nTrain <- length(nonspecial_df$transactions) - 90
yTrain <- ts(nonspecial_df$transactions[1:nTrain], start=c(2020,252), freq=301)
xTrain <- data.frame(my_transactions = nonspecial_df$transactions[1:nTrain])
plot(stl(yTrain, s.window='periodic'))
stlm.reg.fit <- stlm(yTrain, s.window='periodic', xreg=as.matrix(xTrain), method='arima')
stlm.reg.fit$model
plot(stlm.reg.fit$residuals)







