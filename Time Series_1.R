library(tseries)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(ggplot2)
library(seasonalview)
library(aTSA)
library(imputeTS)
library(prophet)
library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(lubridate)

Electricity <- read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/Homework1_TS2/hrl_load_metered.csv")
Hold_out = read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/ncsumsa2023tsprojecttestdatasets/hrl_load_metered - test1.csv")
validation = read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/ncsumsa2023tsprojecttestdatasets/hrl_load_metered - test2.csv")
electricityforecast <- rbind(Electricity,Hold_out,validation)
electricityforecast$mw[electricityforecast$mw == 0] = NA
electricityforecast <- na_interpolation(electricityforecast)


electricityforecast <- ts(electricityforecast$mw, start = 8/1/2019, frequency = 24)
#interpolate two points that had values of 0 
electricityforecast[electricityforecast == 0] = NA
electricityforecast <- na_interpolation(electricityforecast)

decomp.train = stl(electricityforecast,s.window = 7)
plot(decomp.train)

electricityforecast %>% diff(lag = 24) %>% ggtsdisplay()

#overall time plot of the electricity forecast
autoplot(electricityforecast) + labs(title="Time Series plot for mw", x="Day",y="MW")

#create training set from overall electricity dataset
training <- subset(electricityforecast,end= length(electricityforecast)- 336)
# #create validation set from overall electricity dataset
valid <- subset(electricityforecast,start=length(electricityforecast)- 335, end= length(electricityforecast)- 168)
# #create test set from overall electricity dataset
test <- subset(electricityforecast, start= length(electricityforecast) - 167)
#create Holt-Winters additive model
HW.Electricity <- hw(training, seasonal = 'additive', h= 168, initial = 'optimal')
autoplot(HW.Electricity, fcol = "Red") + autolayer(fitted(HW.Electricity),series="Trained Model")+
  labs(title="Holt-Winters' Additive Model",x="Hour", y="Total Hourly MegaWatts") +
  geom_vline(xintercept = 1142,linetype="dashed",color="Orange")
summary(HW.Electricity)
#create Holt-Winters multiplicative model
HWM.Electricity <- hw(training, seasonal = 'multiplicative', h= 168, initial = 'optimal')
autoplot(HW.Electricity, fcol = "Red") + autolayer(fitted(HW.Electricity),series="Trained Model")+
  labs(title="Holt-Winters' Multiplicative Model",x="Hour", y="Total Hourly MegaWatts") +
  geom_vline(xintercept = 1142,linetype="dashed",color="Orange")
summary(HWM.Electricity)
#determining the number of differences to take, which is 1
training %>% nsdiffs()
#build a plot comparing original data to differenced data
cbind("Hourly MW Energy Use" = training,
      "Hourly change in MW use" = diff(training, 24)) %>%
  autoplot(facets=TRUE) +
  xlab("Time") + ylab("") +
  ggtitle("Comparison of Differenced Data to Original")

#create a plot to determine the appropriate number of lags to go back
training %>% diff(lag= 168) %>% ggtsdisplay()
#creation of a seasonal ARIMA model with one difference and 2 lags
S.ARIMA <- Arima(training, order=c(2,0,0), seasonal=c(1,1,1))
summary(S.ARIMA)
# need to remove autoplot for seasonal arima on Ryan's code
# autoplot(S.ARIMA)

#checks the residuals 
checkresiduals(S.ARIMA)

#autoarima function on data for 2nd model ARIMA(0,0,2)(0,1,2)[24] 
S.ARIMA2 = auto.arima(training, method = "ML", seasonal = TRUE)
summary(S.ARIMA2)
checkresiduals(S.ARIMA2)
#autoarima function written out 
S.ARIMA3 <- Arima(training, order=c(5,0,0), seasonal=c(2,1,0))

#gives forecast for each day of the week for seasonal ARIMA models with two differences
S.ARIMA.error= valid - forecast::forecast(S.ARIMA, h= 168)$mean
S.ARIMA.MAE <-  mean(abs(S.ARIMA.error))
S.ARIMA.MAPE <- mean(abs(S.ARIMA.error)/abs(valid))*100

S.ARIMA2.error= valid - forecast::forecast(S.ARIMA2, h= 168)$mean
S.ARIMA2.MAE <-  mean(abs(S.ARIMA2.error))
S.ARIMA2.MAPE <- mean(abs(S.ARIMA2.error)/abs(valid))*100

S.ARIMA3.error= valid - forecast::forecast(S.ARIMA3, h= 168)$mean
S.ARIMA3.MAE <-  mean(abs(S.ARIMA3.error))
S.ARIMA3.MAPE <- mean(abs(S.ARIMA3.error)/abs(valid))*100
#Gives MAE of 129.45 and MAPE of 12.14
S.ARIMA.MAE
S.ARIMA.MAPE

S.ARIMA2.MAE
S.ARIMA2.MAPE

S.ARIMA3.MAE
S.ARIMA3.MAE
#gives forecast for each day of the week from Holt-Winters additive & multiplicative models
HW.error= valid - forecast::forecast(HW.Electricity, h= 168)$mean
HW.MAE <- mean(abs(HW.error))
HW.MAPE <- mean(abs(HW.error)/abs(valid))*100

HWM.error= valid - forecast::forecast(HWM.Electricity, h= 168)$mean
HWM.MAE <- mean(abs(HWM.error))
HWM.MAPE <- mean(abs(HWM.error)/abs(valid))*100
#gives MAE of 164.37 and MAPE of 13.91
HW.MAE
HW.MAPE

HWM.MAE
HWM.MAPE

# combining training & validation to test final models
training.valid <- subset(electricityforecast,end= length(electricityforecast)- 168)
F.HWM <- hw(training.valid, seasonal = 'multiplicative', h= 168, initial = 'optimal')
F.ARIMA <- Arima(training.valid, order=c(2,0,0), seasonal=c(1,1,1))

#forecast and evaluate errors
F.HWM.error= test - forecast::forecast(F.HWM, h= 168)$mean
F.HWM.MAE <- mean(abs(F.HWM.error))
F.HWM.MAPE <- mean(abs(F.HWM.error)/abs(test))*100

F.ARIMA.error= test - forecast::forecast(F.ARIMA, h= 168)$mean
F.ARIMA.MAE <- mean(abs(F.ARIMA.error))
F.ARIMA.MAPE <- mean(abs(F.ARIMA.error)/abs(test))*100
# plot ARIMA model forecast
autoplot(forecast::forecast(F.ARIMA,h=168)) +  autolayer(fitted(F.ARIMA),series="Validated Model")+
  labs(title="ARIMA(2,0,0)(1,1,1)24 Model",x="Hour", y="Total Hourly MegaWatts")

#print MAE & MAPE
F.HWM.MAE
F.HWM.MAPE

F.ARIMA.MAE
F.ARIMA.MAPE

#choose fourier variables
plots = list()
for (i in seq(6)){
  fit = auto.arima(TS, xreg = fourier(TS, K = i),
                   seasonal = FALSE, lambda = NULL)
  plots[[i]] = autoplot(forecast::forecast(fit,xreg = fourier(TS, K = i, h=7))) + 
    xlab(paste("K=",i,"   BIC=", round(fit$bic,2))) + 
    ylab("")
}
gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],nrow=3)

TS = ts(electricityforecast$y, start = 8/1/2019 ,frequency = 24)

#############################
Electricity <- read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/Homework1_TS2/hrl_load_metered.csv")
Hold_out = read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/ncsumsa2023tsprojecttestdatasets/hrl_load_metered - test1.csv")
validation = read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/ncsumsa2023tsprojecttestdatasets/hrl_load_metered - test2.csv")
electricityforecast <- rbind(Electricity,Hold_out,validation)
electricityforecast$mw[electricityforecast$mw == 0] = NA
electricityforecast <- na_interpolation(electricityforecast)

week3 = read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/hrl_load_metered - test3.csv")
# convert to time series class
week3TS = ts(week3$mw, frequency = 24)
#make a dataframe with every hour from start to end date of training data set
ds=seq(as.POSIXct('2019-08-01'), as.POSIXct('2022-10-06 23:00:00'),by='hour')

#make a dataframe with added additional week for forecast
dsforecast=seq(as.POSIXct('2019-08-01'), as.POSIXct('2022-10-13 23:00:00'),by='hour')

#create prophet dataframe
prophet.data = data.frame(ds=ds,y=electricityforecast)
#rename MW variable to y *must rename so that prophet code will run later
prophet.data = rename(prophet.data,y = y.mw)

#create sunday holidays using dsforecast, have to have dates from forecasted week or holidays won't do anything
sunday <- data.frame(
  holiday = 'sunday',
  ds = dsforecast[format(dsforecast,format="%w") %in% c(0)], #this filters for all instances where the date falls on a Sunday
  lower_window = 0, 
  upper_window = 0
)

#create summer holidays
summer <- data.frame(
  holiday = 'Summer',
  ds = dsforecast[format(dsforecast,format="%B") %in% c('July','August')], #this filters for all instances where the date falls in July or Aug
  lower_window = -1, #incorporate some lags & timepoits before
  upper_window = 1
)

#create 6 pm holidays
evening <- data.frame(
  holiday = 'evening',
  ds = dsforecast[format(dsforecast,format="%H") %in% c(18)], #filters for all date/time where it's 6 PM EST, hottest time of the day when i sorted MW in training data
  lower_window = 0,  
  upper_window = 0
)
holidays = bind_rows(sunday,summer,evening) #combine individual holidays into 1 dataframe

#create prophet model
Prof <- prophet(holidays = holidays, holidays.prior.scale = 0.05)
Prof <- add_country_holidays(Prof,"US") #add US holidays
# Prof <- add_seasonality(Prof, name='daily',period=24,fourier.order =3)
Prof <- fit.prophet(Prof, prophet.data)

#make future data frame with training + 168 hr (1 week forecast)
forecast.data <- make_future_dataframe(Prof, periods = 168, freq = 'hour')
#predict
forecast.predict = predict(Prof,forecast.data)
#plot
plot(Prof, forecast.predict, main = "",xlab = "Dates", ylab= "Hourly Megawatts")
title(main = "Prophet Model Forecast on Training")
prophet_plot_components(Prof, forecast.predict)

# prophet validation with week 3
Prophet.error <- week3TS - tail(forecast.predict$yhat,168)

Prophet.MAE <- mean(abs(Prophet.error))
Prophet.MAPE <- mean(abs(Prophet.error)/abs(week3TS))*100

#Neural Network Model
set.seed(12345)
#using ARIMA (5,0,0)(2,1,0)
#forecasting next 100 days
NN.Model <- nnetar(diff(TS, 168), p = 5, P = 3)

NN.Forecast <- forecast::forecast(NN.Model, h = 168)
plot(NN.Forecast)


Pass.Forecast <- rep(NA, 168)

for(i in 1:168){
  Pass.Forecast[i] <- TS[length(TS) - 168 + i] + forecast::forecast(NN.Model, h = 168)$mean[i]
}

Pass.Forecast <- ts(Pass.Forecast, frequency = 24)

plot(TS, main = "US Airline Passengers ARIMA Model Forecasts", xlab = "Date", ylab = "Passengers (Thousands)")
lines(Pass.Forecast, col = "blue")
abline(v = 1150, col = "red", lty = "dashed")

# Calculate prediction errors from forecast
NN.error <- week3TS - Pass.Forecast

# Calculate prediction error statistics (MAE and MAPE)
NN.MAE <- mean(abs(NN.error))
NN.MAPE <- mean(abs(NN.error)/abs(week3TS))*100

NN.MAE

## Creating Some Visualizations ##


## CREATING PLOTS OF FORECAST VS ACTUAL DATA##

Prophet.model <- tail(forecast.predict$yhat,168) %>% as_tibble() %>% rename('Prophet'= 'value')

Neural.model <- Pass.Forecast %>% as_tibble() %>% rename('Neural' = 'x') %>% select('Neural') %>% cbind(Prophet.model)

testHW2 <- week3 %>% mutate(Date = mdy_hm(datetime_beginning_ept)) %>% cbind(Neural.model) %>%
  select(c("Date", "mw", "Prophet", "Neural" ))

colors <- c("Hourly Megawatts Observed" = "black", "Prophet Forecast" = "cyan", "Neural Network Forecast" = "blue")

ggplot(testHW2, aes(x=Date)) + 
  geom_line(aes(y = mw, color="Hourly Megawatts Observed"), size=1.6, alpha=.3) + 
  geom_line(aes(y = Prophet, color="Prophet Forecast"), size = 1.4, alpha = .5) + 
  geom_line(aes(y = Neural, color="Neural Network Forecast"), alpha = .8) +
  ggtitle("Forecasts on Validation Data") +
  theme(text = element_text(size=12)) +
  labs(x = "Date",
       y = "Megawatt",
       color = "Legend") +
  scale_color_manual(values = colors) + theme(legend.position='top', legend.justification='left')
