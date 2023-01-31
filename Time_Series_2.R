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
library(stringr)
library(lubridate)

#reading in original data
#166 weeks
Electricity <- read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/Homework1_TS2/hrl_load_metered.csv")

#reading in test data 1
#1 week
Hold_out1 = read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/ncsumsa2023tsprojecttestdatasets/hrl_load_metered - test1.csv")

#reading in test data  2
#1 week
Hold_out2  = read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/ncsumsa2023tsprojecttestdatasets/hrl_load_metered - test2.csv")

#reading in test data 3
#1 week
Hold_out3= read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/hrl_load_metered - test3.csv")

#reading in test data 4
HOld_out4=read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/hrl_load_metered - test4.csv")

#read in week 5 CSV
week5 = read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/hrl_load_metered - test5.csv")
week5 = ts(week5$mw, frequency = 24)

#combining all data sets
electricityforecast <- rbind(Electricity,Hold_out1,Hold_out2,Hold_out3,HOld_out4)

#interpolate two points that had values of 0 
electricityforecast[electricityforecast == 0] = NA
electricityforecast <- na_interpolation(electricityforecast)

#creating time series object
#we have hourly data and daily seasons, 24 hrs=1 day
electricityforecast <- ts(electricityforecast$mw, start = 8/1/2019, frequency = 24)

#create training set from overall electricity dataset

trainx = subset(electricityforecast,end= length(electricityforecast)- 168)

#create validation set from overall electricity dataset

valid <- subset(electricityforecast, start= length(electricityforecast) - 167)

#overall time plot of the electricity forecast
autoplot(electricityforecast) + labs(title="Time Series plot for mw", x="Day",y="MW")

#####################################################
#Recreating HW ESM
HW.Electricity <- hw(trainx, seasonal = 'multiplicative', h= 168, initial = 'optimal')

HW.error= valid - forecast::forecast(HW.Electricity, h= 168)$mean
HW.MAE <- mean(abs(HW.error))
HW.MAPE <- mean(abs(HW.error)/abs(valid))*100

#gives MAE of 383.43 and MAPE of 45.45
HW.MAE
HW.MAPE

##########################################
#Recreating Seasonal ARIMA
S.ARIMA <- Arima(trainx, order=c(5,0,0), seasonal=c(2,1,0))

S.ARIMA.error= valid - forecast::forecast(S.ARIMA, h= 168)$mean
S.ARIMA.MAE <-  mean(abs(S.ARIMA.error))
S.ARIMA.MAPE <- mean(abs(S.ARIMA.error)/abs(valid))*100

#Gives MAE of 69.43 and MAPE of 9.26
S.ARIMA.MAE
S.ARIMA.MAPE

################################
#Recreating Prophet
#make a dataframe with every hour from start to end date of training data set
ds=seq(as.POSIXct('2019-08-01'), as.POSIXct('2022-10-13 23:00:00'),by='hour')
#make a dataframe with added additional week for forecast
dsforecast=seq(as.POSIXct('2019-08-01'), as.POSIXct('2022-10-20 23:00:00'),by='hour')

#create prophet dataframe
prophet.data = data.frame(ds=ds,y=trainx)

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
Prof <- prophet(holidays = holidays)
Prof <- add_country_holidays(Prof,"US") #add US holidays
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
Prophet.error <- valid - tail(forecast.predict$yhat,168)

Prophet.MAE <- mean(abs(Prophet.error))
Prophet.MAPE <- mean(abs(Prophet.error)/abs(valid))*100



################################
#Dynamic model
#add temperature data 
#reading in original data
#166 weeks
Temperature <- read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/rt_tempset.csv")

temp1 = read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/rt_tempset 1.csv")

temp2  = read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/rt_tempset 2.csv")

temp3= read.csv("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/rt_tempset 3.csv")

#combining all temp data sets
PJMtemp <- rbind(Temperature,temp1,temp2,temp3)
#remove UTC and ending EPT datetime
PJMtemp = PJMtemp[-c(1,3,4)]
#string split temp variable
PJMtemp$rt_temperature_set = str_sub(PJMtemp$rt_temperature_set,1,2)

#convert date variable to POSIX
PJMtemp$datetime_beginning_ept = mdy_hms(PJMtemp$datetime_beginning_ept)

electNtemp <- rbind(Electricity,Hold_out1,Hold_out2,Hold_out3,HOld_out4)
electNtemp$datetime_beginning_ept = mdy_hm(electNtemp$datetime_beginning_ept)

#join temp and electricity
electNtemp = left_join(electNtemp,PJMtemp, by = "datetime_beginning_ept")
electNtemp = electNtemp[-7] #remove region copied variable from join
electNtemp$rt_temperature_set = as.numeric(electNtemp$rt_temperature_set)

ENT = electNtemp[c(1,6,7)]
ENT[ENT == 0] = NA
ENT <- na_interpolation(ENT)
ENT <- ts(ENT, start = 8/1/2019, frequency = 24)

autoplot(ENT[,2:3], facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Quarterly changes in PJM hourly MW load
    and AECO temperature")

Workday = rep(0,nrow(electNtemp))
Workday = as.data.frame(Workday)
Workday$date = electNtemp$datetime_beginning_ept

Workday$Workday[format(Workday$date,format="%w") %in% c(1:5)] = 1


xreg <- cbind(MaxTemp = electNtemp$rt_temperature_set, Work = Workday$Workday)
fit <- Arima(trainx, order=c(5,0,0), seasonal=c(2,1,0),xreg=xreg[1:28080,], method = "ML")

checkresiduals(fit)
fcast = forecast::forecast(fit,xreg = cbind(MaxTemp = electNtemp$rt_temperature_set[28081:28248], Work = Workday$Workday[28081:28248]))
autoplot(fcast) + ylab("Eletricity Demand (MW)")

DYN.error= valid - fcast$mean
DYN.MAE <- mean(abs(DYN.error))
DYN.MAPE <- mean(abs(DYN.error)/abs(valid))*100

DYN.MAE
#####################################
#Recreating Neural Network Model
set.seed(12345)
#using ARIMA (5,0,0)(2,1,0)
#forecasting next 168 hours
NN.Model <- nnetar(diff(trainx, 168), p = 5, P = 3)

NN.Forecast <- forecast::forecast(NN.Model, h = 168)
plot(NN.Forecast)


Pass.Forecast <- rep(NA, 168)

for(i in 1:168){
  Pass.Forecast[i] <- trainx[length(trainx) - 168 + i] + NN.Forecast$mean[i]
}

#plot(TS, main = "US Airline Passengers ARIMA Model Forecasts", xlab = "Date", ylab = "Passengers (Thousands)")
#lines(Pass.Forecast, col = "blue")
#abline(v = 1150, col = "red", lty = "dashed")

# Calculate prediction errors from forecast
NN.error <- valid - Pass.Forecast


# Calculate prediction error statistics (MAE and MAPE)
NN.MAE <- mean(abs(NN.error))
NN.MAPE <- mean(abs(NN.error)/abs(valid))*100

NN.MAE

##########################################
#Averaged models
# simple average of HW, ARIMA, Prophet, & Neural
For.Avg <- (forecast::forecast(HW.Electricity, h= 168)$mean + 
               Pass.Forecast +
              tail(forecast.predict$yhat,168)+forecast::forecast(S.ARIMA, h= 168)$mean)/4

Avg.error <- valid - For.Avg

Avg.MAE <- mean(abs(Avg.error))
Avg.MAPE <- mean(abs(Avg.error)/abs(valid))*100

##########################################
# Visualizations of 2 best models Avg and Neural

Avg.model <- For.Avg.PNN %>% as_tibble() %>% rename('Average'= 'x')

Neural.model <- Pass.Forecast %>% as_tibble() %>% rename('Neural' = 'value') %>% select('Neural') %>% cbind(Avg.model)

testFinal <- HOld_out4 %>% mutate(Date = mdy_hm(datetime_beginning_ept)) %>% cbind(Neural.model) %>%
  select(c("Date", "mw", "Average", "Neural" ))

colors <- c("Hourly Megawatts Observed" = "black", "Average Forecast" = "cyan", "Neural Network Forecast" = "blue")

ggplot(testFinal, aes(x=Date)) + 
  geom_line(aes(y = mw, color="Hourly Megawatts Observed"), size=1.6, alpha=.3) + 
  geom_line(aes(y = Average, color="Average Forecast"), size = 1.4, alpha = .5) + 
  geom_line(aes(y = Neural, color="Neural Network Forecast"), alpha = .8) +
  ggtitle("Forecasts on Validation Data") +
  theme(text = element_text(size=12)) +
  labs(x = "Date",
       y = "Megawatt",
       color = "Legend") +
  scale_color_manual(values = colors) + theme(legend.position='top', legend.justification='left')

######################
#Neural Network on Test Data
#Recreating Neural Network Model rolled up

set.seed(12345)
#using ARIMA (5,0,0)(2,1,0)
#forecasting next 168 hours
NN.Model.Final <- nnetar(diff(electricityforecast, 168), p = 5, P = 3)

#forecast 144 to match week 5
NN.Forecast.Final <- forecast::forecast(NN.Model.Final, h = 144)
plot(NN.Forecast.Final)

Pass.Forecast.Final <- rep(NA, 144)

for(i in 1:144){
  Pass.Forecast.Final[i] <- electricityforecast[length(electricityforecast) - 144 + i] + NN.Forecast.Final$mean[i]
}

# Calculate prediction errors from forecast
NN.error.Final <- week5 - Pass.Forecast.Final

# Calculate prediction error statistics (MAE and MAPE)
NN.MAE.Final <- mean(abs(NN.error.Final))
NN.MAPE.Final <- mean(abs(NN.error.Final)/abs(week5))*100

################################## FINAL CSV
# roll up all data prior to Oct 28, 2022 to create test CSV for Dr. LaBarr
#combining all data sets
Final.data <- rbind(Electricity,Hold_out1,Hold_out2,Hold_out3,HOld_out4,week5)

#interpolate two points that had values of 0 
Final.data[Final.data == 0] = NA
Final.data <- na_interpolation(Final.data)

#creating time series object
#we have hourly data and daily seasons, 24 hrs=1 day
Final.TS <- ts(Final.data$mw, start = 8/1/2019, frequency = 24)

Final.model <- nnetar(diff(Final.TS, 168), p = 5, P = 3)

#forecast 192 hours from oct 27 to Nov 3
Final.forecast <- forecast::forecast(Final.model, h = 192)
plot(Final.forecast)

Final.pass <- rep(NA, 192)

for(i in 1:192){
  Final.pass[i] <- Final.TS[length(Final.TS) - 192 + i] + Final.forecast$mean[i]
}

#create dataframe of date/time to combine with forecasts
Final.date=seq(as.POSIXct('2022-10-27 00:00:00'), as.POSIXct('2022-11-03 23:00:00'),by='hour')

Final.df = as.data.frame(Final.date)
Final.df$mw = Final.pass

#subset dates oct 28 to nov 3 and write CSV file
Final.csv = tail(Final.df,n = 168)

write.csv(Final.csv,"/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Time Series II/Forecast10-28-to-11-03.csv")

