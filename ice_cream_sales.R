# EDA and Forecast study for Ice Cream Sales
setwd("C:/Users/Marcelo/Desktop/Data/Ice Cream Sales/")
# Load libraries
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)
library(readr)
library(plyr)
library(data.table)

# Loading data
data <- data.table(read.csv("data.txt", nrow = 28, sep = "\t", header = TRUE))
# Naming columns properly
names(data) <- make.names(c("Date", "Temp", "Sales"))
head(data)

# Formating and Converting column to date
data$Date <- format(as.Date(data$Date, format = "%d/%m/%Y"), "%Y-%m-%d")
data$NewDate <- as.Date(data$Date, "%Y-%m-%d")

# Ploting Sales x days
ggplot(data = data, aes(x = NewDate, y = Sales, colour = Temp))+
        geom_point()+
        geom_smooth()+
        ggtitle("Ice Cream Sales per day")+
        ylab("Sales / U$")+
        xlab("Date")+
        theme(plot.title=element_text(size=15))

## Sales are scattered but with an almost linear behaviour ranging from
## 30000 - 50000 U$ depending on T (excluding the outlier)

# Ploting Sales x Temp
ggplot(data = data, aes(x = Temp, y = Sales))+
        geom_point()+
        geom_smooth()+
        ggtitle("Ice Cream Sales as a function of Temperature")+
        ylab("Sales / U$")+
        xlab("Temperature / ºF")+
        theme(plot.title=element_text(size=15))

# Converting Dates into weekdays and weeks
data$weekday <- wday(ymd(data$NewDate))
data$week <- week(ymd(data$NewDate))

data_week <- ddply(data,.(week), summarise, count = mean(Sales))
data_wdays <- ddply(data,.(weekday), summarise, count = mean(Sales))

# Ploting Average Sales per Day and per weekday        
ggplot(data = data_wdays, aes(x = weekday, y = count))+
        geom_point()+
        geom_smooth(formula = y ~ sin(x), se = TRUE)+ # Trying to fit a sin function
        ggtitle("Average Ice Cream Sales per weekday")+
        ylab("Sales / U$")+
        xlab("Weekday")+
        theme(plot.title=element_text(size=15))
        
# Ploting Average Sales per Day and per week        
ggplot(data = data_week, aes(x = week, y = count))+
        geom_point()+
        geom_smooth(formula = y ~ sin(x), se = TRUE)+ # Trying to fit a sin function
        ggtitle("Average Ice Cream Sales per Week")+
        ylab("Sales / U$")+
        xlab("Week")+
        theme(plot.title=element_text(size=15))
        
# Trying to Forecast the sales on 01/11/2019 with a Temprature of 65 ºF
library(forecast)
t.ser <- ts(data$Sales, start = 1, end = 28, freq = 1)
t.ets <- ets(t.ser)
t.fc <- forecast(t.ets,h=4)
t.fc

# Ploting Data and forecast
autoplot(t.fc, ylab = "Days", xlab = "Sales",
         main = "Ice Cream Sales Forecast Prediction")
