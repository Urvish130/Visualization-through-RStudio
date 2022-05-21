library(tidyverse)
library(caret)
library(psych) 
library(ISLR)
library(dlookr)
library(lubridate)
library(readr)
library(broom)
library(ggplot2)
library(MLmetrics)
library(forecast)
library(dplyr)

getwd()                                        #getting current directory
setwd("F://Data Visualization 2nd sem//Project")   #setting the directory
df = read.csv("US_Accidents.csv") 
table(is.na(df))
df = na.omit(df)
table(is.na(df))
df = unique(df)

str(df)
summary(df)
describe(df)

normality(df)    # hypothesis test of normality

pdf("my_plot.pdf")         

plot_normality(df)
dev.off()
correlate(df)
plot_correlate(df)           #Correlation matrix

df$Amenity = as.integer(df$Amenity == "TRUE")
df$Bump  = as.integer(df$Bump  == "TRUE")
df$Crossing = as.integer(df$Crossing == "TRUE")
df$Give_Way  = as.integer(df$Give_Way  == "TRUE")
df$Junction  = as.integer(df$Junction  == "TRUE")
df$No_Exit  = as.integer(df$No_Exit  == "TRUE")
df$Railway  = as.integer(df$Railway  == "TRUE")
df$Roundabout  = as.integer(df$Roundabout  == "TRUE")
df$Station    = as.integer(df$Station    == "TRUE")
df$Stop   = as.integer(df$Stop   == "TRUE")
df$Traffic_Calming  = as.integer(df$Traffic_Calming  == "TRUE")
df$Traffic_Signal   = as.integer(df$Traffic_Signal   == "TRUE")
df$Turning_Loop   = as.integer(df$Turning_Loop   == "TRUE")
df$Sunrise_Sunset   = as.integer(df$Sunrise_Sunset   == "Day")
df$Civil_Twilight    = as.integer(df$Civil_Twilight   == "Day")
df$Nautical_Twilight   = as.integer(df$Nautical_Twilight   == "Day")
df$Astronomical_Twilight   = as.integer(df$Astronomical_Twilight   == "Day")

tab_sev = table(df$State,df$Severity)
tab_sev

tab_sev_df = data.frame(tab_sev)
colnames(tab_sev_df) = c("State","Severity","Accidents")
tab_sev_df

set_plot_dimensions <- function(width_choice, height_choice)                   #https://medium.com/analytics-vidhya/visual-outputs-in-r-example-1-d71cb4be50eb
{options(repr.plot.width = width_choice, repr.plot.height = height_choice)}


set_plot_dimensions(50,25)


p = ggplot(data=tab_sev_df,aes(x=State,y=Accidents,fill=Severity))+
  geom_bar(stat = 'identity')
p
boxplot(`Temperature.F.`~Severity, data=df,col.axis="red",
        las=0,
        col=rainbow(4),xlab="Severity",ylab="Temperature(F)"
)

tab_wea = table(df$Weather_Condition,df$Severity)
tab_wea_df = data.frame(tab_wea)
colnames(tab_wea_df) = c("Weather_Condition","Severity","Accidents")


set_plot_dimensions(50,25)

ggplot(data=tab_wea_df,aes(x=Weather_Condition,y=Accidents,fill=Severity))+
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(colour = "red",angle=90))

subset_wea = subset(tab_wea_df, Accidents>1000)
subset_wea

set_plot_dimensions(50,25)


ggplot(data=subset_wea,aes(x=Weather_Condition,y=Accidents,fill=Severity))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(colour = "red",angle=90))

subset_wea_Se4 = subset(tab_wea_df, Severity==4 & Accidents>500)
subset_wea_Se4

ggplot(data=subset_wea_Se4,aes(x=Weather_Condition,y=Accidents,fill=Severity))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(colour = "red",angle=90))


#before Cleaning
df_actual = read.csv("US_Accidents.csv")
accidents_new_actual = df_actual %>%
  mutate(startHr=hour(Start_Time))
head(accidents_new_actual)
accidents_count_actual = accidents_new_actual %>%
  dplyr::count(startHr)
head(accidents_count_actual)

ggplot(accidents_count_actual, aes(startHr, n)) + geom_point() +geom_path()
cor(df[,sapply(df,is.numeric)],use="complete.obs",method="pearson")

#after CLeaning
accidents_new = df %>%
  mutate(startHr=hour(Start_Time))
head(accidents_new)
accidents_count = accidents_new %>%
  dplyr::count(startHr)
head(accidents_count)
ggplot(accidents_count, aes(startHr, n)) + geom_point() +geom_path() 

my_func <- function(df, group){
  df %>%
    group_by(!!group) %>%
    summarise(my_count = n()) %>%
    arrange(desc(my_count))
}

#Count of severity 
Count_severity = my_func(df, quo(Severity))
ggplot(data=Count_severity, aes(x=Severity, y=my_count)) +
  geom_bar(stat="identity", fill="steelblue", width = 1)+
  geom_text(aes(label=my_count), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + ylab("Number of accidents") + scale_y_continuous(labels = scales::comma)

bp= ggplot(Count_severity, aes(x="", y=my_count,fill=Severity))+
  geom_bar(width = 1, stat = "identity")
pie = bp + coord_polar("y", start=0)
pie + scale_y_continuous(labels = scales::comma) 
library(ggpubr)
ggboxplot(df, x = "Severity", y = "Wind_Chill.F.", width = 0.8)

Count_severity_city = my_func(df, quo(City))
Count_severity_city = head(Count_severity_city, 20)
ggplot(data=Count_severity_city, aes(x=City, y=my_count)) +
  geom_bar(stat="identity", fill="steelblue", width = 1)+
  geom_text(aes(label=my_count), vjust=2.6, color="white", size=2.5)+
  theme_minimal() + ylab("Number of Accidents")

#Time Series Fore Casting
df_for_TSF = read.csv("US_Accidents.csv")
head(df_for_TSF)
df_for_TSF$MY = format(as.Date(df_for_TSF$Start_Time), "%m-%y")
count_accidents_MY = my_func(df_for_TSF,quo(MY))

library(zoo)
count_accidents_MY <- count_accidents_MY[order(as.yearmon(count_accidents_MY$MY, "%m-%Y")),]
head(count_accidents_MY,20)

summary(count_accidents_MY)
accidents.ts = ts(count_accidents_MY$my_count, start = 2016, end = 2020,frequency = 12)
plot(accidents.ts, xlab = "Time", ylab = "Number of accidents", )
plot(decompose(accidents.ts))
acf(accidents.ts)
pacf(accidents.ts)

library(tseries)
adf.test(accidents.ts)

seasonplot(accidents.ts, xlab = "Month", ylab = "Number of accidents", main="Seasonal plot", year.labels.left = TRUE, col =1:20, pch = 19)

monthplot(accidents.ts, xlab = "Month", ylab = "Number of accidents", main="Seasonal Standard deviation",xaxt = "n")
axis(1, at = 1:12, labels = month.abb, cex = 0.8)

ntrain = length(accidents.ts) - 12
train.ts = window(accidents.ts, start =c(2016,1), end = c(2016,ntrain))
valid.ts = window(accidents.ts, start =c(2016, ntrain +1 ), end = c(2016,ntrain+ 12))


#naive model
model_naive = snaive(train.ts, h = 12)
MAPE(model_naive$mean, 12)*100
accuracy(model_naive, valid.ts)
plot(accidents.ts, xlab = "Time", ylab = "Number of accidents", )
lines(model_naive$mean, col="red", lwd=2)

#ARIMA model
model_arima = auto.arima(train.ts)
summary(model_arima)
model_arima_pred = forecast(model_arima, h= 12, level = 0)
accuracy(model_arima_pred, valid.ts)
Box.test(model_arima$residuals)
plot(model_arima_pred)

pred = predict(auto.arima(train.ts), n.ahead = 10*12)
plot(accidents.ts, xlab = "Time", ylab = "Number of accidents", xlim = c(2016,2027))
lines(pred$pred, col="red", lwd=2)

Forecast_val = ((forecast(accidents.ts, h=30)))
plot(accidents.ts, xlab = "Time", ylab = "Number of accidents", xlim = c(2016,2027))
lines(Forecast_val$mean, col="red", lwd=2)

Forecast_val2 = (forecast(auto.arima(train.ts), h=30))
plot(accidents.ts, xlab = "Time", ylab = "Number of accidents", xlim = c(2016,2027))
lines(Forecast_val2$mean, col="red", lwd=2)

highest_accident_zipcode=my_func(df, quo(Zipcode))
head(highest_accident_zipcode, 20)
ggplot(data=head(highest_accident_zipcode, 20), aes(x=Zipcode, y=my_count)) +
  geom_bar(stat="identity", fill="steelblue", width = 1)+
  geom_text(aes(label=my_count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

highest_accident_area = my_func(df, quo(Street))
head(highest_accident_area, 20)
ggplot(data=head(highest_accident_area, 20), aes(x=Street, y=my_count)) +
  geom_bar(stat="identity", fill="steelblue", width = 1)+
  geom_text(aes(label=my_count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

library(usmap)
library(ggplot2)
library(usmap)
library(ggplot2)

# Lat/Lon of Sioux Falls, SD
test_data = data.frame(df$Start_Lng,df$Start_Lat)

transformed_data = usmap_transform(test_data)
plot_usmap("states",labels = TRUE) + 
  geom_point(data = transformed_data, 
             aes(x = df.Start_Lng.1, y = df.Start_Lat.1), 
             color = "red",
             size = .5)

#best model logistic Model
best_logistic_model = glm(formula = as.factor(Severity) ~ Temperature.F. + Civil_Twilight + 
                            Nautical_Twilight + Astronomical_Twilight + Humidity... + 
                            Pressure.in. + Wind_Speed.mph. + Precipitation.in., family = binomial, 
                          data = df)
summary(best_logistic_model)



df$days <- weekdays(as.Date(df$Start_Time))
#after CLeaning
accidents_count = df %>%
  dplyr::count(days)
head(accidents_count)
ggplot(accidents_count, aes(startHr, n)) + geom_point() +geom_path() 





