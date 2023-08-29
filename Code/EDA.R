# Initial file

covdf <- read.csv('Data/covid_19_data_portal.csv')

anzdf <- read.csv('Data/new-zealand.anz-activity-outlook.csv', 
                  sep='\t')

pmidf <- readxl::read_excel(path = 'Data/PMI-Time-Series-Data.xls', 
                            skip=1)
names(pmidf)[1] <- 'DATE'

jobdf <- readxl::read_excel(path = 'Data/Seek_NZ_data_01-07-2023.xlsx', 
                            sheet = 'SEEK Job Ad Index')

appdf <- readxl::read_excel(path = 'Data/Seek_NZ_data_01-07-2023.xlsx', 
                            sheet = 'SEEK Applications per Ad Index')

head(covdf)
head(anzdf)
head(pmidf)
head(jobdf)
head(appdf)

library(ggplot2)
library(forecast)
library(readxl)
library(lubridate)

covdf$parameter <- as.Date(covdf$parameter, format="%Y-%m-%d")
anzdf$Date <- as.Date(anzdf$Date, format="%Y.%m.%d")
pmidf$DATE <- as.Date(pmidf$Date, format="%Y-%m-%d")

class(covdf$parameter)
class(anzdf$Date)
class(pmidf$DATE)


library(ggplot2)
ggplot(covdf, aes(x=parameter, y=value)) +
  geom_line() +
  ggtitle("New Zealand Activity Index over Time") +
  xlab("Date") +
  ylab("Activity Index")


ggplot(pmidf, aes(x=DATE, y=PMI)) +
  geom_line() +
  ggtitle("PMI over Time") +
  xlab("Date") +
  ylab("PMI")



library(forecast)
covid_ts <- ts(covdf$value, start=c(2003, 10), frequency=12)
decomposed_covid <- stl(covid_ts, s.window="periodic")
plot(decomposed_covid)

arima_model <- auto.arima(covid_ts)
future_forecast <- forecast(arima_model, h=12)
plot(future_forecast)


ggplot(anzdf, aes(x=Date)) +
  geom_line(aes(y=ActualValue, color="Actual")) +
  geom_line(aes(y=ForecastValue, color="Forecast")) +
  geom_line(aes(y=PreviousValue, color="Previous")) +
  ggtitle("ANZ Activity Outlook over Time") +
  xlab("Date") +
  ylab("Value")

