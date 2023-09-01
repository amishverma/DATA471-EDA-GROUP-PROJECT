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
#pmidf$DATE <- as.Date(pmidf$Date, format="%Y-%m-%d")

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

ggplot(anzdf, aes(x=Date)) +
  geom_line(aes(y=ActualValue, color="Actual")) +
  geom_line(aes(y=ForecastValue, color="Forecast")) +
  geom_line(aes(y=PreviousValue, color="Previous")) +
  ggtitle("ANZ Activity Outlook over Time") +
  xlab("Date") +
  ylab("Value")






# seek job sheet 2 ( JOB AD INDEX)


jobdf$DATE <- as.Date(jobdf$DATE)
appdf$DATE <- as.Date(appdf$DATE)

library(xts)
jobdf_xts <- xts(jobdf[, -1], order.by = jobdf$DATE)

jobdf_monthly <- apply.monthly(jobdf_xts$ADS_SA_INDEX, mean)
jobdf_quarterly <- apply.quarterly(jobdf_xts$ADS_SA_INDEX, mean)
jobdf_yearly <- apply.yearly(jobdf_xts$ADS_SA_INDEX, mean)

jobdf_monthly[1:20]

appdf_xts <- xts(appdf[, -1], order.by = appdf$DATE)

appdf_monthly <- apply.monthly(appdf_xts$CA_SA_INDEX, mean)
appdf_quarterly <- apply.quarterly(appdf_xts$CA_SA_INDEX, mean)
appdf_yearly <- apply.yearly(appdf_xts$CA_SA_INDEX, mean)


library(ggplot2)

# For the SEEK Job Ad Index
ggplot(data.frame(Date=index(jobdf_monthly), ADS_SA_INDEX=coredata(jobdf_monthly)), aes(x=Date, y=ADS_SA_INDEX)) +
  geom_line() +
  ggtitle('Monthly SEEK Job Ad Index') +
  xlab('Date') +
  ylab('ADS_SA_INDEX')

# For the SEEK Applications per Ad Index
ggplot(data.frame(Date=index(appdf_monthly), CA_SA_INDEX=coredata(appdf_monthly)), aes(x=Date, y=CA_SA_INDEX)) +
  geom_line() +
  ggtitle('Monthly SEEK Applications per Ad Index') +
  xlab('Date') +
  ylab('CA_SA_INDEX')











# Decompose the time series
jobdf_ts <- ts(jobdf$ADS_SA_INDEX, frequency = 12)
appdf_ts <- ts(appdf$CA_SA_INDEX, frequency = 12) 



jobdf_ts

# data is monthly

jobdf_decompose <- decompose(jobdf_ts)
head(jobdf_decompose)
appdf_decompose <- decompose(appdf_ts)



# Filter out NA values from the trend component and corresponding dates
jobdf_trend_filtered <- na.omit(data.frame(Date = jobdf$DATE, Trend = jobdf_decompose$trend))

#  plot the trend component
ggplot(jobdf_trend_filtered, aes(x = Date, y = Trend)) +
  geom_line() +
  ggtitle("Trend Component - Job Ad Index") +
  xlab("Date") +
  ylab("Trend Value") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



appdf_ts <- ts(appdf$CA_SA_INDEX, frequency = 12)  # Assuming the data is monthly

# Decompose the time series
appdf_decompose <- decompose(appdf_ts)

# Filter out NA values from the trend component and corresponding dates
appdf_trend_filtered <- na.omit(data.frame(Date = appdf$DATE, Trend = appdf_decompose$trend))

#     the trend component for Applications per Ad Index
ggplot(appdf_trend_filtered, aes(x = Date, y = Trend)) +
  geom_line() +
  ggtitle("Trend Component - Applications per Ad Index") +
  xlab("Date") +
  ylab("Trend Value") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Load necessary libraries
library(dplyr)

# monthly growth rate for Job Ad Index
jobdf <- jobdf %>% 
  arrange(DATE) %>% 
  mutate(Monthly_Growth = (ADS_SA_INDEX / lag(ADS_SA_INDEX) - 1) * 100)


jobdf1<-jobdf%>%
  arrange(DATE)

jobdf1
# monthly growth rate for Applications per Ad Index
appdf <- appdf %>% 
  arrange(DATE) %>% 
  mutate(Monthly_Growth = (CA_SA_INDEX / lag(CA_SA_INDEX) - 1) * 100)

# Plotting the monthly growth rates
ggplot(jobdf, aes(x = DATE, y = Monthly_Growth)) +
  geom_line() +
  ggtitle("Monthly Growth Rate - Job Ad Index") +
  xlab("Date") +
  ylab("Monthly Growth Rate (%)") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(appdf, aes(x = DATE, y = Monthly_Growth)) +
  geom_line() +
  ggtitle("Monthly Growth Rate - Applications per Ad Index") +
  xlab("Date") +
  ylab("Monthly Growth Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





jobdf_seasonal_filtered <- na.omit(data.frame(Date = jobdf$DATE, Seasonal = jobdf_decompose$seasonal))
appdf_seasonal_filtered <- na.omit(data.frame(Date = appdf$DATE, Seasonal = appdf_decompose$seasonal))

# Extract the month from the date for plotting
jobdf_seasonal_filtered$Month <- format(jobdf_seasonal_filtered$Date, "%m - %b")
appdf_seasonal_filtered$Month <- format(appdf_seasonal_filtered$Date, "%m - %b")

# plot the seasonal component for Job Ad Index
ggplot(jobdf_seasonal_filtered, aes(x = Month, y = Seasonal)) +
  geom_boxplot() +
  ggtitle("Seasonal Component - Job Ad Index") +
  xlab("Month") +
  ylab("Seasonal Value")

# plot the seasonal component for Applications per Ad Index
ggplot(appdf_seasonal_filtered, aes(x = Month, y = Seasonal)) +
  geom_boxplot() +
  ggtitle("Seasonal Component - Applications per Ad Index") +
  xlab("Month") +
  ylab("Seasonal Value")



