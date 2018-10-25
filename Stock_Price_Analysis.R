install.packages("quantmod")
library(quantmod)
library(forecast)

# Web Scrapping from yahoo finance, retrieving the data for amazon stock using AMZN tick.
amz <- getSymbols("AMZN", auto.assign=F,
                  from = "2015-01-01", to = "2018-10-19")
head(amz)
class(amz)
summary(amz)
str(amz)

chartSeries(amz, type = "line")

amz1 <- as.data.frame(amz)
amz1
amz_ts <- ts(amz1$AMZN.Open, start = c(2015,1), end = c(2018,10), frequency = 12)
plot(amz_ts, main = 'Timeseries', ylab = "Amazon_Open")

stl1 <- stl(amz_ts, s.window = 12)
plot(stl1, col = "red ", main="Seasonal Decomposition")

deseasonal_cnt <- seasadj(stl1)
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)

ACF <- acf(count_d1, col="blue", main="ACF Decomposition")
Pacf <- pacf(count_d1, col="blue", main= "PACF Decomposition")

# Train test split for holt-winters.
train_ts <- window(amz_ts, start = c(2015,1), end = c(2017,10))
test_ts <- window(amz_ts, start = c(2017,11))

# Train test split for arima
train_ts1 <- window(amz_ts - stl1$time.series[,1], start = c(2014,1), end = c(2017,10))
print(train_ts1)
test_ts1 <- window(amz_ts - stl1$time.series[,1], start = c(2017,11))
print(test_ts1)

# Using the holt winter model
fit = HoltWinters(train_ts, alpha = 0.9)
print(fit)
forecast <- forecast(fit, 12)
plot(forecast, main = "Holt-Winter", ylab="Amazon_Open")
lines(test_ts, col = "red")

# Using auto arima model
fit_auto <- auto.arima(train_ts1,seasonal=T, d= 1, D=1)
fit_auto
forecasted_ts1 <- forecast(fit_auto, 12)
print(forecasted_ts1$mean)
plot(forecasted_ts1, main = "Auto-Arima", ylab="Amazon_Open")
lines(test_ts1, col = "red")

# Using Arima model
amzarima2 = Arima(train_ts1, order = c(1,1,0), seasonal = list(order=c(0,1,0), period = 12))
amzarima2
forecasted_arima <- forecast(amzarima2, 12)
plot(forecasted_arima, main = "Arima")
lines(test_ts1, col = "red")


# Accuracy for Arima
arima_accuracy <- 1/12 *sum(abs(forecasted_arima$mean - test_ts1))
print(arima_accuracy)

# Accuracy for auto.arima
auto.arima_accuracy <- 1/12 *sum(abs(forecasted_ts1$mean - test_ts1))
print(auto.arima_accuracy)

# Accuracy for Holt-Winters
HoltWinters_accuracy <- 1/12 *sum(abs(forecast$mean - test_ts))
print(HoltWinters_accuracy)

# Forecasted value with the actual data for all the three method.
autoplot(amz_ts) + geom_line(size=2) +
  forecast::autolayer(forecast$mean, series = "Holt-Winters", size = 1.0) +
  forecast::autolayer(forecasted_arima$mean, series = "Arima", size = 1.0) +
  forecast::autolayer(forecasted_ts1$mean, series = "Auto-ARIMA", size = 1.0) +
  xlab("year") + ylab("Amazon Stock price in US dollars for Open price") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Amazon Stock Price") + theme(plot.title=element_text(family="Times", hjust = 0.5, 
                                                                color = "blue", face="bold", size=15))

### Adding the rownames as date
amz1$Date = rownames(amz1)
amz1$Date
amz1$Date = as.Date(amz1$Date)
head(amz1)

my_dates = seq.Date(from = as.Date("2015-01-02"), 
                    to = as.Date("2018-10-19"), 
                    by = 1)
my_dates = data.frame(Date = my_dates)
my_data = merge(amz1, my_dates, by = "Date", all.y = T)
head(my_data)

# Removing initial days to start on monday
my_data = my_data[4:1387,]
# Removing sundays,
my_data = my_data[-(seq(from = 7, to = nrow(my_data), by = 7)),]
# Removing saturdays,
my_data = my_data[-(seq(from = 6, to = nrow(my_data), by = 6)),]
# Using last observatoin carried forward imputation
my_data = na.locf(my_data)
head(my_data)
class(my_data$AMZN.Open)

# Putting the Highprice into a weekly time series
highest_price = ts(my_data$AMZN.High, 
                   frequency = 5)
median(highest_price)
seasonplot(highestprice, season.labels = c("Mon", "Tue", "Wed", "Thu", "Fri"))
monthplot(highest_price, base = median, col.base = "red")

# Comparison with the low prices
par(mfrow = c(1,2))
lowest_price = ts(my_data$AMZN.Close, 
                  frequency = 5)
median(lowest_price)
monthplot(lowest_price, base = median, col.base = "red")

monthplot(highest_price, base = median, col.base = "red")
par(mfrow = c(1,1))














