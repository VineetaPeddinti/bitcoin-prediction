library(imputeTS)
library(dplyr)
library(ggplot2)
library(lubridate) #Date Package used in ggplot
library(forecast)
library(tseries)#Dickey-Fuller Test

data <- read.csv("bitstamp.csv")

head(data)

str(data)

nrow(data)

data$Timestamp <- as.Date(as.POSIXct(data$Timestamp , origin="1970-01-01"))

data <- data[,c("Timestamp","Weighted_Price")]
head(data)

#Data from 2016 till the end--------for reference
Data <- data[data$Timestamp>="2016-01-01" ,]

sapply(data, function(x) sum(is.na(x)))

data <- Data
data <- data %>%
  group_by(Timestamp) %>%
  mutate(Weighted_Price = mean(Weighted_Price,na.rm=T)         
)

data <- unique(data)

head(data)

sapply(data, function(x) sum(is.na(x)))

data_train <- data[data$Timestamp>="2016-01-01" & data$Timestamp<="2019-07-20",]

data_test <-  data[data$Timestamp>"2019-07-20",]

head(data_train)

tail(data_train)

nrow(data_train)

head(data_test)

tail(data_test)

nrow(data_test)

ggplot(data = data_train, aes(x = Timestamp, y = Weighted_Price)) + 
  geom_line(color = "blue", size = 1)

#https://stackoverflow.com/questions/41206181/ggplot-multiple-years-on-same-plot-by-month
ggplot(data_train, aes(month(Timestamp, label=TRUE, abbr=TRUE), 
                Weighted_Price, group=factor(year(Timestamp)), colour=factor(year(Timestamp)))) +
  geom_line(size = 1) +
  geom_point() +
  labs(x="Month", colour="Year") +
  theme_classic()

data_train$sales_ma = ma(data_train$Weighted_Price, order=7) 
data_train$sales_ma30 = ma(data_train$Weighted_Price, order=30)
data_train$sales_ma365 = ma(data_train$Weighted_Price, order=365)

ggplot() +
  geom_line(data = data_train, aes(x = Timestamp, y = data_train$sales_ma, colour = "Weighted_Price")) +
  geom_line(data = data_train, aes(x = Timestamp, y = data_train$sales_ma30,   colour = "Monhtly Moving Average"))  +
  geom_line(data = data_train, aes(x = Timestamp, y = data_train$sales_ma365, colour = "Yearly Moving Average"))  +
  ylab('Store Sales')

count_ma = ts(na.omit(data_train$Weighted_Price), frequency=365.25, start = 2016)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
plot(count_ma)
plot(deseasonal_cnt)
grid (lty = 8, col = "red")

#Dickey-Fuller Test
adf.test(count_ma, alternative = "stationary")

Acf(count_ma, main='')

Pacf(count_ma, main='')

plot(count_ma)

count_d1 = diff(deseasonal_cnt, differences = 1)

Acf(count_d1,main='ACF Plot after differentiation')

Pacf(count_d1,main='PACF plot after differentiation')

plot(count_d1)

plot(count_d2)

#Dickey-Fuller Test
adf.test(count_d1, alternative = "stationary")

auto.arima(deseasonal_cnt, seasonal=FALSE,trace=TRUE)

#ARIMA
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='Seasonal Model Residuals')

#SARIMA
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE,trace=TRUE)
fit_w_seasonality

tsdisplay(residuals(fit_w_seasonality), lag.max=15, main='Seasonal Model Residuals')

spfinal.aic <- Inf
spfinal.order <- c(0,0,0)
for (i in 10:5) for (j in 10:5) {
  spcurrent.aic <- AIC(arima(deseasonal_cnt, order=c(i, 1, j)))
  if (spcurrent.aic < spfinal.aic) {
    spfinal.aic <- spcurrent.aic
    spfinal.order <- c(i, 1, j)
    spfinal.arma <- arima(deseasonal_cnt, order=spfinal.order)
  }
}

spfinal.order

fitm16 = arima(deseasonal_cnt, order=spfinal.order)
fitm16
tsdisplay(residuals(fitm16), lag.max=15, main='Model Residuals')

seas_fcast <- forecast(fitm16,h=23)
plot(seas_fcast)

pred <- predict(fitm16,n.ahead = 12)

ts.cont <- ts.plot(deseasonal_cnt,pred$pred,lty=c(1,3))

seas_fcast

x <-as.data.frame(seas_fcast)
x$time <- rownames(x)
x$time <- as.double(x$time)

ggplot() +
  geom_line(data = x, aes(x = time, y = `Hi 95`, colour = "Weighted_Price")) 

ggplot() +
geom_line(data = data_train,aes(x=Timestamp, y = Weighted_Price, colour = "train")) +
geom_line(data = data_test,aes(x=Timestamp, y = Weighted_Price, colour = "test")) +
geom_line(data = x, aes(x = Timestamp, y = `Point Forecast`, colour = "Forecast")) 

typeof(x$time)

x$time

nrow(x$time)

x$time <- as.Date(x$time)

x$Timestamp <- data_test$Timestamp

nrow(data_test)

x

row.names(x) <- NULL

ggplot()+
geom_line(data = data_test,aes(x=Timestamp, y = Weighted_Price, colour = "test")) +
geom_line(data = x, aes(x = Timestamp, y = `Point Forecast`, colour = "Forecast")) 


