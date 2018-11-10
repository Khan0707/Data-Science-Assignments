#library(dplyr)
library(tidyr)
library(forecast)
library(tseries)
library(zoo)
library(xts)
library(lubridate)
library(stats)
library(raster)
library(stats)


#Read data:
superstoredata<-read.csv("Global Superstore.csv",stringsAsFactors = F)
str(superstoredata)   #51290 obs. of  24 variables

#Checking for NA values:

sapply(superstoredata,function(x) sum(is.na(x)))

#Postal code column has blank values. More than 70% rows have blank postal values. Hence we CANNOT remove them.

#Checking for duplicate rows
sum(duplicated(superstoredata))

#Findind Different segments :
length(unique(superstoredata$Segment))  #3 Levels

length(unique(superstoredata$Market))  # 7 levels

#Only keeping required columns in the datatset

superstoredata<-superstoredata[c(3,8,13,16,19,20,21,22)]

#Coverting dates from character format to date format:
superstoredata$Order.Date<-parse_date_time(x=superstoredata$Order.Date,orders = "dmY")

superstoredata$OrderYear = year(superstoredata$Order.Date)
superstoredata$OrderMonth = month(superstoredata$Order.Date)


#Summing up Sales,Quantity & Profit values on month of the Order.Date for all combinations of Market and Segments.

superstoredata_agg<-aggregate(superstoredata[,c("Sales","Quantity","Profit")],
                              by=list(superstoredata$Market,superstoredata$Segment,
                                      superstoredata$OrderYear,superstoredata$OrderMonth)
                              ,FUN = sum)

colnames(superstoredata_agg)[1]<-c("Market")
colnames(superstoredata_agg)[2]<-c("Segment")

colnames(superstoredata_agg)[3]<-c("Year")
colnames(superstoredata_agg)[4]<-c("Month")

######## calculating sum of profit and variance in profit segmentwise and marketwise.

#aggregate (sum) profit for each month for each market segment

AggregatedProfit <-aggregate(superstoredata_agg[,c("Profit")],
                             by=list(superstoredata_agg$Market,superstoredata_agg$Segment)
                             ,FUN = sum) 
colnames(AggregatedProfit)[1]<-c("Market")
colnames(AggregatedProfit)[2]<-c("Segment")
colnames(AggregatedProfit)[3]<-c("Total_Profit")


#aggregate variance in profit for each month for each market segment

AggregatedVariance <-aggregate(superstoredata_agg[,c("Profit")],
                               by=list(superstoredata_agg$Market,superstoredata_agg$Segment)
                               ,FUN =cv) 

colnames(AggregatedVariance)[1]<-c("Market")
colnames(AggregatedVariance)[2]<-c("Segment")
colnames(AggregatedVariance)[3]<-c("Profit_cv")

#merging AggregatedVariance and AggregatedProfit

StoreFinal= merge(AggregatedProfit,AggregatedVariance)

###Getting the first two rows which has the highest profit
head(StoreFinal[order(StoreFinal$Total_Profit,decreasing = T),c(1:2)],2)

#Get the first two rows which has the least cv
head(StoreFinal[order(StoreFinal$Profit_cv),c(1:2)],2)

#Both have the same common Market Segment listed that is
# EU Consumer and 
# APAC Consumer

# hence APAC Consumer and EU consumer have the lower cv on profit and the highest profit across all the segments.
#Hence subsetting

Consumer_APAC<-subset(superstoredata_agg,superstoredata_agg$Market=="APAC" & superstoredata_agg$Segment=="Consumer")
Consumer_EU<-subset(superstoredata_agg,superstoredata_agg$Market=="EU" & superstoredata_agg$Segment=="Consumer")

#Generating unique sequence number required for timeseries based on month and year columns:

monthseq<-function(df){
  MonthCount <- df$Year*12+ df$Month
  Min_Month_Count <- min(MonthCount)
  df$Month_sequence  <- MonthCount - Min_Month_Count +1
  df <- df[order(df$Month_sequence),]
  return(df)
}
Consumer_APAC<-monthseq(Consumer_APAC)
Consumer_EU<-monthseq(Consumer_EU)

#Converting to timeseries:
Total_timeseries_APAC_Sales<-ts(Consumer_APAC$Sales)   ############DURING MAPE this is total_tseries_apac_sales



#******************APAC Sales Smoothening,Modeling,Prediction using Classical Decompostion and ARIMA:******
cols <- c("red", "blue","green")

labels <- c("Raw", "Smoothed","Predicted")

ylab1 <- c("Sales")

xlab1 <- c("Time")

title <- c("APAC SALES")


#Out of the 48 unique months, use the first 42 rows representing the earliest 42 months to train the model.
#And the rest 6 months to test the model.

cons_apac_sales_train<-Consumer_APAC[1:42,]
timeseries_APAC_Sales<-ts(cons_apac_sales_train$Sales)
plot(timeseries_APAC_Sales)


###using simple moving average method for smoothing:

width<-2
plot(timeseries_APAC_Sales, main=title, xlab = xlab1, 
     ylab = ylab1, col=cols[1])


smoothedseries <- stats::filter(timeseries_APAC_Sales, 
                                filter=rep(1/(2*width+1),(2*width+1)), 
                                method='convolution', sides=2)

n<-nrow(cons_apac_sales_train)

#Smoothing right end of the time series

diff <- smoothedseries [n-width] - smoothedseries [n-width-1]

for (i in seq(n-width+1, n)) {
  smoothedseries [i] <- smoothedseries [i-1] + diff
}

#Smoothing left end of the time series

diff <-smoothedseries [width+2] - smoothedseries [width+1]

for (i in seq(width,1,-1)) {
  smoothedseries [i] <- smoothedseries [i+1] - diff
}


lines(smoothedseries, col=cols[2], lwd=2)

legend("topleft", labels, col=cols, lwd=2,cex=0.5)


#Plot the smoothed time series

timevals_in <- cons_apac_sales_train$Month_sequence
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_APAC_Sales <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf_APAC_Sales) <- c('Month', 'Sales')


#Modelling and Prediction done for APAC Segment Sales using Classical Decompostion and ARIMA method:

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_APAC_Sales<- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                      + Month, data=smootheddf_APAC_Sales)

global_pred_APAC_Sales <- predict(lmfit_APAC_Sales)

summary(global_pred_APAC_Sales)

lines(timevals_in, global_pred_APAC_Sales, col='green', lwd=2)
#legend("topleft", labels, col=cols, lwd=2,cex=0.5)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_Sales <- timeseries_APAC_Sales-global_pred_APAC_Sales
plot(local_pred_APAC_Sales, col='black', type = "l")
acf(local_pred_APAC_Sales)
acf(local_pred_APAC_Sales, type="partial")
armafit_APAC_Sales <- auto.arima(local_pred_APAC_Sales)

tsdiag(armafit_APAC_Sales )
armafit_APAC_Sales     ####ARIMA(0,0,0) with zero mean 

#We'll check if the residual series is white noise

resi_APAC_Sales <- local_pred_APAC_Sales-fitted(armafit_APAC_Sales)

adf.test(resi_APAC_Sales,alternative = "stationary")   ####p=0.01 < 0.05 hence the timeseries  stationary

kpss.test(resi_APAC_Sales)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the next 6 months

outdata_apac_sales<-Consumer_APAC[43:48,]
timevals_apac_sales_out<-outdata_apac_sales$Month_sequence

global_pred_out <- predict(lmfit_APAC_Sales,data.frame(Month =timevals_apac_sales_out))

fcast_APAC_Sales <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_APAC_Sales <- accuracy(fcast_APAC_Sales,outdata_apac_sales[,5])[5]
MAPE_APAC_Sales


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_APAC_Sales <- c(ts(global_pred_APAC_Sales),ts(global_pred_out))
plot(Total_timeseries_APAC_Sales, col = "black")
lines(class_dec_pred_APAC_Sales, col = "red")

#########################################let's do an ARIMA fit to APAC_SALES

autoarima <- auto.arima(timeseries_APAC_Sales)
autoarima     #ARIMA(0,1,1)
##Coefficients:
#ma1
#-0.7559
##sigma^2 estimated as 174361555:  log likelihood=-447.11
#AIC=898.23   AICc=898.55   BIC=901.66

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeseries_APAC_Sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")   # p-value = 0.01

kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata_apac_sales[,5])[5]

MAPE_auto_arima    ###27.68952     


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Total_timeseries_APAC_Sales, col = "black")
lines(auto_arima_pred, col = "red")



##################################################APAC_Quantity######################################

Total_timeseries_APAC_Quantity<-ts(Consumer_APAC$Quantity)

#******************APAC Quantity Smoothening,Modeling,Prediction using Classical Decompostion and ARIMA:******
cols <- c("red", "blue","green")

labels <- c("Raw", "Smoothed","Predicted")

ylab1 <- c("Quantity")

xlab1 <- c("Time")

title <- c("APAC QUANTITY")


#Out of the 48 unique months, use the first 42 rows representing the earliest 42 months to train the model.
#And the rest 6 months to test the model.

cons_APAC_Quantity_train<-Consumer_APAC[1:42,]
timeseries_APAC_Quantity<-ts(cons_APAC_Quantity_train$Quantity)
plot(timeseries_APAC_Quantity)


###using simple moving average method for smoothing:

width<-2
plot(timeseries_APAC_Quantity, main=title, xlab = xlab1, 
     ylab = ylab1, col=cols[1])


smoothedseries <- stats::filter(timeseries_APAC_Quantity, 
                                filter=rep(1/(2*width+1),(2*width+1)), 
                                method='convolution', sides=2)

n<-nrow(cons_APAC_Quantity_train)

#Smoothing right end of the time series

diff <- smoothedseries [n-width] - smoothedseries [n-width-1]

for (i in seq(n-width+1, n)) {
  smoothedseries [i] <- smoothedseries [i-1] + diff
}

#Smoothing left end of the time series

diff <-smoothedseries [width+2] - smoothedseries [width+1]

for (i in seq(width,1,-1)) {
  smoothedseries [i] <- smoothedseries [i+1] - diff
}


lines(smoothedseries, col=cols[2], lwd=2)

legend("topleft",labels, col=cols, lwd=2,cex=0.5)


#Plot the smoothed time series

timevals_in <- cons_APAC_Quantity_train$Month_sequence
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_APAC_Quantity <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf_APAC_Quantity) <- c('Month', 'Quantity')


#Modelling and Prediction done for APAC Segment Quantity using Classical Decompostion and ARIMA method:

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_APAC_Quantity<- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                         + Month, data=smootheddf_APAC_Quantity)

global_pred_APAC_Quantity <- predict(lmfit_APAC_Quantity)

summary(global_pred_APAC_Quantity)

lines(timevals_in, global_pred_APAC_Quantity, col='green', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_Quantity <- timeseries_APAC_Quantity-global_pred_APAC_Quantity
plot(local_pred_APAC_Quantity, col='black', type = "l")
acf(local_pred_APAC_Quantity)
acf(local_pred_APAC_Quantity, type="partial")
armafit_APAC_Quantity <- auto.arima(local_pred_APAC_Quantity)

tsdiag(armafit_APAC_Quantity )
armafit_APAC_Quantity     ####ARIMA(0,0,0) with zero mean 

#We'll check if the residual series is white noise

resi_APAC_Quantity <- local_pred_APAC_Quantity-fitted(armafit_APAC_Quantity)

adf.test(resi_APAC_Quantity,alternative = "stationary")   ####p=0.01 < 0.05 hence the timeseries  stationary

kpss.test(resi_APAC_Quantity)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the next 6 months

outdata_APAC_Quantity<-Consumer_APAC[43:48,]
timevals_APAC_Quantity_out<-outdata_APAC_Quantity$Month_sequence

global_pred_out <- predict(lmfit_APAC_Quantity,data.frame(Month =timevals_APAC_Quantity_out))

fcast_APAC_Quantity <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_APAC_Quantity <- accuracy(fcast_APAC_Quantity,outdata_APAC_Quantity[,6])[5]
MAPE_APAC_Quantity


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_APAC_Quantity <- c(ts(global_pred_APAC_Quantity),ts(global_pred_out))
plot(Total_timeseries_APAC_Quantity, col = "black")
lines(class_dec_pred_APAC_Quantity, col = "red")

#########################################let's do an ARIMA fit to APAC_QUANTITY

autoarima <- auto.arima(timeseries_APAC_Quantity)
autoarima     #ARIMA(0,1,0) 

#sigma^2 estimated as 25366:  log likelihood=-266.07
#AIC=534.14   AICc=534.24   BIC=535.85

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeseries_APAC_Quantity - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")   # p-value = 0.01

kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata_APAC_Quantity[,6])[5]

MAPE_auto_arima    ###26.24458    


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Total_timeseries_APAC_Quantity, col = "black")
lines(auto_arima_pred, col = "red")





#******************EU Sales Smoothening,Modeling,Prediction using Classical Decompostion and ARIMA:******
Total_timeseries_EU_Sales<-ts(Consumer_EU$Sales)

cols <- c("red", "blue","green")

labels <- c("Raw", "Smoothed","Predicted")

ylab1 <- c("Sales")

xlab1 <- c("Time")

title <- c("EU SALES")


#Out of the 48 unique months, use the first 42 rows representing the earliest 42 months to train the model.
#And the rest 6 months to test the model.

cons_EU_sales_train<-Consumer_EU[1:42,]
timeseries_EU_Sales<-ts(cons_EU_sales_train$Sales)
plot(timeseries_EU_Sales)


###using simple moving average method for smoothing:

width<-2
plot(timeseries_EU_Sales, main=title, xlab = xlab1, 
     ylab = ylab1, col=cols[1])


smoothedseries <- stats::filter(timeseries_EU_Sales, 
                                filter=rep(1/(2*width+1),(2*width+1)), 
                                method='convolution', sides=2)

n<-nrow(cons_EU_sales_train)

#Smoothing right end of the time series

diff <- smoothedseries [n-width] - smoothedseries [n-width-1]

for (i in seq(n-width+1, n)) {
  smoothedseries [i] <- smoothedseries [i-1] + diff
}

#Smoothing left end of the time series

diff <-smoothedseries [width+2] - smoothedseries [width+1]

for (i in seq(width,1,-1)) {
  smoothedseries [i] <- smoothedseries [i+1] - diff
}


lines(smoothedseries, col=cols[2], lwd=2)

legend("topleft", labels, col=cols, lwd=2,cex=0.5)


#Plot the smoothed time series

timevals_in <- cons_EU_sales_train$Month_sequence
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_EU_Sales <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf_EU_Sales) <- c('Month', 'Sales')


#Modelling and Prediction done for EU Segment Sales using Classical Decompostion and ARIMA method:

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EU_Sales<- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                    + Month, data=smootheddf_EU_Sales)

global_pred_EU_Sales <- predict(lmfit_EU_Sales)

summary(global_pred_EU_Sales)

lines(timevals_in, global_pred_EU_Sales, col='green', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_Sales <- timeseries_EU_Sales-global_pred_EU_Sales

plot(local_pred_EU_Sales, col='black', type = "l")
acf(local_pred_EU_Sales)
acf(local_pred_EU_Sales, type="partial")
armafit_EU_Sales <- auto.arima(local_pred_EU_Sales)

tsdiag(armafit_EU_Sales )
armafit_EU_Sales     ####ARIMA(0,0,0) with zero mean 

#We'll check if the residual series is white noise

resi_EU_Sales <- local_pred_EU_Sales-fitted(armafit_EU_Sales)

adf.test(resi_EU_Sales,alternative = "stationary")   ####p=0.01 < 0.05 hence the timeseries  stationary

kpss.test(resi_EU_Sales)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the next 6 months

outdata_EU_sales<-Consumer_EU[43:48,]
timevals_EU_sales_out<-outdata_EU_sales$Month_sequence

global_pred_out <- predict(lmfit_EU_Sales,data.frame(Month =timevals_EU_sales_out))

fcast_EU_Sales <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_EU_Sales <- accuracy(fcast_EU_Sales,outdata_EU_sales[,5])[5]
MAPE_EU_Sales
###28.27462

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_EU_Sales <- c(ts(global_pred_EU_Sales),ts(global_pred_out))
plot(Total_timeseries_EU_Sales, col = "black")
lines(class_dec_pred_EU_Sales, col = "red")

#########################################let's do an ARIMA fit to EU_SALES

autoarima <- auto.arima(timeseries_EU_Sales)
autoarima     #ARIMA(2,1,0) 

#Coefficients:
#ar1      ar2
##s.e.   0.1346   0.1310

#sigma^2 estimated as 168564623:  log likelihood=-445.84
#AIC=897.67   AICc=898.32   BIC=902.81

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeseries_EU_Sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")   # p-value = 0.01

kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata_EU_sales[,5])[5]

MAPE_auto_arima    ###28.9226   


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Total_timeseries_EU_Sales, col = "black")
lines(auto_arima_pred, col = "red")

#*************************************################################***********************************************



Total_timeseries_EU_Quantity<-ts(Consumer_EU$Quantity)

#******************EU Quantity Smoothening,Modeling,Prediction using Classical Decompostion and ARIMA:******
cols <- c("red", "blue","green")

labels <- c("Raw", "Smoothed","Predicted")

ylab1 <- c("Quantity")

xlab1 <- c("Time")

title <- c("EU QUANTITY")


#Out of the 48 unique months, use the first 42 rows representing the earliest 42 months to train the model.
#And the rest 6 months to test the model.

cons_EU_Quantity_train<-Consumer_EU[1:42,]
timeseries_EU_Quantity<-ts(cons_EU_Quantity_train$Quantity)
plot(timeseries_EU_Quantity)


###using simple moving average method for smoothing:

width<-2
plot(timeseries_EU_Quantity, main=title, xlab = xlab1, 
     ylab = ylab1, col=cols[1])


smoothedseries <- stats::filter(timeseries_EU_Quantity, 
                                filter=rep(1/(2*width+1),(2*width+1)), 
                                method='convolution', sides=2)

n<-nrow(cons_EU_Quantity_train)

#Smoothing right end of the time series

diff <- smoothedseries [n-width] - smoothedseries [n-width-1]

for (i in seq(n-width+1, n)) {
  smoothedseries [i] <- smoothedseries [i-1] + diff
}

#Smoothing left end of the time series

diff <-smoothedseries [width+2] - smoothedseries [width+1]

for (i in seq(width,1,-1)) {
  smoothedseries [i] <- smoothedseries [i+1] - diff
}


lines(smoothedseries, col=cols[2], lwd=2)

legend("topleft", labels, col=cols, lwd=2,cex=0.55)


#Plot the smoothed time series

timevals_in <- cons_EU_Quantity_train$Month_sequence
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_EU_Quantity <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf_EU_Quantity) <- c('Month', 'Quantity')


#Modelling and Prediction done for EU Segment Quantity using Classical Decompostion and ARIMA method:

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EU_Quantity<- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                       + Month, data=smootheddf_EU_Quantity)

global_pred_EU_Quantity <- predict(lmfit_EU_Quantity)

summary(global_pred_EU_Quantity)

lines(timevals_in, global_pred_EU_Quantity, col='green', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_Quantity <- timeseries_EU_Quantity-global_pred_EU_Quantity
plot(local_pred_EU_Quantity, col='black', type = "l")
acf(local_pred_EU_Quantity)
acf(local_pred_EU_Quantity, type="partial")
armafit_EU_Quantity <- auto.arima(local_pred_EU_Quantity)

tsdiag(armafit_EU_Quantity )
armafit_EU_Quantity     ####ARIMA(2,0,0) with zero mean 

#We'll check if the residual series is white noise

resi_EU_Quantity <- local_pred_EU_Quantity-fitted(armafit_EU_Quantity)

adf.test(resi_EU_Quantity,alternative = "stationary")   ####p=0.01 < 0.05 hence the timeseries  stationary

kpss.test(resi_EU_Quantity)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the next 6 months

outdata_EU_Quantity<-Consumer_EU[43:48,]
timevals_EU_Quantity_out<-outdata_EU_Quantity$Month_sequence

global_pred_out <- predict(lmfit_EU_Quantity,data.frame(Month =timevals_EU_Quantity_out))

fcast_EU_Quantity <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_EU_Quantity <- accuracy(fcast_EU_Quantity,outdata_EU_Quantity[,6])[5]
MAPE_EU_Quantity
#######31.39889

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_EU_Quantity <- c(ts(global_pred_EU_Quantity),ts(global_pred_out))
plot(Total_timeseries_EU_Quantity, col = "black")
lines(class_dec_pred_EU_Quantity, col = "red")

#########################################let's do an ARIMA fit to EU_QUANTITY

autoarima <- auto.arima(timeseries_EU_Quantity)
autoarima     #ARIMA(2,1,0) 

#Coefficients:
#ar1      ar2
#-0.7359  -0.5879
#s.e.   0.1224   0.1185

#sigma^2 estimated as 21185:  log likelihood=-261.9
#AIC=529.8   AICc=530.44   BIC=534.94

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeseries_EU_Quantity - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")   # p-value = 0.04521

kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata_EU_Quantity[,6])[5]

MAPE_auto_arima    ###30.13319    


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Total_timeseries_EU_Quantity, col = "black")
lines(auto_arima_pred, col = "red")


#It is logical to predict from month 49 to 54. We are bulding model using data for 48 months and 
#hence we reserved 42 rows for model training and 6 rowas for testing accuracy of the model using MAPE. 
#Once you have chosen the best model to use, it is logical to use it to forecast sales and quantity for 
#the next 6 months. This is one of the aim of the case study that the model should be used to plan for 6 #
#in future.  
