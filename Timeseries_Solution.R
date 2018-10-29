
#----------------------------------TIME  SERIES ANALYSIS---------------------------------#
#--------------------------------------------------------------------------------------#

# Read the Source data file

superstore_data <- read.csv("Global Superstore.csv", header = T, sep = ',')

##------------BUSINESS UNDERSTANDING----------------#

# Packages Required 


library(forecast)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)

# Consider the relevant data for the analysis

superstore_data<- superstore_data[,c(3,8,13,19,20,22)]

superstore_data$Date <- as.Date(superstore_data$Order.Date, "%d-%m-%Y")
superstore_data$Date <- format(superstore_data$Date,"%y-%m")

# Checking the data 
head(superstore_data)


# As Mentioned in the problem statement-we need to analysis 21 buckets to find the best 5 in terms of profitability.
# Either we can go for creating 21 subset of the main dataset or we can use some in build function or (write a function ) 
# to perform the same job for each sunset with minimal codes 

# ################################Additional(Lengthy process)(Creating 21 subsets)()##################################

# For Consumer level
#superstore_Consumer_Africa = subset(superstore, Market==levels(superstore$Market)[1]&Segment== levels(superstore$Segment)[1])
#superstore_Consumer_APAC = subset(superstore, Market==levels(superstore$Market)[2]&Segment== levels(superstore$Segment)[1])
#superstore_Consumer_Canada = subset(superstore, Market==levels(superstore$Market)[3]&Segment== levels(superstore$Segment)[1])
#superstore_Consumer_EMEA = subset(superstore, Market==levels(superstore$Market)[4]&Segment== levels(superstore$Segment)[1]) 
#superstore_Consumer_EU = subset(superstore, Market==levels(superstore$Market)[5]&Segment== levels(superstore$Segment)[1])
#superstore_Consumer_LATAM = subset(superstore, Market==levels(superstore$Market)[6]&Segment== levels(superstore$Segment)[1])
#superstore_Consumer_US = subset(superstore, Market==levels(superstore$Market)[7]&Segment== levels(superstore$Segment)[1])

# For Corporate Level
#superstore_Corporate_Africa = subset(superstore, Market==levels(superstore$Market)[1]&Segment== levels(superstore$Segment)[2])
#superstore_Corporate_APAC = subset(superstore, Market==levels(superstore$Market)[2]&Segment== levels(superstore$Segment)[2])
#superstore_Corporate_Canada = subset(superstore, Market==levels(superstore$Market)[3]&Segment== levels(superstore$Segment)[2])
#superstore_Corporate_EMEA = subset(superstore, Market==levels(superstore$Market)[4]&Segment== levels(superstore$Segment)[2]) 
#superstore_Corporate_EU = subset(superstore, Market==levels(superstore$Market)[5]&Segment== levels(superstore$Segment)[2])
#superstore_Corporate_LATAM = subset(superstore, Market==levels(superstore$Market)[6]&Segment== levels(superstore$Segment)[2])
#superstore_Corporate_US = subset(superstore, Market==levels(superstore$Market)[7]&Segment== levels(superstore$Segment)[2])

# For Home_Office Level
#superstore_Home_Office_Africa = subset(superstore, Market==levels(superstore$Market)[1]&Segment== levels(superstore$Segment)[3])
#superstore_Home_Office_APAC = subset(superstore, Market==levels(superstore$Market)[2]&Segment== levels(superstore$Segment)[3])
#superstore_Home_Office_Canada = subset(superstore, Market==levels(superstore$Market)[3]&Segment== levels(superstore$Segment)[3])
#superstore_Home_Office_EMEA = subset(superstore, Market==levels(superstore$Market)[4]&Segment== levels(superstore$Segment)[3]) 
#superstore_Home_Office_EU = subset(superstore, Market==levels(superstore$Market)[5]&Segment== levels(superstore$Segment)[3])
#superstore_Home_Office_LATAM = subset(superstore, Market==levels(superstore$Market)[6]&Segment== levels(superstore$Segment)[3])
#superstore_Home_Office_US = subset(superstore, Market==levels(superstore$Market)[7]&Segment== levels(superstore$Segment)[3])

##########################################################################################################

# Let's combine the "Market" and"Segment" variable 

superstore_data$concatenate <- as.factor(paste(superstore_data$Market,superstore_data$Segment))

# Let's check the levels of concatenate variable

levels(superstore_data$concatenate)

#Let's first create a first subset of "Africa Consumer" variable

k1 <- subset(superstore_data,superstore_data$concatenate== "Africa Consumer")

k2 <- aggregate(cbind(Sales,Profit,Quantity)~ Date,k1,sum)

# Function for coefficient of variation calculation 

Store <- function(var_name){
  
  k1 <- subset(superstore_data,superstore_data$concatenate== var_name)
  
  k2 <- aggregate(cbind(Sales,Profit,Quantity)~ Date,k1,sum)
  
  k2$Month <- seq(1,nrow(k2),1)

 
    if(nrow(k2)==48){
    k2_in=k2[1:42,]
    k2_out=k2[43:48,]
    
    cov <- sd(k2_in$Profit)/mean(k2_in$Profit)
   
    }
  return(list(nrow(k2),cov))
}


empty_matrix <- matrix(0,21,2)


for(i in 1:21){
  
  if(Store(levels(superstore_data$concatenate)[1])[[1]]< 48){
    
  empty_matrix[i,1] <- levels(superstore_data$concatenate)[i]
  empty_matrix[i,2] <- "NA"
  
  } 
  
  if(Store(levels(superstore_data$concatenate)[i])[[1]]==48) {
    
    empty_matrix[i,1] <- levels(superstore_data$concatenate)[i]
    empty_matrix[i,2] <- Store(levels(superstore_data$concatenate)[i])[[2]]
    
  }
    
}

# Order the matrix 


empty_matrix <-data.frame(empty_matrix)

colnames(empty_matrix)<-c("segments_name","COV")

# Check the dataframe(7,8,9 rows are empty) Let's remove those

empty_matrix <- empty_matrix[-c(7:9),]

# Order the matrix
empty_matrix <- empty_matrix[order(empty_matrix$COV), ]

head(empty_matrix)

#segments_name       COV
#  APAC Consumer   0.60363
#  EU Consumer     0.65533

top2 <-as.character(empty_matrix$segments_name[1:2])

detach(package:dplyr)

#################### Sales Forecasting ####################
dfList_1 <- list()




#  APAC Consumer

j <- 1

#  Creating a time series object containing APAC consumer Sales

  k1 <- subset(superstore_data,superstore_data$concatenate==top2[j])
  k2 <- aggregate(cbind(Sales,Quantity)~ Date,k1,sum)
  Month <- seq(1,nrow(k2),1)
  k2 <- cbind(Month,k2)
  k2_in=k2[1:42,]
  k2_out=k2[43:48,]
  
  timeser <- ts(k2_in$Sales)
  
#Smoothing the series - Moving Average Smoothing
  
  w <-1
  
  smoothedseries <- filter(timeser,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#Smoothing left end of the time series
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  
  
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  n <- length(timeser)
  
  timevals <- k2_in[,1]

#Smoothing right end of the time series  

  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }

#Plot the smoothed time series
  
  plot(timeser,xlab= "Month",ylab=paste(top2[j]),main="Sales")
  lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
  
  smootheddf <- as.data.frame(cbind(timevals, as.vector(smoothedseries)))
  
  colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
  
  lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2) + Month , data=smootheddf)
  
  trend <- predict(lmfit, Month=timevals)
  
  lines(timevals, trend, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
    
  resi <- timeser - trend
  plot(resi, col='red')
  acf(resi)
  acf(resi, type="partial")
  armafit <- auto.arima(resi)
  
  tsdiag(armafit)
  armafit
  
#We'll check if the residual series is white noise
  
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
  
  timevals1 <- k2_out[,1]
  
  timevals1_1 <- as.data.frame(timevals1)
  
  trend1 <- predict(lmfit, timevals1_1) 
  
  fcast <- trend1[43:48]

#Now, let's compare our prediction with the actual values, using MAPE
  
  MAPE_reg <- accuracy(fcast,k2_out[,3])[5]
  
  MAPE_reg

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

  total_timeser <- ts(k2$Sales)
  pred <- ts(trend1)
  plot(total_timeser, col = "black")
  lines(pred, col = "red", lwd = 2)
  lines(pred[1:42],col="blue", lwd = 2)

#So, that was classical decomposition, now let's do an ARIMA fit
  
  autoarima <- auto.arima(timeser)
  autoarima
  tsdiag(autoarima)
  
  resi<- timeser - fitted(autoarima)
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  
  plot(autoarima$x, col="black")
  lines(fitted(autoarima), col="red")
  fcast1 <- predict(autoarima, n.ahead=6)

#Again, let's evaluate the model using MAPE
    
  MAPE_arima <- accuracy(fcast1$pred,k2_out[,3])[5]
  
  MAPE_arima
  
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
  
  auto_arima_pred <- c(fitted(autoarima),ts(fcast1$pred))
  plot(total_timeser, col = "black")
  lines(auto_arima_pred, col = "red", lwd = 2)
  lines(fitted(autoarima), col = "blue", lwd = 2)

  #  EU Consumer
  
  j <- 2
  
  k1 <- subset(superstore_data,superstore_data$concatenate==top2[j])
  k2 <- aggregate(cbind(Sales,Quantity)~ Date,k1,sum)
  Month <- seq(1,nrow(k2),1)
  k2 <- cbind(Month,k2)
  k2_in=k2[1:42,]
  k2_out=k2[43:48,]
  
  timeser <- ts(k2_in$Sales)
  
  
  w <-1
  
  smoothedseries <- filter(timeser,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  
  
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  n <- length(timeser)
  
  timevals <- k2_in[,1]
  
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  plot(timeser,xlab= "Month",ylab=paste(top2[j]),main="Sales")
  lines(smoothedseries, col="blue", lwd=2)
  
  smootheddf <- as.data.frame(cbind(timevals, as.vector(smoothedseries)))
  
  
  colnames(smootheddf) <- c('Month', 'Sales')
  
  lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2) + Month , data=smootheddf)
  
  trend <- predict(lmfit, Month=timevals)
  
  lines(timevals, trend, col='red', lwd=2)
  
  resi <- timeser - trend
  plot(resi, col='red')
  acf(resi)
  acf(resi, type="partial")
  armafit <- auto.arima(resi)
  
  tsdiag(armafit)
  armafit
  
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  
  timevals1 <- k2_out[,1]
  
  timevals1_1 <- as.data.frame(timevals1)
  
  trend1 <- predict(lmfit, timevals1_1) 
  
  fcast <- trend1[43:48]
  
  MAPE_reg <- accuracy(fcast,k2_out[,3])[5]
  
  MAPE_reg

  total_timeser <- ts(k2$Sales)
  pred <- ts(trend1)
  plot(total_timeser, col = "black")
  lines(pred, col = "red", lwd = 2)
  lines(pred[1:42],col="blue", lwd = 2)
  
  autoarima <- auto.arima(timeser)
  autoarima
  tsdiag(autoarima)
  
  resi<- timeser - fitted(autoarima)
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  
  plot(autoarima$x, col="black")
  lines(fitted(autoarima), col="red")
  fcast1 <- predict(autoarima, n.ahead=6)
  
  MAPE_arima <- accuracy(fcast1$pred,k2_out[,3])[5]
  
  MAPE_arima
  
  auto_arima_pred <- c(fitted(autoarima),ts(fcast1$pred))
  plot(total_timeser, col = "black")
  lines(auto_arima_pred, col = "red", lwd = 2)
  lines(fitted(autoarima), col = "blue", lwd = 2)
  
###################################################################### Sales end.

## Quantity Forecasting#########################################################

dfList_2 <- list()


  #  APAC Consumer
  
  j <- 1
  
  k1 <- subset(superstore_data,superstore_data$concatenate==top2[j])
  k2 <- aggregate(cbind(Sales,Quantity)~ Date,k1,sum)
  Month <- seq(1,nrow(k2),1)
  k2 <- cbind(Month,k2)
  k2_in=k2[1:42,]
  k2_out=k2[43:48,]
  
  timeser <- ts(k2_in$Quantity)
  
  
  w <-1
  
  smoothedseries <- filter(timeser,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  
  
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  n <- length(timeser)
  
  timevals <- k2_in[,1]
  
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  plot(timeser,xlab= "Month",ylab=paste(top2[j]),main="Quantity")
  lines(smoothedseries, col="blue", lwd=2)
  
  smootheddf <- as.data.frame(cbind(timevals, as.vector(smoothedseries)))
  
  
  colnames(smootheddf) <- c('Month', 'Sales')
  
  lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2) + Month , data=smootheddf)
  
  trend <- predict(lmfit, Month=timevals)
  
  lines(timevals, trend, col='red', lwd=2)
  
  resi <- timeser - trend
  plot(resi, col='red')
  acf(resi)
  acf(resi, type="partial")
  armafit <- auto.arima(resi)
  
  tsdiag(armafit)
  armafit
  
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  
  timevals1 <- k2_out[,1]
  
  timevals1_1 <- as.data.frame(timevals1)
  
  trend1 <- predict(lmfit, timevals1_1) 
  
  fcast <- trend1[43:48]
  
  MAPE_reg <- accuracy(fcast,k2_out[,4])[5]
  
  MAPE_reg
  
  total_timeser <- ts(k2$Quantity)
  pred <- ts(trend1)
  plot(total_timeser, col = "black")
  lines(pred, col = "red", lwd = 2)
  lines(pred[1:42],col="blue", lwd = 2)
  
  autoarima <- auto.arima(timeser)
  autoarima
  tsdiag(autoarima)
  
  resi<- timeser - fitted(autoarima)
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  
  plot(autoarima$x, col="black")
  lines(fitted(autoarima), col="red")
  fcast1 <- predict(autoarima, n.ahead=6)
  
  MAPE_arima <- accuracy(fcast1$pred,k2_out[,4])[5]
  
  MAPE_arima
  
  auto_arima_pred <- c(fitted(autoarima),ts(fcast1$pred))
  plot(total_timeser, col = "black")
  lines(auto_arima_pred, col = "red", lwd = 2)
  lines(fitted(autoarima), col = "blue", lwd = 2)
  
  #  EU Consumer
  
  j <- 2
  
  k1 <- subset(superstore_data,superstore_data$concatenate==top2[j])
  k2 <- aggregate(cbind(Sales,Quantity)~ Date,k1,sum)
  Month <- seq(1,nrow(k2),1)
  k2 <- cbind(Month,k2)
  k2_in=k2[1:42,]
  k2_out=k2[43:48,]
  
  timeser <- ts(k2_in$Quantity)
  
  
  w <-1
  
  smoothedseries <- filter(timeser,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  
  
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  n <- length(timeser)
  
  timevals <- k2_in[,1]
  
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  plot(timeser,xlab= "Month",ylab=paste(top2[j]),main="Quantity")
  lines(smoothedseries, col="blue", lwd=2)
  
  smootheddf <- as.data.frame(cbind(timevals, as.vector(smoothedseries)))
  
  
  colnames(smootheddf) <- c('Month', 'Sales')
  
  lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2) + Month , data=smootheddf)
  
  trend <- predict(lmfit, Month=timevals)
  
  lines(timevals, trend, col='red', lwd=2)
  
  resi <- timeser - trend
  plot(resi, col='red')
  acf(resi)
  acf(resi, type="partial")
  armafit <- auto.arima(resi)
  
  tsdiag(armafit)
  armafit
  
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  
  timevals1 <- k2_out[,1]
  
  timevals1_1 <- as.data.frame(timevals1)
  
  trend1 <- predict(lmfit, timevals1_1) 
  
  fcast <- trend1[43:48]
  
  MAPE_reg <- accuracy(fcast,k2_out[,4])[5]
  
  MAPE_reg
  
  total_timeser <- ts(k2$Quantity)
  pred <- ts(trend1)
  plot(total_timeser, col = "black")
  lines(pred, col = "red", lwd = 2)
  lines(pred[1:42],col="blue",lwd = 2)
  
  autoarima <- auto.arima(timeser)
  autoarima
  tsdiag(autoarima)
  
  resi<- timeser - fitted(autoarima)
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  
  plot(autoarima$x, col="black")
  lines(fitted(autoarima), col="red")
  fcast1 <- predict(autoarima, n.ahead=6)

  MAPE_arima <- accuracy(fcast1$pred,k2_out[,4])[5]
  
  MAPE_arima
  
  auto_arima_pred <- c(fitted(autoarima),ts(fcast1$pred))
  plot(total_timeser, col = "black")
  lines(auto_arima_pred, col = "red", lwd = 2)
  lines(fitted(autoarima), col = "blue", lwd = 2)
  
################################################################ Quantity End 