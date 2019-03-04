
library(dplyr)
require(graphics)
library(forecast)

superstore <- read.csv("Global Superstore.csv", stringsAsFactors = FALSE)
unique(superstore$Segment)
unique(superstore$Market)


typeof(superstore$Order.Date)

# converting order.date into POSIX format AND extract order month from it
superstore$order.month <- as.numeric(format(strptime(superstore$Order.Date,format = "%d-%m-%Y"),"%Y%m"))
typeof(superstore$order.month)


# Aggregating on Order Month for each of these 21 segements

For_aggregate_superstore <- select(superstore,Segment,Market,order.month,Sales,Profit,Quantity)
Aggregate_superstore <- aggregate(For_aggregate_superstore[-1:-3],by = list(For_aggregate_superstore$Segment,For_aggregate_superstore$Market,For_aggregate_superstore$order.month) ,FUN = sum,na.rm = T)
names(Aggregate_superstore)[1:3] <- c("Segment","Market","OpenMonth")

sum(Aggregate_superstore$Sales)
sum(Aggregate_superstore$Profit)
sum(Aggregate_superstore$Quantity)

sum(superstore$Sales)
sum(superstore$Profit)
sum(superstore$Quantity)

# to find the most profitable 2 segements
For_Aggregate_superstore_segement <- Aggregate_superstore[-3] 
Aggregate_superstore_segement <- aggregate(For_Aggregate_superstore_segement[-1:-2],by = list(For_Aggregate_superstore_segement$Segment,For_Aggregate_superstore_segement$Market) ,FUN = sum,na.rm = T)
names(Aggregate_superstore_segement)[1:2] <- c("Segment","Market")
Aggregate_superstore_segement_top2 <- head(Aggregate_superstore_segement[order(Aggregate_superstore_segement$Profit,decreasing = T),],n=2)

# Consumer APAC and Consumer EU are the 2 most profitable segements
# Consumer APAC profit  = 222817
# Consumer EU profit  = 188687

# to find the most consistently profitable 2 segements

# split aggegrate data into 21 segements

Aggregate_superstore_US_cons <- filter(Aggregate_superstore,Segment =="Consumer" & Market == "US" )
Aggregate_superstore_US_corp <- filter(Aggregate_superstore,Segment =="Corporate" & Market == "US" )
Aggregate_superstore_US_Hoff <- filter(Aggregate_superstore,Segment =="Home Office" & Market == "US" )


Aggregate_superstore_APAC_cons <- filter(Aggregate_superstore,Segment =="Consumer" & Market == "APAC" )
Aggregate_superstore_APAC_corp <- filter(Aggregate_superstore,Segment =="Corporate" & Market == "APAC" )
Aggregate_superstore_APAC_Hoff <- filter(Aggregate_superstore,Segment =="Home Office" & Market == "APAC" )


Aggregate_superstore_EU_cons <- filter(Aggregate_superstore,Segment =="Consumer" & Market == "EU" )
Aggregate_superstore_EU_corp <- filter(Aggregate_superstore,Segment =="Corporate" & Market == "EU" )
Aggregate_superstore_EU_Hoff <- filter(Aggregate_superstore,Segment =="Home Office" & Market == "EU" )


Aggregate_superstore_Africa_cons <- filter(Aggregate_superstore,Segment =="Consumer" & Market == "Africa" )
Aggregate_superstore_Africa_corp <- filter(Aggregate_superstore,Segment =="Corporate" & Market == "Africa" )
Aggregate_superstore_Africa_Hoff <- filter(Aggregate_superstore,Segment =="Home Office" & Market == "Africa" )


Aggregate_superstore_EMEA_cons <- filter(Aggregate_superstore,Segment =="Consumer" & Market == "EMEA" )
Aggregate_superstore_EMEA_corp <- filter(Aggregate_superstore,Segment =="Corporate" & Market == "EMEA" )
Aggregate_superstore_EMEA_Hoff <- filter(Aggregate_superstore,Segment =="Home Office" & Market == "EMEA" )



Aggregate_superstore_LATAM_cons <- filter(Aggregate_superstore,Segment =="Consumer" & Market == "LATAM" )
Aggregate_superstore_LATAM_corp <- filter(Aggregate_superstore,Segment =="Corporate" & Market == "LATAM" )
Aggregate_superstore_LATAM_Hoff <- filter(Aggregate_superstore,Segment =="Home Office" & Market == "LATAM" )


Aggregate_superstore_Canada_cons <- filter(Aggregate_superstore,Segment =="Consumer" & Market == "Canada" )
Aggregate_superstore_Canada_corp <- filter(Aggregate_superstore,Segment =="Corporate" & Market == "Canada" )
Aggregate_superstore_Canada_Hoff <- filter(Aggregate_superstore,Segment =="Home Office" & Market == "Canada" )


# coefficient of variation calculations for the segements

COV_US_cons <- (sqrt(mean(Aggregate_superstore_US_cons$Profit^2)-mean(Aggregate_superstore_US_cons$Profit)^2))/mean(Aggregate_superstore_US_cons$Profit)
COV_US_corp <- (sqrt(mean(Aggregate_superstore_US_corp$Profit^2)-mean(Aggregate_superstore_US_corp$Profit)^2))/mean(Aggregate_superstore_US_corp$Profit)
COV_US_Hoff <- (sqrt(mean(Aggregate_superstore_US_Hoff$Profit^2)-mean(Aggregate_superstore_US_Hoff$Profit)^2))/mean(Aggregate_superstore_US_Hoff$Profit)
COV_US_cons
COV_US_corp
COV_US_Hoff

COV_EU_cons <- (sqrt(mean(Aggregate_superstore_EU_cons$Profit^2)-mean(Aggregate_superstore_EU_cons$Profit)^2))/mean(Aggregate_superstore_EU_cons$Profit)
COV_EU_corp <- (sqrt(mean(Aggregate_superstore_EU_corp$Profit^2)-mean(Aggregate_superstore_EU_corp$Profit)^2))/mean(Aggregate_superstore_EU_corp$Profit)
COV_EU_Hoff <- (sqrt(mean(Aggregate_superstore_EU_Hoff$Profit^2)-mean(Aggregate_superstore_EU_Hoff$Profit)^2))/mean(Aggregate_superstore_EU_Hoff$Profit)
COV_EU_cons
COV_EU_corp
COV_EU_Hoff


COV_Africa_cons <- (sqrt(mean(Aggregate_superstore_Africa_cons$Profit^2)-mean(Aggregate_superstore_Africa_cons$Profit)^2))/mean(Aggregate_superstore_Africa_cons$Profit)
COV_Africa_corp <- (sqrt(mean(Aggregate_superstore_Africa_corp$Profit^2)-mean(Aggregate_superstore_Africa_corp$Profit)^2))/mean(Aggregate_superstore_Africa_corp$Profit)
COV_Africa_Hoff <- (sqrt(mean(Aggregate_superstore_Africa_Hoff$Profit^2)-mean(Aggregate_superstore_Africa_Hoff$Profit)^2))/mean(Aggregate_superstore_Africa_Hoff$Profit)
COV_Africa_cons
COV_Africa_corp
COV_Africa_Hoff



COV_LATAM_cons <- (sqrt(mean(Aggregate_superstore_LATAM_cons$Profit^2)-mean(Aggregate_superstore_LATAM_cons$Profit)^2))/mean(Aggregate_superstore_LATAM_cons$Profit)
COV_LATAM_corp <- (sqrt(mean(Aggregate_superstore_LATAM_corp$Profit^2)-mean(Aggregate_superstore_LATAM_corp$Profit)^2))/mean(Aggregate_superstore_LATAM_corp$Profit)
COV_LATAM_Hoff <- (sqrt(mean(Aggregate_superstore_LATAM_Hoff$Profit^2)-mean(Aggregate_superstore_LATAM_Hoff$Profit)^2))/mean(Aggregate_superstore_LATAM_Hoff$Profit)
COV_LATAM_cons
COV_LATAM_corp
COV_LATAM_Hoff

COV_APAC_cons <- (sqrt(mean(Aggregate_superstore_APAC_cons$Profit^2)-mean(Aggregate_superstore_APAC_cons$Profit)^2))/mean(Aggregate_superstore_APAC_cons$Profit)
COV_APAC_corp <- (sqrt(mean(Aggregate_superstore_APAC_corp$Profit^2)-mean(Aggregate_superstore_APAC_corp$Profit)^2))/mean(Aggregate_superstore_APAC_corp$Profit)
COV_APAC_Hoff <- (sqrt(mean(Aggregate_superstore_APAC_Hoff$Profit^2)-mean(Aggregate_superstore_APAC_Hoff$Profit)^2))/mean(Aggregate_superstore_APAC_Hoff$Profit)
COV_APAC_cons
COV_APAC_corp
COV_APAC_Hoff


COV_Canada_cons <- (sqrt(mean(Aggregate_superstore_Canada_cons$Profit^2)-mean(Aggregate_superstore_Canada_cons$Profit)^2))/mean(Aggregate_superstore_Canada_cons$Profit)
COV_Canada_corp <- (sqrt(mean(Aggregate_superstore_Canada_corp$Profit^2)-mean(Aggregate_superstore_Canada_corp$Profit)^2))/mean(Aggregate_superstore_Canada_corp$Profit)
COV_Canada_Hoff <- (sqrt(mean(Aggregate_superstore_Canada_Hoff$Profit^2)-mean(Aggregate_superstore_Canada_Hoff$Profit)^2))/mean(Aggregate_superstore_Canada_Hoff$Profit)
COV_Canada_cons
COV_Canada_corp
COV_Canada_Hoff



COV_EMEA_cons <- (sqrt(mean(Aggregate_superstore_EMEA_cons$Profit^2)-mean(Aggregate_superstore_EMEA_cons$Profit)^2))/mean(Aggregate_superstore_EMEA_cons$Profit)
COV_EMEA_corp <- (sqrt(mean(Aggregate_superstore_EMEA_corp$Profit^2)-mean(Aggregate_superstore_EMEA_corp$Profit)^2))/mean(Aggregate_superstore_EMEA_corp$Profit)
COV_EMEA_Hoff <- (sqrt(mean(Aggregate_superstore_EMEA_Hoff$Profit^2)-mean(Aggregate_superstore_EMEA_Hoff$Profit)^2))/mean(Aggregate_superstore_EMEA_Hoff$Profit)
COV_EMEA_cons
COV_EMEA_corp
COV_EMEA_Hoff


COV_values <- c(
  COV_Canada_cons
  ,COV_Canada_corp
  ,COV_Canada_Hoff
  ,COV_APAC_cons
  ,COV_APAC_corp
  ,COV_APAC_Hoff
  ,COV_LATAM_cons
  ,COV_LATAM_corp
  ,COV_LATAM_Hoff
  ,COV_Africa_cons
  ,COV_Africa_corp
  ,COV_Africa_Hoff
  ,COV_EU_cons
  ,COV_EU_corp
  ,COV_EU_Hoff
  ,COV_US_cons
  ,COV_US_corp
  ,COV_US_Hoff
  ,COV_EMEA_cons
  ,COV_EMEA_corp
  ,COV_EMEA_Hoff)

COV_Market_Segement <- c(
  "COV_Canada_cons"
  ,"COV_Canada_corp"
  ,"COV_Canada_Hoff"
  ,"COV_APAC_cons"
  ,"COV_APAC_corp"
  ,"COV_APAC_Hoff"
  ,"COV_LATAM_cons"
  ,"COV_LATAM_corp"
  ,"COV_LATAM_Hoff"
  ,"COV_Africa_cons"
  ,"COV_Africa_corp"
  ,"COV_Africa_Hoff"
  ,"COV_EU_cons"
  ,"COV_EU_corp"
  ,"COV_EU_Hoff"
  ,"COV_US_cons"
  ,"COV_US_corp"
  ,"COV_US_Hoff"
  ,"COV_EMEA_cons"
  ,"COV_EMEA_corp"
  ,"COV_EMEA_Hoff")
COV<-cbind.data.frame(COV_Market_Segement,COV_values)

Least_COV_top2 <- head(COV[order(COV$COV_values,decreasing = F),],n=2)
Least_COV_top2

# Consumer segement in EU and Consumer segement in APAC are the most profitable and have the least coeeficient of variation.
# Thus selecting these 2 for the prediction

Consumer_EU <- Aggregate_superstore_EU_cons
Consumer_APAC <- Aggregate_superstore_APAC_cons


#################### Model Building for Consumer EU sales prediction ##############


# Classical decomposition:

# Smoothing :

# Moving average smoothing: 

ts_Consumer_EU_sales <- ts(Consumer_EU[1:42,4])
plot(ts_Consumer_EU_sales,main="Indata Consumer_EU_Sales ", xlab = "Months", ylab = "Sales" )

# Width 3 fits best after many trials

w <- 3
smoothedseries <- stats::filter(ts_Consumer_EU_sales, 
                         filter=rep(1/(w),(w)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#lines(smoothedseries, col="blue")

#Smoothing right end of the time series

n <- length(ts_Consumer_EU_sales)
diff <- smoothedseries[n-w+1] - smoothedseries[n-w]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#lines(smoothedseries, col="red")

#Plot the smoothed time series


lines(smoothedseries,main="Consumer_EU_Sales", xlab = "Months", ylab = "Sales" , col="blue", lwd=1)
legend("topleft",c("Raw","Smoothed"), col=c("Black","Blue"), lwd=1)

# Converting smoothed time series into a dataframe:
timevals <<-c(1:42)

Cons_EU_sales_df <- as.data.frame(cbind(timevals,as.vector(smoothedseries)))
colnames(Cons_EU_sales_df) <- c("Month","Sales")


# Global prediction model building
#Mape 56#lmfit_Cons_EU_sales <- lm(Sales ~ sin(0.5*Month)*poly(Month,2)+cos(0.5*Month)*poly(Month,2)+Month, data = Cons_EU_sales_df)
#Mape 25#lmfit_Cons_EU_sales <- lm(Sales ~ sin(0.5*Month)+cos(0.5*Month)+Month, data = Cons_EU_sales_df)
#Mape 32#lmfit_Cons_EU_sales <- lm(Sales ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,2)+Month, data = Cons_EU_sales_df)
#Mape 24#lmfit_Cons_EU_sales <- lm(Sales ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,5)+Month, data = Cons_EU_sales_df)
#Mape 20#lmfit_Cons_EU_sales <- lm(Sales ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,4)+Month, data = Cons_EU_sales_df)


lmfit_Cons_EU_sales <- lm(Sales ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,4)+Month, data = Cons_EU_sales_df)
global_pred_Cons_EU_sales <- predict(lmfit_Cons_EU_sales, Month=timevals )
summary(global_pred_Cons_EU_sales)

lines(global_pred_Cons_EU_sales,main="Consumer_EU_Sales", xlab = "Months", ylab = "Sales" ,col='magenta',lwd=2)
legend("topleft",c("Raw","Smoothed","Glob_Pred"), col=c("Black","Blue","Magenta"), lwd=1)


#For locally predictable series

local_pred_Cons_EU_sales <- ts_Consumer_EU_sales-global_pred_Cons_EU_sales
plot(local_pred_Cons_EU_sales,main="Local_TS_Consumer_EU_Sales", xlab = "Months", ylab = "Sales" , col='red', type = "l")
acf(local_pred_Cons_EU_sales)
acf(local_pred_Cons_EU_sales, type="partial")
armafit_Cons_EU_sales <- auto.arima(local_pred_Cons_EU_sales)


par(mar=c(5,2,3,3))
tsdiag(armafit_Cons_EU_sales)

armafit_Cons_EU_sales
plot(armafit_Cons_EU_sales$x,main="Local_Pred_Consumer_EU_Sales", xlab = "Months", ylab = "Sales" )
lines(fitted(armafit_Cons_EU_sales),col='magenta',lwd=2)



resi_Cons_EU_sales <- local_pred_Cons_EU_sales-fitted(armafit_Cons_EU_sales)
lines(resi_Cons_EU_sales,col='red',lwd=1)
legend("topleft",c("Raw","ARMA_Local","Resi"), col=c("Black","Magenta","Red"), lwd=2)


#Check if the residual series is white noise

adf.test(resi_Cons_EU_sales,alternative = "stationary")
kpss.test(resi_Cons_EU_sales)

#Model evaluation using MAPE
#Prediction for the last 6 months

outdata_Cons_EU_sales <- Consumer_EU[43:48,4]
timevals_out <- c(43:48)

# Predict the local ARMA values for 6 obs :

Local_pred_auto_arima_Cons_EU_sales <- predict(armafit_Cons_EU_sales, n.ahead = 6)

GLobal_pred_out_Cons_EU_sales <- predict(lmfit_Cons_EU_sales,data.frame(Month=timevals_out) )


plot(GLobal_pred_out_Cons_EU_sales,main="Outdata GLobal_Pred_Consumer_EU_Sales", xlab = "Outdata Months", ylab = "Sales" )
plot(Local_pred_auto_arima_Cons_EU_sales$pred,main="Outdata Local_Pred_Consumer_EU_Sales", xlab = "Outdata Months", ylab = "Sales" )

fcast_Cons_EU_sales <- GLobal_pred_out_Cons_EU_sales+Local_pred_auto_arima_Cons_EU_sales$pred
plot(fcast_Cons_EU_sales,main="Outdata Glob+Local Pred_Consumer_EU_Sales", xlab = "Months", ylab = "Sales" )
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_Cons_EU_sales <- accuracy(fcast_Cons_EU_sales,outdata_Cons_EU_sales)[5]
MAPE_class_dec_Cons_EU_sales

# MAPE = 20.83%


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit


class_dec_pred_Cons_EU_sales <- c(ts(global_pred_Cons_EU_sales)[1:42],ts(fcast_Cons_EU_sales))
plot(ts(Consumer_EU[,4]),main="Final Classical Decoposition Consumer_EU_Sales", xlab = "Months", ylab = "Sales" , col = "black")
lines(class_dec_pred_Cons_EU_sales, col = "red")
legend("topleft",c("Raw","Final_Pred"), col=c("Black","Red"), lwd=1)



#ARIMA fit for Consumer EU sales

autoarima_Cons_EU_sales <- auto.arima(ts_Consumer_EU_sales)
autoarima_Cons_EU_sales
tsdiag(autoarima_Cons_EU_sales)
plot(autoarima_Cons_EU_sales$x,main="Auto ARIMA Consumer_EU_Sales", xlab = "Months", ylab = "Sales", col="black")
lines(fitted(autoarima_Cons_EU_sales), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_Cons_EU_sales <- ts_Consumer_EU_sales - fitted(autoarima_Cons_EU_sales)

adf.test(resi_auto_arima_Cons_EU_sales,alternative = "stationary")
kpss.test(resi_auto_arima_Cons_EU_sales)

#Also, let's evaluate the model using MAPE
full_auto_arima_Cons_EU_sales <- predict(autoarima_Cons_EU_sales, n.ahead = 6)


MAPE_auto_arima_Cons_EU_sales <- accuracy(full_auto_arima_Cons_EU_sales$pred,outdata_Cons_EU_sales)[5]
MAPE_auto_arima_Cons_EU_sales

# MAPE = 28.92%

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_Cons_EU_sales <- c(fitted(autoarima_Cons_EU_sales),ts(full_auto_arima_Cons_EU_sales$pred))
plot(ts(Consumer_EU[,4]),main="Final Auto ARIMA Consumer_EU_Sales", xlab = "Months", ylab = "Sales", col = "black")
lines(auto_arima_pred_Cons_EU_sales, col = "red")
legend("topleft",c("Raw","Final_Pred"), col=c("Black","Red"), lwd=1)




######################################################################################

################# Consumer  EU  Quantity predictions ################################



# Classical decomposition:

# Smoothing :

# Moving average smoothing: 

ts_Consumer_EU_quantity <- ts(Consumer_EU[1:42,6])
plot(ts_Consumer_EU_quantity,main="Indata Consumer_EU_quantity ", xlab = "Months", ylab = "quantity" )

# Width 3 fits best after many trials

w <- 3
smoothedseries <- stats::filter(ts_Consumer_EU_quantity, 
                                filter=rep(1/(w),(w)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#lines(smoothedseries, col="blue")

#Smoothing right end of the time series

n <- length(ts_Consumer_EU_quantity)
diff <- smoothedseries[n-w+1] - smoothedseries[n-w]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#lines(smoothedseries, col="red")

#Plot the smoothed time series


lines(smoothedseries,main="Consumer_EU_quantity", xlab = "Months", ylab = "quantity" , col="blue", lwd=1)
legend("topleft",c("Raw","Smoothed"), col=c("Black","Blue"), lwd=1)

# Converting smoothed time series into a dataframe:
timevals <<-c(1:42)

Cons_EU_quantity_df <- as.data.frame(cbind(timevals,as.vector(smoothedseries)))
colnames(Cons_EU_quantity_df) <- c("Month","quantity")


# Global prediction model building
#Mape 22#lmfit_Cons_EU_quantity <- lm(quantity ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,4)+Month, data = Cons_EU_quantity_df)
#Mape 26#lmfit_Cons_EU_quantity <- lm(quantity ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,5)+Month, data = Cons_EU_quantity_df)
#Mape 25#lmfit_Cons_EU_quantity <- lm(quantity ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,3)+Month, data = Cons_EU_quantity_df)


lmfit_Cons_EU_quantity <- lm(quantity ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,4), data = Cons_EU_quantity_df)
global_pred_Cons_EU_quantity <- predict(lmfit_Cons_EU_quantity, Month=timevals )
summary(global_pred_Cons_EU_quantity)

lines(global_pred_Cons_EU_quantity,main="Consumer_EU_quantity", xlab = "Months", ylab = "quantity" ,col='magenta',lwd=2)
legend("topleft",c("Raw","Smoothed","Glob_Pred"), col=c("Black","Blue","Magenta"), lwd=1)


#For locally predictable series

local_pred_Cons_EU_quantity <- ts_Consumer_EU_quantity-global_pred_Cons_EU_quantity
plot(local_pred_Cons_EU_quantity,main="Local_TS_Consumer_EU_quantity", xlab = "Months", ylab = "quantity" , col='red', type = "l")
acf(local_pred_Cons_EU_quantity)
acf(local_pred_Cons_EU_quantity, type="partial")
armafit_Cons_EU_quantity <- auto.arima(local_pred_Cons_EU_quantity)


par(mar=c(5,2,3,3))
tsdiag(armafit_Cons_EU_quantity)

armafit_Cons_EU_quantity
plot(armafit_Cons_EU_quantity$x,main="Local_Pred_Consumer_EU_quantity", xlab = "Months", ylab = "quantity" )
lines(fitted(armafit_Cons_EU_quantity),col='magenta',lwd=2)



resi_Cons_EU_quantity <- local_pred_Cons_EU_quantity-fitted(armafit_Cons_EU_quantity)
lines(resi_Cons_EU_quantity,col='red',lwd=1)
legend("topleft",c("Raw","ARMA_Local","Resi"), col=c("Black","Magenta","Red"), lwd=2)


#Check if the residual series is white noise

adf.test(resi_Cons_EU_quantity,alternative = "stationary")
kpss.test(resi_Cons_EU_quantity)

#Model evaluation using MAPE
#Prediction for the last 6 months

outdata_Cons_EU_quantity <- Consumer_EU[43:48,6]
timevals_out <- c(43:48)

# Predict the local ARMA values for 6 obs :

Local_pred_auto_arima_Cons_EU_quantity <- predict(armafit_Cons_EU_quantity, n.ahead = 6)

GLobal_pred_out_Cons_EU_quantity <- predict(lmfit_Cons_EU_quantity,data.frame(Month=timevals_out) )


plot(GLobal_pred_out_Cons_EU_quantity,main="Outdata GLobal_Pred_Consumer_EU_quantity", xlab = "Outdata Months", ylab = "quantity" )
plot(Local_pred_auto_arima_Cons_EU_quantity$pred,main="Outdata Local_Pred_Consumer_EU_quantity", xlab = "Outdata Months", ylab = "quantity" )

fcast_Cons_EU_quantity <- GLobal_pred_out_Cons_EU_quantity+Local_pred_auto_arima_Cons_EU_quantity$pred
plot(fcast_Cons_EU_quantity,main="Outdata Glob+Local Pred_Consumer_EU_quantity", xlab = "Months", ylab = "quantity" )
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_Cons_EU_quantity <- accuracy(fcast_Cons_EU_quantity,outdata_Cons_EU_quantity)[5]
MAPE_class_dec_Cons_EU_quantity

# MAPE = 22.57%

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit


class_dec_pred_Cons_EU_quantity <- c(ts(global_pred_Cons_EU_quantity)[1:42],ts(fcast_Cons_EU_quantity))
plot(ts(Consumer_EU[,6]),main="Final Classical Decoposition Consumer_EU_quantity", xlab = "Months", ylab = "quantity" , col = "black")
lines(class_dec_pred_Cons_EU_quantity, col = "red")
legend("topleft",c("Raw","Final_Pred"), col=c("Black","Red"), lwd=1)



#ARIMA fit for Consumer EU quantity

autoarima_Cons_EU_quantity <- auto.arima(ts_Consumer_EU_quantity)
autoarima_Cons_EU_quantity
tsdiag(autoarima_Cons_EU_quantity)
plot(autoarima_Cons_EU_quantity$x,main="Auto ARIMA Consumer_EU_quantity", xlab = "Months", ylab = "quantity", col="black")
lines(fitted(autoarima_Cons_EU_quantity), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_Cons_EU_quantity <- ts_Consumer_EU_quantity - fitted(autoarima_Cons_EU_quantity)

adf.test(resi_auto_arima_Cons_EU_quantity,alternative = "stationary")
kpss.test(resi_auto_arima_Cons_EU_quantity)

#Also, let's evaluate the model using MAPE
full_auto_arima_Cons_EU_quantity <- predict(autoarima_Cons_EU_quantity, n.ahead = 6)


MAPE_auto_arima_Cons_EU_quantity <- accuracy(full_auto_arima_Cons_EU_quantity$pred,outdata_Cons_EU_quantity)[5]
MAPE_auto_arima_Cons_EU_quantity

# MAPE  - 30.13%

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_Cons_EU_quantity <- c(fitted(autoarima_Cons_EU_quantity),ts(full_auto_arima_Cons_EU_quantity$pred))
plot(ts(Consumer_EU[,6]),main="Final Auto ARIMA Consumer_EU_quantity", xlab = "Months", ylab = "quantity", col = "black")
lines(auto_arima_pred_Cons_EU_quantity, col = "red")
legend("topleft",c("Raw","Final_Pred"), col=c("Black","Red"), lwd=1)








############################################### APAC #############################################################



#################### Model Building for Consumer APAC sales prediction ##############


# Classical decomposition:

# Smoothing :

# Moving average smoothing: 

ts_Consumer_APAC_sales <- ts(Consumer_APAC[1:42,4])
plot(ts_Consumer_APAC_sales,main="Indata Consumer_APAC_Sales ", xlab = "Months", ylab = "Sales" )

# Width 4 fits best after many trials

w <- 4
smoothedseries <- stats::filter(ts_Consumer_APAC_sales, 
                                filter=rep(1/(w),(w)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#lines(smoothedseries, col="blue")

#Smoothing right end of the time series

n <- length(ts_Consumer_APAC_sales)
diff <- smoothedseries[n-w+1] - smoothedseries[n-w]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

lines(smoothedseries, col="red")

#Plot the smoothed time series


lines(smoothedseries,main="Consumer_APAC_Sales", xlab = "Months", ylab = "Sales" , col="blue", lwd=1)
legend("topleft",c("Raw","Smoothed"), col=c("Black","Blue"), lwd=1)

# Converting smoothed time series into a dataframe:
timevals <<-c(1:42)

Cons_APAC_sales_df <- as.data.frame(cbind(timevals,as.vector(smoothedseries)))
colnames(Cons_APAC_sales_df) <- c("Month","Sales")


# Global prediction model building
#Mape 56#lmfit_Cons_APAC_sales <- lm(Sales ~ sin(0.5*Month)*poly(Month,2)+cos(0.5*Month)*poly(Month,2)+Month, data = Cons_APAC_sales_df)
#Mape 25#lmfit_Cons_APAC_sales <- lm(Sales ~ sin(0.5*Month)+cos(0.5*Month)+Month, data = Cons_APAC_sales_df)
#Mape 32#lmfit_Cons_APAC_sales <- lm(Sales ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,2)+Month, data = Cons_APAC_sales_df)
#Mape 24#lmfit_Cons_APAC_sales <- lm(Sales ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,5)+Month, data = Cons_APAC_sales_df)
#Mape 20#lmfit_Cons_APAC_sales <- lm(Sales ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,4)+Month, data = Cons_APAC_sales_df)


lmfit_Cons_APAC_sales <- lm(Sales ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,4)+Month, data = Cons_APAC_sales_df)
global_pred_Cons_APAC_sales <- predict(lmfit_Cons_APAC_sales, Month=timevals )
summary(global_pred_Cons_APAC_sales)

lines(global_pred_Cons_APAC_sales,main="Consumer_APAC_Sales", xlab = "Months", ylab = "Sales" ,col='magenta',lwd=2)
legend("topleft",c("Raw","Smoothed","Glob_Pred"), col=c("Black","Blue","Magenta"), lwd=1)


#For locally predictable series

local_pred_Cons_APAC_sales <- ts_Consumer_APAC_sales-global_pred_Cons_APAC_sales
plot(local_pred_Cons_APAC_sales,main="Local_TS_Consumer_APAC_Sales", xlab = "Months", ylab = "Sales" , col='red', type = "l")
acf(local_pred_Cons_APAC_sales)
acf(local_pred_Cons_APAC_sales, type="partial")
armafit_Cons_APAC_sales <- auto.arima(local_pred_Cons_APAC_sales)


par(mar=c(5,2,3,3))
tsdiag(armafit_Cons_APAC_sales)

armafit_Cons_APAC_sales
plot(armafit_Cons_APAC_sales$x,main="Local_Pred_Consumer_APAC_Sales", xlab = "Months", ylab = "Sales" )
lines(fitted(armafit_Cons_APAC_sales),col='magenta',lwd=2)

###### Thus Local TS is not present in the time series for APAC sales 

resi_Cons_APAC_sales <- local_pred_Cons_APAC_sales-fitted(armafit_Cons_APAC_sales)
lines(resi_Cons_APAC_sales,col='red',lwd=1)
legend("topleft",c("Raw","ARMA_Local","Resi"), col=c("Black","Magenta","Red"), lwd=2)


#Check if the residual series is white noise

adf.test(resi_Cons_APAC_sales,alternative = "stationary")
kpss.test(resi_Cons_APAC_sales)

# Residual is white noise 

#Model evaluation using MAPE
#Prediction for the last 6 months

outdata_Cons_APAC_sales <- Consumer_APAC[43:48,4]
timevals_out <- c(43:48)

# Predict the local ARMA values for 6 obs :

Local_pred_auto_arima_Cons_APAC_sales <- predict(armafit_Cons_APAC_sales, n.ahead = 6)

GLobal_pred_out_Cons_APAC_sales <- predict(lmfit_Cons_APAC_sales,data.frame(Month=timevals_out) )


plot(GLobal_pred_out_Cons_APAC_sales,main="Outdata GLobal_Pred_Consumer_APAC_Sales", xlab = "Outdata Months", ylab = "Sales" )

fcast_Cons_APAC_sales <- GLobal_pred_out_Cons_APAC_sales
plot(fcast_Cons_APAC_sales,main="Outdata Glob+Local Pred_Consumer_APAC_Sales", xlab = "Months", ylab = "Sales" )
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_Cons_APAC_sales <- accuracy(fcast_Cons_APAC_sales,outdata_Cons_APAC_sales)[5]
MAPE_class_dec_Cons_APAC_sales

# MAPE = 22.73%


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit


class_dec_pred_Cons_APAC_sales <- c(ts(global_pred_Cons_APAC_sales)[1:42],ts(fcast_Cons_APAC_sales))
plot(ts(Consumer_APAC[,4]),main="Final Classical Decoposition Consumer_APAC_Sales", xlab = "Months", ylab = "Sales" , col = "black")
lines(class_dec_pred_Cons_APAC_sales, col = "red")
legend("topleft",c("Raw","Final_Pred"), col=c("Black","Red"), lwd=1)



#ARIMA fit for Consumer APAC sales

autoarima_Cons_APAC_sales <- auto.arima(ts_Consumer_APAC_sales)
autoarima_Cons_APAC_sales
tsdiag(autoarima_Cons_APAC_sales)
plot(autoarima_Cons_APAC_sales$x,main="Auto ARIMA Consumer_APAC_Sales", xlab = "Months", ylab = "Sales", col="black")
lines(fitted(autoarima_Cons_APAC_sales), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_Cons_APAC_sales <- ts_Consumer_APAC_sales - fitted(autoarima_Cons_APAC_sales)

adf.test(resi_auto_arima_Cons_APAC_sales,alternative = "stationary")
kpss.test(resi_auto_arima_Cons_APAC_sales)

#Also, let's evaluate the model using MAPE
full_auto_arima_Cons_APAC_sales <- predict(autoarima_Cons_APAC_sales, n.ahead = 6)


MAPE_auto_arima_Cons_APAC_sales <- accuracy(full_auto_arima_Cons_APAC_sales$pred,outdata_Cons_APAC_sales)[5]
MAPE_auto_arima_Cons_APAC_sales

# MAPE  = 27.68%


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_Cons_APAC_sales <- c(fitted(autoarima_Cons_APAC_sales),ts(full_auto_arima_Cons_APAC_sales$pred))
plot(ts(Consumer_APAC[,4]),main="Final Auto ARIMA Consumer_APAC_Sales", xlab = "Months", ylab = "Sales", col = "black")
lines(auto_arima_pred_Cons_APAC_sales, col = "red")
legend("topleft",c("Raw","Final_Pred"), col=c("Black","Red"), lwd=1)




######################################################################################

################# Consumer  APAC  Quantity predictions ################################



# Classical decomposition:

# Smoothing :

# Moving average smoothing: 

ts_Consumer_APAC_quantity <- ts(Consumer_APAC[1:42,6])
plot(ts_Consumer_APAC_quantity,main="Indata Consumer_APAC_quantity ", xlab = "Months", ylab = "quantity" )

# Width 3 fits best after many trials

w <- 3
smoothedseries <- stats::filter(ts_Consumer_APAC_quantity, 
                                filter=rep(1/(w),(w)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#lines(smoothedseries, col="blue")

#Smoothing right end of the time series

n <- length(ts_Consumer_APAC_quantity)
diff <- smoothedseries[n-w+1] - smoothedseries[n-w]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#lines(smoothedseries, col="red")

#Plot the smoothed time series


lines(smoothedseries,main="Consumer_APAC_quantity", xlab = "Months", ylab = "quantity" , col="blue", lwd=1)
legend("topleft",c("Raw","Smoothed"), col=c("Black","Blue"), lwd=1)

# Converting smoothed time series into a dataframe:
timevals <<-c(1:42)

Cons_APAC_quantity_df <- as.data.frame(cbind(timevals,as.vector(smoothedseries)))
colnames(Cons_APAC_quantity_df) <- c("Month","quantity")


# Global prediction model building
#Mape 33#lmfit_Cons_APAC_quantity <- lm(quantity ~ sin(0.5*Month)*poly(Month,2)+cos(0.5*Month)*poly(Month,2), data = Cons_APAC_quantity_df)
#Mape 33.38#lmfit_Cons_APAC_quantity <- lm(quantity ~ sin(0.5*Month)*poly(Month,2)+cos(0.5*Month)*poly(Month,2)+poly(Month,2), data = Cons_APAC_quantity_df)
#Mape 22.76#lmfit_Cons_APAC_quantity <- lm(quantity ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,2), data = Cons_APAC_quantity_df)


lmfit_Cons_APAC_quantity <- lm(quantity ~ sin(0.5*Month)+cos(0.5*Month)+poly(Month,2), data = Cons_APAC_quantity_df)
global_pred_Cons_APAC_quantity <- predict(lmfit_Cons_APAC_quantity, Month=timevals )
summary(global_pred_Cons_APAC_quantity)

lines(global_pred_Cons_APAC_quantity,main="Consumer_APAC_quantity", xlab = "Months", ylab = "quantity" ,col='magenta',lwd=2)
legend("topleft",c("Raw","Smoothed","Glob_Pred"), col=c("Black","Blue","Magenta"), lwd=1)


#For locally predictable series

local_pred_Cons_APAC_quantity <- ts_Consumer_APAC_quantity-global_pred_Cons_APAC_quantity
plot(local_pred_Cons_APAC_quantity,main="Local_TS_Consumer_APAC_quantity", xlab = "Months", ylab = "quantity" , col='red', type = "l")
acf(local_pred_Cons_APAC_quantity)
acf(local_pred_Cons_APAC_quantity, type="partial")
armafit_Cons_APAC_quantity <- auto.arima(local_pred_Cons_APAC_quantity)


par(mar=c(5,2,3,3))
tsdiag(armafit_Cons_APAC_quantity)

armafit_Cons_APAC_quantity
plot(armafit_Cons_APAC_quantity$x,main="Local_Pred_Consumer_APAC_quantity", xlab = "Months", ylab = "quantity" )
lines(fitted(armafit_Cons_APAC_quantity),col='magenta',lwd=2)


###### Thus Local TS is not present in the time series for APAC sales 


resi_Cons_APAC_quantity <- local_pred_Cons_APAC_quantity-fitted(armafit_Cons_APAC_quantity)
lines(resi_Cons_APAC_quantity,col='red',lwd=1)
legend("topleft",c("Raw","ARMA_Local","Resi"), col=c("Black","Magenta","Red"), lwd=2)


#Check if the residual series is white noise

adf.test(resi_Cons_APAC_quantity,alternative = "stationary")
kpss.test(resi_Cons_APAC_quantity)

#Model evaluation using MAPE
#Prediction for the last 6 months

outdata_Cons_APAC_quantity <- Consumer_APAC[43:48,6]
timevals_out <- c(43:48)

# Predict the local ARMA values for 6 obs :


GLobal_pred_out_Cons_APAC_quantity <- predict(lmfit_Cons_APAC_quantity,data.frame(Month=timevals_out) )


plot(GLobal_pred_out_Cons_APAC_quantity,main="Outdata GLobal_Pred_Consumer_APAC_quantity", xlab = "Outdata Months", ylab = "quantity" )

fcast_Cons_APAC_quantity <- GLobal_pred_out_Cons_APAC_quantity
plot(fcast_Cons_APAC_quantity,main="Outdata Glob+Local Pred_Consumer_APAC_quantity", xlab = "Months", ylab = "quantity" )
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_Cons_APAC_quantity <- accuracy(fcast_Cons_APAC_quantity,outdata_Cons_APAC_quantity)[5]
MAPE_class_dec_Cons_APAC_quantity

# MAPE  = 22.76%

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit


class_dec_pred_Cons_APAC_quantity <- c(ts(global_pred_Cons_APAC_quantity)[1:42],ts(fcast_Cons_APAC_quantity))
plot(ts(Consumer_APAC[,6]),main="Final Classical Decoposition Consumer_APAC_quantity", xlab = "Months", ylab = "quantity" , col = "black")
lines(class_dec_pred_Cons_APAC_quantity, col = "red")
legend("topleft",c("Raw","Final_Pred"), col=c("Black","Red"), lwd=1)



#ARIMA fit for Consumer APAC quantity

autoarima_Cons_APAC_quantity <- auto.arima(ts_Consumer_APAC_quantity)
autoarima_Cons_APAC_quantity
tsdiag(autoarima_Cons_APAC_quantity)
plot(autoarima_Cons_APAC_quantity$x,main="Auto ARIMA Consumer_APAC_quantity", xlab = "Months", ylab = "quantity", col="black")
lines(fitted(autoarima_Cons_APAC_quantity), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_Cons_APAC_quantity <- ts_Consumer_APAC_quantity - fitted(autoarima_Cons_APAC_quantity)

adf.test(resi_auto_arima_Cons_APAC_quantity,alternative = "stationary")
kpss.test(resi_auto_arima_Cons_APAC_quantity)

#Also, let's evaluate the model using MAPE
full_auto_arima_Cons_APAC_quantity <- predict(autoarima_Cons_APAC_quantity, n.ahead = 6)


MAPE_auto_arima_Cons_APAC_quantity <- accuracy(full_auto_arima_Cons_APAC_quantity$pred,outdata_Cons_APAC_quantity)[5]
MAPE_auto_arima_Cons_APAC_quantity

# MAPE = 26.24%

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_Cons_APAC_quantity <- c(fitted(autoarima_Cons_APAC_quantity),ts(full_auto_arima_Cons_APAC_quantity$pred))
plot(ts(Consumer_APAC[,6]),main="Final Auto ARIMA Consumer_APAC_quantity", xlab = "Months", ylab = "quantity", col = "black")
lines(auto_arima_pred_Cons_APAC_quantity, col = "red")
legend("topleft",c("Raw","Final_Pred"), col=c("Black","Red"), lwd=1)



# After looking at MAPE and AIC values, Classical decomposition models outperform the Auto ARIMA models in segments












