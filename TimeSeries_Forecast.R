## Time Series Forecasting
##install.packages(forecast)
##install.packages(fpp)
##Begin with inbuilt data
library(forecast)
data(elecsales, package = "fpp")
elecsales
plot(elecsales,ylab="Electricity", xlab="Year")
ma.1 <- ma(elecsales,order=5)
plot(elecsales, main="Electricity",
     ylab="GWh", xlab="Year")
lines(ma(elecsales,3),col="red")
lines(ma(elecsales,9),col="blue")
print(ma.1)
print(elecsales)
#################################################################################
###################################################################################
library("ggplot2")
library("forecast")
data(oil, package = "fpp")
plot(oil)
oildata <- window(oil,start=1996,end=2007)
plot(oildata, ylab="Oil (millions of tonnes)", xlab="Year")
#Simple exponential smoothing
fit1 <- ses(oildata, alpha=0.2, initial="simple", h=3) #lo=y1 or initial=optimal
fit2 <- ses(oildata, alpha=0.6, initial="simple", h=3)
fit3 <- ses(oildata, h=3)
#Plot for alpha=0.2,0.6,0.89
plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="l")
lines(fitted(fit1), col="blue", type="l")
lines(fitted(fit2), col="red", type="l")
lines(fitted(fit3), col="green", type="l")
lines(fit1$mean, col="blue", type="l")
lines(fit2$mean, col="red", type="l")
lines(fit3$mean, col="green", type="l")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)
#level lt for each alpha
fit1$model$state
fit2$model$state
fit3$model$state
#fitted(fit1)
#fitted(fit2)
#fitted(fit3)
#Summary of results
summary(fit1)
summary(fit2)
summary(fit3)

library(forecast)
data(ausair, package = "fpp")
#original data - from 1970 until 2010
plot(ausair) 
air <- window(ausair,start=1990,end=2004)
fit1 <- holt(air, alpha=0.8, beta=0.2, initial="simple", h=5) 

fit2 <- holt(air, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=5) 
fit3 <- holt(air, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=5) 
plot(air,ylab="Air passengers", xlab="Year")
air
print(air)
#Results for first model:
#level lt and trend bt
fit1$model$state 
#Fitted values (lt+bt)
fitted(fit1)
#Forecasts
fit1$mean
#Plots
#######################################################################
#######################################################################
#################ARIMA###################################################
#######################################################################
king=read.csv("kings.csv")
plot(king)
acf(king)
pacf(king)
Box.test(king, 10)
Box.test(king, 10, type = "Ljung-Box")
ar(king)
kg.ar1 <- arima(king, c(3, 0, 0))
kg.ar2 <- arima(king, c(1, 0, 0)) #BIC
summary(kg.ar2)
tsdiag(kg.ar2) 
#Diagnostics
tsdiag(kg.ar1) 
#Automatic modelling: AR models
kgAR <- auto.arima(king, d=0, D=0, max.p=12, max.q=0, max.P=0, max.Q=0, 
                   max.order=12, ic="bic", stepwise=FALSE, approximation=FALSE)
summary(kgAR)
#Automatic modelling: ARMA models
kgARMA <- auto.arima(king,  trace=T, d=0, D=0, max.p=12, max.q=12, max.P=0, max.Q=0, 
                     max.order=6, ic="aic", stepwise=F, approximation=FALSE)
kgARMA
summary(kgARMA)
########################################################################
## Temprature Forecast
data1= read.csv('AmericasTemp.csv')
bt=data1[,5]
bt=ts(bt,start='1991',frequency = 12)
##############################################
#####################################################################
#Temperature
#### Forecasting with Exponential Smoothing, ARIMA and ANN
############################################################
fit1 <- holt(bt, alpha=0.8, beta=0.2, initial="simple", h=26) 
fit2 <- holt(bt, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=26) 
fit3 <- holt(bt, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=26) 
fit4 <- hw(bt,seasonal="multiplicative", h=26)
fit5= auto.arima(bt,seasonal=T,trace=T)
fit6=nnetar(bt, decay = 0.5, maxit = 150)
#####################################################################
#####################################################################
plot(bt,ylab="Temperature", xlab="Year")
plot(fit2, type="l", ylab="Temperature", xlab="Year", 
     fcol="white", plot.conf=FALSE, main="")
lines(fitted(fit1), col="blue") 
lines(fitted(fit2), col="red")
lines(fitted(fit3), col="green")
lines(fitted(fit4),col='yellow')
lines(fitted(fit5),col='brown')
lines(fitted(fit6),col='wheat')

###############################################################

lines(fit1$mean, col="blue", type="l") 
lines(fit2$mean, col="red", type="l")
lines(fit3$mean, col="green", type="l")
lines(fit4$mean,col='yellow',type='l')
lines(fit5$mean,col='brown',type='l')
lines(fit6$mean,col='wheat',type='l')
legend("topleft", lty=1, col=c("black","blue","red","green",'yellow'), 
       c("Temperature","SES","HW-Exponential trend","Additive damped trend",'Multiplicative','SARIMA','ANN'))


#Forecast results
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

forc1=forecast(fit1, h = 26);forc1
forc2=forecast(fit2, h = 26);forc2
forc3=forecast(fit3, h = 26);forc3
forc4=forecast(fit4, h = 26 );forc4
forc5=forecast(fit5, h = 26);forc5
forc6=forecast(fit6, h = 26);forc6


#plot(forc1,ylab='Temperature')
#plot(forc2,ylab='Temperature')
#plot(forc3,ylab= 'Temperature')
#plot(forc4,ylab='Temperature')
#plot(forc5,ylab = 'Temperature')
#plot(forc6,ylab = 'Temperature')

#accuracy(forc1)
#accuracy(forc2)
#accuracy(forc3)
#accuracy(forc4)
#accuracy(forc5)
#accuracy(forc6)
###########################################################################
##The best model is the model with the lowest RMSE
###########################################################################

#######################################################################
#######################################################################
## Rainfall Forecast
br= read.csv('AmericasRainfall.csv')
br = br[,5]
br = ts(br, start = 1991, frequency = 12)
plot(br)
#####################################################################
########Rainfall models
############################################################
mod1 <- holt(br, alpha=0.8, beta=0.2, initial="simple", h=26) 
mod2 <- holt(br, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=26) 
mod3 <- holt(br, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=26) 
mod4 <- hw(br,seasonal="multiplicative", h = 26)
mod5= auto.arima(br,seasonal=T,trace=T)
mod6=nnetar(br, decay =0.5, maxit = 150)
#####################################################################
#####################################################################
plot(br,ylab="Rainfall", xlab="Year")
plot(mod2, type="l", ylab="Rainfall", xlab="Year", 
     fcol="white", plot.conf=FALSE, main="")
lines(fitted(mod1), col="blue") 
lines(fitted(mod2), col="red")
lines(fitted(mod3), col="green")
lines(fitted(mod4),col='yellow')
lines(fitted(mod5),col='brown')
lines(fitted(mod6),col='wheat')

###############################################################

lines(mod1$mean, col="blue", type="l") 
lines(mod2$mean, col="red", type="l")
lines(mod3$mean, col="green", type="l")
lines(mod4$mean,col='yellow',type='l')
lines(mod5$mean,col='brown',type='l')
lines(mod6$mean,col='wheat',type='l')
legend("topleft", lty=1, col=c("black","blue","red","green",'yellow'), 
       c("Rainfall","SES","HW-Exponential trend","Additive damped trend",'Multiplicative','SARIMA','ANN'))

##########################################################################################################
#Forecast results
summary(mod1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

ft1=forecast(mod1, h = 26);ft1
ft2=forecast(mod2, h  = 26);ft2
ft3=forecast(mod3, h = 26);ft3
ft4=forecast(mod4, h = 26);ft4
ft5=forecast(mod5, h = 26);ft5
ft6=forecast(mod6, h = 26);ft6


#plot(forc1,ylab='Rainfall')
#plot(forc2,ylab='Rainfall')
#plot(forc3,ylab= 'Rainfall')
#plot(forc4,ylab='Rainfall')
#plot(forc5,ylab = 'Rainfall')
#plot(forc6,ylab = 'Rainfall')

accuracy(forc1)
accuracy(forc2)
accuracy(forc3)
accuracy(forc4)
accuracy(forc5)
accuracy(forc6)
#####################################
###########################################################################
## Temperature
#seasonal=N,A,M

########################################################################


mod3=HoltWinters(bt, alpha =T)
forcast3=forecast(mod3, h=12)
plot(forcast3,  ylab= 'Rainfall')
accuracy(forcast3)




### Best model is ARIMA(1,0,0) with non-zero mean. Check and compare results

fit1 <- hw(br,seasonal="additive")
fit2 <- hw(br,seasonal="multiplicative")
fit3 <- hw(br,seasonal="additive", damped=TRUE)
fit4 <- hw(br,seasonal="multiplicative", damped=TRUE)
fit5=  auto.arima(br,seasonal=T, trace=T)
fit6= nnetar(br)
#level lt, trend bt, and seasons 
fit1$model$state 
#Fitted values (lt+bt)
fitted(fit1)
#Forecasts
fit1$mean
plot(fit2,ylab="Temperature",
     plot.conf=FALSE, type="l", fcol="white", xlab="Year",main="")
lines(fitted(fit1), col="red", lty=2)
lines(fitted(fit2), col="green", lty=2)
lines(fit1$mean, type="l", col="red")
lines(fit2$mean, type="l", col="green")
lines(fitted(fit3), col="blue", lty=2)
lines(fitted(fit4), col="grey", lty=2)
lines(fit3$mean, type="l", col="blue")
lines(fit4$mean, type="l", col="grey")

legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))
#Forecast results
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

####################################################################
###Neural Network
library(neuralnet)
library(forecast)
mod=nnetar(br)
forecast4=forecast(mod, h=12, level=95)
plot(forecast4, ylab= 'Rainfall',xlab='Year')
accuracy(forecast4)
##########################################################################################
############################################################################################
