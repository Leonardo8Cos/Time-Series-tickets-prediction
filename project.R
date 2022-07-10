rm(list=ls())

library(readxl)
library(timeSeries)
library(tseries)
library(TSstudio)
library(lmtest)
library(strucchange) #include Chow tet function
library(sandwich)
library(car)
library(forecast)
library(ggplot2)
library(gridExtra)

#non sempre da installare questi ultimi due
install.packages("rlang", type = "binary") 
library(tidyverse)

###########
# DATASET #
###########
#setwd("/Users/tommasofiori/Desktop/xG Project")
data <- read_xlsx("Inter 2009-2021.xlsx")
summary(data)
data$tickets
series <- ts(data$tickets, start = 2006, end = 2022, frequency = 19)
plot(series, ylab= "Tickets Sold", xlab="Seasons")


#############
# CHOW TEST #
#############

n <- length(data$tickets)
sctest(data$tickets[-1] ~ data$tickets[-n], type = "Chow", point = 260 )

#We can reject the null hypothesis of the test because the p-value is less than 0.05. 
#This means we have enough evidence to conclude that 
#the data contains a structural breakpoint.



#################
# CLEANING DATA #
#################


data.new <- data[-c(260:297),]
series.new <- ts(data.new$tickets, start = 2006, end = 2022, frequency = 19)
plot(series.new, ylab= "Tickets Sold", xlab="Seasons")
plot(diff(series.new), ylab= "Tickets Sold", xlab="Seasons")


#pdf("acf-pacf.pdf")
series.acf <- ggAcf(data.new$tickets, lag.max = 20)+ggtitle("ACF Tickets")
series.pacf <- ggAcf(data.new$tickets, type="partial", lag.max = 10)+ggtitle("PACF Tickets")
grid.arrange(series.acf, series.pacf, ncol=2)
dev.off()


plot(acf(diff(data.new$tickets), lag.max = 20), main = "ACF Tickets")
plot(pacf(diff(data.new$tickets), lag.max = 10), main = "PACF Tickets") 


########################
# BREUSCH-GODFREY TEST #
########################

n <- length(data.new$tickets)
fit <- lm(data.new$tickets[-1]~data.new$tickets[-n], data=data.new) #da chiedere al prof
bgtest(fit, 19) #strong autocorrelation 


######################
# DICKEY-FULLER TEST #
######################

fit.adf.tickets <- lm(data.new$tickets[-1]~data.new$tickets[-n], data=data.new )
adf.tickets <- coeftest(fit.adf.tickets)[2,1]/coeftest(fit.adf.tickets)[2,2]
adf.tickets

adf1 <- adf.test(data.new$tickets)
adf1$p.value # not rejected so our time series is not stationary

###############
# SEASONALITY #
###############

#pdf("stl.pdf")
plot(stl(series.new, s.window = 19)) 
dev.off()

###################
# PREPARE DATASET #
###################

#train test
data.train <- data.new[1:228,]
data.test <- data.new[229:266,]



#########################
# MODELLING SEASONALITY #
#########################


y <- ts(data.train[,"tickets"], start=2006, frequency=19)
plot(y)
plot(density(y))


#Seasonal dummies for train
Q1 <- ts(rep(c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q2 <- ts(rep(c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q3 <- ts(rep(c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q4 <- ts(rep(c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q5 <- ts(rep(c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q6 <- ts(rep(c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q7 <- ts(rep(c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q8 <- ts(rep(c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q9 <- ts(rep(c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q10 <-ts(rep(c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q11 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q12 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q13 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0), 12), start=2006, frequency=19)
Q14 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0), 12), start=2006, frequency=19)
Q15 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0), 12), start=2006, frequency=19)
Q16 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0), 12), start=2006, frequency=19)
Q17 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0), 12), start=2006, frequency=19)
Q18 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0), 12), start=2006, frequency=19)

xreg <- cbind(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18)



#################
# MODEL FITTING #
#################

#autoregressive
fit_ar <- arima(y, order=c(1,1,0), method = "ML", xreg = xreg)
checkresiduals(fit_ar)
qqnorm(fit_ar$residuals)
qqline(fit_ar$residuals, col="red")

#moving average
fit_ma <- arima(y, order=c(0,1,1), method = "ML", xreg = xreg)
checkresiduals(fit_ma)
qqnorm(fit_ma$residuals)
qqline(fit_ma$residuals, col="red")


#arima 1,1,1
fit_arima1 <- arima(y, order=c(1,1,1), method = "ML", xreg = xreg)
checkresiduals(fit_arima1)
qqnorm(fit_arima1$residuals)
qqline(fit_arima1$residuals, col="red")

#arima 2,1,2
fit_arima22 <- arima(y, order = c(2,1,2), method="ML", xreg=xreg)
checkresiduals(fit_arima22)
qqnorm(fit_arima22$residuals)
qqline(fit_arima22$residuals, col="red")

plot(acf(residuals(fit_arima22)))
plot(pacf(residuals(fit_arima22)))

#arima 3,1,1
fit_arima31 <- arima(y, order=c(3,1,1), method = "ML", xreg = xreg)
checkresiduals(fit_arima31)
qqnorm(fit_arima31$residuals)
qqline(fit_arima31$residuals, col="red")

plot(acf(residuals(fit_arima31)))
plot(pacf(residuals(fit_arima31))) #uncorrelated 


#############
# FORECASTS #
#############

#Seasonal dummies for test
P1 <- ts(rep(c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P2 <- ts(rep(c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P3 <- ts(rep(c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P4 <- ts(rep(c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P5 <- ts(rep(c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P6 <- ts(rep(c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P7 <- ts(rep(c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P8 <- ts(rep(c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P9 <- ts(rep(c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P10 <-ts(rep(c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P11 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P12 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0), 2), start=2018, frequency=19)
P13 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0), 2), start=2018, frequency=19)
P14 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0), 2), start=2018, frequency=19)
P15 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0), 2), start=2018, frequency=19)
P16 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0), 2), start=2018, frequency=19)
P17 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0), 2), start=2018, frequency=19)
P18 <-ts(rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0), 2), start=2018, frequency=19)

xreg1 <- cbind(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18)
xreg1



forecast_ar <- Arima(data.test$tickets,model=fit_ar, xreg= xreg1)

forecast_ma <- Arima(data.test$tickets,model=fit_ma, xreg= xreg1)

forecast_arima1 <- Arima(data.test$tickets,model=fit_arima1, xreg= xreg1)

forecast_arima22 <- Arima(data.test$tickets,model=fit_arima22, xreg= xreg1)

forecast_arima31 <- Arima(data.test$tickets,model=fit_arima31, xreg= xreg1)


###########
# METRICS #
###########


mse_ar <- mean((forecast_ar$fitted - data.test$tickets)^2)

mse_ma <- mean((forecast_ma$fitted - data.test$tickets)^2)

mse_arima1 <- mean((forecast_arima1$fitted - data.test$tickets)^2)

mse_arima22 <- mean((forecast_arima22$fitted - data.test$tickets)^2)

mse_arima31 <- mean((forecast_arima31$fitted - data.test$tickets)^2) #BEST

which.min(c(mse_ar,mse_ma,mse_arima1,mse_arima22,mse_arima31))

mse_arima31

##################
# POINT ESTIMATE #
##################

time <- data.new$Season
time

#pdf("point_est.pdf")
par(mfrow=c(1,2), mar=c(2,4.5,3,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(data.new$tickets, ylab = "Tickets" , main="ARIMA(3,1,1)", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(time, format="%Y")[axTicks(1)+1])
axis(2)
lines(data.new$tickets, col="grey")
lines(229:266, forecast_arima31$fitted, col=2)
abline(v=229,lty=2)

plot(data.new$tickets,ylab = "Tickets", main="ARIMA(2,1,2)", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(time, format="%Y")[axTicks(1)+1])
axis(2)
lines(data.new$tickets, col="grey")
lines(229:266, forecast_arima22$fitted, col=2)
abline(v=229,lty=2)
dev.off()


#######################
# PREDICTION INTERVAL #
#######################



#Checking Normal distribution assumption for the residuals

fit_arima31 #we use this model

#pdf("checkres.pdf")
checkresiduals(fit_arima31) #c(3,1,1)
dev.off()


#pdf("whitenoise.pdf")
tsdisplay(residuals(fit_arima31),  main = "Residuals ARIMA(3,1,1)")
dev.off()

#pdf("resnormal.pdf")
qqnorm(fit_arima31$residuals)
qqline(fit_arima31$residuals, col="red")
dev.off()

# par(mfrow=c(1,2), mar=c(2.5,4.5,4,1.5), pty="m", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
# plot(acf(residuals(fit_arima31)))
# plot(pacf(residuals(fit_arima31))) #uncorrelated 
# dev.off()  DOES NOT WORK WITH THE PAR FUNCTION 

#We predict the test set

fore <- predict(fit_arima31, n.ahead = 38, newxreg = xreg1, se.fit = TRUE)  
prediction <- fore$pred
std <- fore$se


##CREATE A PREDICTION INVERVALL OF 95%##

lower<- c()
upper <- c()
for (i in 1:38) {
  lower[i] <- prediction[i] -  1.96*std[i]
  upper[i] <- prediction[i] +  1.96*std[i]
}


##CREATE A PREDICTION INVERVALL OF 80%##

lower1<- c()
upper1 <- c()
for (i in 1:38) {
  lower1[i] <- prediction[i] -  1.28*std[i]
  upper1[i] <- prediction[i] +  1.28*std[i]
}


#95%
#par(mfrow=c(1,2), mar=c(2,4.5,3,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
#pdf("95.pdf")
plot(data.new$tickets,type="l", axes=F, ylab="Tickets")
axis(1, at=axTicks(1), labels = format(time, format="%Y")[axTicks(1)+1],
     xlim = (2006:2022))
axis(2)
abline(v=229,lty=2)
polygon(c(229:266,rev(229:266)),c(lower,rev(upper)), col="pink") #90%
lines(229:266, prediction, col=2, lwd=2) #prediction
lines(229:266, lower, col="orange", lwd=2) 
lines(229:266, upper, col="orange", lwd=2)
lines(229:266, data.test$tickets) #real values
dev.off()

#80%
#pdf("80.pdf")
plot(data.new$tickets,type="l", axes=F, ylab="Tickets")
axis(1, at=axTicks(1), labels = format(time, format="%Y")[axTicks(1)+1],
     xlim = (2006:2022))
axis(2)
abline(v=229,lty=2)
polygon(c(229:266,rev(229:266)),c(lower1,rev(upper1)), col="pink") #80%
lines(229:266, prediction, col=2, lwd=2) #prediction
lines(229:266, lower1, col="orange", lwd=2) 
lines(229:266, upper1, col="orange", lwd=2)
lines(229:266, data.test$tickets) #real values
dev.off()







###########
# THE END #
###########



### VECCHIA MANIERA
plot(data.new$tickets,type="l", axes=F)
axis(1, at=axTicks(1), labels = format(time, format="%Y")[axTicks(1)+1],
     xlim = (2006:2022))
axis(2)
abline(v=229,lty=2)
polygon(c(229:266,rev(229:266)),c(lower,rev(upper)), col="blue") #90%
polygon(c(229:266,rev(229:266)),c(lower1,rev(upper1)), col="pink") #80%
lines(229:266, prediction, col=2, lwd=2) #prediction
lines(229:266, data.test$tickets) #real values
#lines(data.new$tickets, col="black")






