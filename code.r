ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 24)[-1]
PACF = ARMAacf(ar=c(1.5,-.75), ma=0, 24, pacf=TRUE)
par(mfrow=c(1,2))


Corn Data

cornModel <

cornModel<- lm(cornYield ~ aprilPrecip + mayPrecip + junePrecip + julyPrecip + augustPrecip + septPrecip + days90 + phosphate + nitrogen, newCorn)

cornModel3 <- step(cornModel)

cornModel4 <- lm(cornYield ~ mayPrecip + phosphate + days90, Data)

cornModel <- lm(cornYield ~ aprilPrecip + mayPrecip + junePrecip + augustPrecip + septPrecip + days90 + phosphate + nitrogen, Data)

cornModel <- lm(cornYield ~ aprilPrecip + mayPrecip + junePrecip + augustPrecip + days90 + phosphate + nitrogen, Data)

cornModel <- lm(cornYield ~ aprilPrecip + mayPrecip + augustPrecip + days90 + phosphate + nitrogen, Data)

cornModel <- lm(cornYield ~ aprilPrecip + mayPrecip + augustPrecip + days90 + phosphate, Data)

cornModel2 <- lm(cornYield ~ mayPrecip + days90 + phosphate, Data)

abline(coef(cornYield ~ mayPrecip + days90 + phosphate, Data))


summary(cornModel, main = "Forecasting Corn Production in Dane County")

plot(cornModel, main = "Forecasting Corn Production in Dane County")

cornPred <- predict(cornModel)

neuralCorn <- nnet(cornYield ~ aprilPrecip + mayPrecip + junePrecip + julyPrecip + augustPrecip + septPrecip + days90 + phosphate + nitrogen, Data, size=2)

nnet.fit <- nnet(cornYield/32107000 ~ aprilPrecip + mayPrecip + junePrecip + julyPrecip + augustPrecip + septPrecip + days90 + phosphate + nitrogen, data=Data, size=2) plot(Data$med

plot(Data$medv, nnet.predict,main="Neural network predictions vs actual", xlab="Actual")

plot(cornModel$medv, nnet.predict, main="Neural network predictions vs actual", xlab="Actual")

nnet.fit <- qrnn.fit(cornYield/50 ~ aprilPrecip + mayPrecip + junePrecip + julyPrecip + augustPrecip + septPrecip + days90 + phosphate + nitrogen, data=Data, size=2)

plot(Data$medv, nnet.predict,main="Neural network predictions vs actual", xlab="Actual")


nn <- nnet(cornYield ~ aprilPrecip + mayPrecip + junePrecip + julyPrecip + augustPrecip + septPrecip + days90 + phosphate + nitrogen,data=Data, hidden=8, size=40,linear.output=TRUE)

nn <- nnet(cornYield ~ aprilPrecip, data=Data, hidden=8, size=40)

qplot(year, cornYield, data=Data, geom = "line")

predictMLRCorn <- predict(cornModel)

abline(predictMLRCorn, col="red")

cornDiff1 <- diff(cornYield, differences=1)

ts(cornYield, start=c(1970, 1))

ARIMA

y=cornProd
diffCorn=diff(cornYieldTS)
t=Year
plot(t,y)
plot(t,dt)

adf.test(y, alternative="stationary", k=0)
adf.test(y, alternative="explosive", k=0)

mydata.arima101 <- arima(y, order = c(1,0,1))
mydata.pred1 <- predict(mydata.arima101, n.ahead=100)
plot (y)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")

arima(diffCorn, order = c(1,0,0))

arima2 <- arima(cornProd, order = c(2,1,0))

mydata.arima101 <- arima(diffCorn, order = c(2,0,0))
mydata.pred1 <- predict(mydata.arima101, n.ahead=200)
plot (diffCorn)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")

predict(arima(cornYieldTS, c(0,0,0)))

plot(year,aprilPrecip)

abline(coef(lm(predictCorn~year, Data)))
plot(year,aprilPrecip,main="April Rainfall (inches) in Madison, Wisconsin via Stephen Data")

corData <- cor(Data)

plot(cornModel2, which=4)

cooks <- cooks.distance(cornModel2)

predictCorn <- predict(cornModel4)

cornModel3 <- lm(cornYield ~ mayPrecip + augustPrecip + days90 + phosphate, Data2)

cornModel4 <- lm(cornYield ~ mayPrecip + days90 + phosphate, Data2)

mlrResid <- resid(cornModel4)

fit <- arima(cornProd, order = c(4,1,0))

accuracy(fit)

plot(forecast(fit,))

fit <- auto.arima(cornProd)

plot(year,cornYield)
lines(year, cornPredict)

forecast1 <- forecast(fit, h=20)

Box.test(resid(fit),type="Ljung",lag=1,fitdf=1)


plot(forecast1, main="Stock Forecasts ARIMA(4, 1, 0) by stephen Data")

plot(Data2$year,Data2$cornYield,xlab="Year", ylab="Corn Yield")
abline(lsfit(Data2$year,Data2$cornYield))

StanRes1 <- rstandard(cornModel4)
plot(cornYield,StanRes1,xlab="Corn Yield", ylab="Standardized Residuals")

arima.sim(model, n=100, innov=NULL, n.start=100)

lines(ARIMA~Time, predictions)
lines(MLR~Time, predictions)


lm <- lm(mlr~actual,results)



boot <- boot.relimp(cornModel, b = 1000, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

cornModel<- lm(cornYield ~ aprilPrecip + mayPrecip + junePrecip + julyPrecip + augustPrecip + septPrecip + days90 + phosphate + nitrogen, newCorn)

cornModel6 <- step(cornModel, direction="both")

plot(MLR,Actual)
abline(coef(lm(MLR~Actual, predictions)))

autoFit <- auto.arima(cornProd)
qqline(autoFit$, asp=0)

Box.test(resid(autoFit),type="Ljung",lag=4,fitdf=1)

arima2 <- arima(diffCorn, order = c(2,1,0))

cor(actual,predictARIMA1)

resid(autoFit)

qqnorm(autoFit$resid)
qqline(autoFit$resid)

d = sum((autoFit$residuals - lag(autoFit$residuals))^2, na.rm = TRUE) /
       sum(autoFit$residuals^2, na.rm = TRUE)
ad

d2 = sum((arima2$residuals - lag(arima2$residuals))^2, na.rm = TRUE) /
       sum(arima2$residuals^2, na.rm = TRUE)


m <- lm(residuals(autoFit) ~ 1) 
bgtest(m)

m1 <- lm(residuals(arima2) ~ 1) 

hist(arima2$resid)

curve(dnorm(x, mean(arima2$resid), sd(arima2$resid)), add=T)

x <- arima2$resid 
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", 
    main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

autoResid <- resid(autoFit)
arimaResid <- resid(arima2)