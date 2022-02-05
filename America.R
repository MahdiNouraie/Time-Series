data = read.csv("C:\\Users\\owner\\Desktop\\TS\\data.csv")
data = as.vector(data)
datat = data[2:320,]


plot(1:319 , datat , pch = 16 , col = 3 , main = 'Positives Time Series', ylab = 'Positive' , xlab = 'Time')

t = 1:length(datat)
t2 = t ^ 2
model = lm(datat ~ t + t2)
summary(model)
resids = residuals(model)
fits = fitted(model)

par(mfrow = c(2,2))
hist(resids , col = 3)
qqnorm(resids , col = 2 , pch = 16)
qqline(resids , col = 4)
plot(resids ~ fits , col = 9 , pch =15)
plot(resids ~ t, type = 'o' , col = 6 , pch = 16)


shapiro.test(resids)
library(nortest)
ad.test(resids)

F = c(rep(1:32 , each = 10))
fac = as.factor(F)
fac = fac[-length(fac)]
library(car)
leveneTest(resids , group = fac)

library(randtests)
runs.test(resids)

#-----------------------------------------------------------
leveneTest(datat , group = fac)
datat_new = log(datat)


#library(forecast)
#lambda = BoxCox.lambda(datat)
#datat_new = BoxCox(datat , lambda)

leveneTest(datat_new , group = fac)

kruskal.test(datat_new , g = fac)

datat.diff = diff(datat_new)
fac.diff = fac[-1]
kruskal.test(datat.diff, g = fac.diff)

plot(datat.diff , col = 2 , type = 'o' , pch = 16)


par(mfrow = c(1,2))
acf(datat.diff)
pacf(datat.diff)

library(forecast)
fit = auto.arima(y = datat_new , d = 1 , D = 0 , start.p = 0 , start.q = 0,
max.p = 11 , max.q = 11 , seasonal = TRUE , start.P = 0 , start.Q = 0 , max.P = 11,
max.Q = 11 , ic = 'aicc' , stepwise = TRUE)
summary(fit)

y = ts(data = datat_new)
order = c(2, 1, 1)
order = c(3, 1, 2)
model = Arima(y = y , order = order)
summary(model)

period = 6
y = ts(data = data_new , frequency = period)
order = c(2 , 1 , 2)
sorder = c(3, 1, 1)
sorder = c(3 , 1 , 2)
seasonal = list(order = sorder, period = period)
model = Arima(y = y, order = order, seasonal = seasonal)
summary(model)

Pred = forecast(fit , h = 10)
Pred = data.frame(Pred)
Pred = Pred[,c(1,4,5)]
Pred = exp(Pred)
data.test = data[321:330, ]
table = data.frame(cbind(Pred , data.test))
attach(table)

par(mfrow = c(1,2))
ts.plot(Point.Forecast , col =2)
ts.plot(data.test , col =3)
plot(forecast(fit , h = 20) , col = 2)
