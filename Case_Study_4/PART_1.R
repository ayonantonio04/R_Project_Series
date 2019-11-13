#============ Scenerio 1 ========#


#Data conversion
data <- gauge
density <- data$density
gain <- data$gain

#Plot exponential/OG data
plot(density, gain, main = "Density vs Gain Plot")
fit<- lm(gain ~ density)
abline(fit)
b <- fit$coefficients[[1]]
slope <- fit$coefficients[[2]]

#Log of gain + logistic R plot
log.gain <- log(gain)

plot(density, log.gain, main = "Density vs Log(gain)")
fit <- lm(log.gain ~ density)
abline(fit)
b <- fit$coefficients[[1]]
slope <- fit$coefficients[[2]]
slope
#Correlation
corr <- cor(density, log.gain)

resi <- residuals(fit)
resi
fit$residuals
plot(density, fit$residuals, main ="R")
plot(resi, main = "Residuals with Log(gain)", type ='h', ylab="Standard Residual", xlab ="Interval")
hist(fit$residuals, main ="Residual Histogram")


qqnorm(fit$residuals)
qqline(fit$residuals, col = "red")

hist(resi, col = 3, main="Histogram of Residuals")
hist(gain)

#Fit residuals 

plot(fit$fitted.values, fit$residuals, main = "Residual Plot")
abline(0,0, col="red")

plot(resi, main = "Residual Plot with log(gain)", ylab ="Standard Residual")
abline(0,0, col="red")


plot(resi, main = "Residuals with Log(gain)", type ='h', ylab="Standard Residual", xlab ="Interval")
