

################################# CASE STUDY 4 ####################

######======= Scenerio 1 ========#############


#Data conversion
data <- gauge.1wug38c.2d15noj
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

#Residuals
resi <- residuals(fit)
resi

##QQ-Plot residuals
qqnorm(fit$residuals, main = "QQ-Plot: Residuals & Least Squares Regression Line")
qqline(fit$residuals, col = "red")


#Fit residuals 

plot(resi, main = "Residuals with Log(gain)", type ='h', ylab="Standard Residual", xlab ="Interval")
abline(0,0, col="red")

plot(resi, main = "Residuals with Log(gain)", ylab="Standard Residual", xlab ="Interval")
abline(0,0, col="red")

#Extra Residual Plot
hist(fit$residuals, main ="Residual Histogram")





##################Scenario 2 #####################

fit <- lm(log.gain ~ density)
summary(fit)
plot(density, log.gain, main = "Density vs Log(gain)")
abline(fit)

#Confidence Interval
new.den = seq(min(density),max(density),by = 0.05)
CI <- predict(fit, newdata=data.frame(density=new.den), interval="confidence",
              level = 0.95)
lines(new.den, CI[,2], col="blue", lty=2)
lines(new.den, CI[,3], col="blue", lty=2)

#Prediction Intervals 
pred.int <- predict(fit, newdata=data.frame(density = new.den), interval="prediction",
                    level = 0.95)
pred.int
lines(new.den, pred.int[,2], col="orange", lty=2)
lines(new.den, pred.int[,3], col="orange", lty=2)

#Predict density 

predict.densities <- function(gain) {
  b <- fit$coefficients[[1]]
  slope <- fit$coefficients[[2]]
  pred.value <- (log(gain)-b)/slope
  
}

print(predict.densities(38.6))

#Predict Density Intervals

pred.int <- function(x,y,given.y) {
  given.y <- log(given.y)
  size <- length(y)
  fit.model <- lm(y ~ x)
  y.fitted <- fit.model$fitted.values
  
  b <- fit.model$coefficients[[1]]
  slope <- fit.model$coefficients[[2]]
  
  #predicted value of Y
  x.pred <- (given.y - b)/slope
  print(x.pred)
  
  sse <- sum((y - y.fitted)^2)
  mse <- sse / (size - 2)
  
  #95% quantile value 
  t.val <- qt(0.975, size - 2)
  #standard error of mean estimate
  
  mean.se <- (1 / size + (x.pred - mean(x))^2 / (sum((x - mean(x))^2)))
  pred.se <- (1 + (1 / size) + (x.pred - mean(x))^2 / (sum((x - mean(x))^2)))
  
  mean.CI.upper <- given.y + t.val*sqrt(mse*mean.se)
  mean.CI.lower <- given.y - t.val*sqrt(mse*mean.se)
  
  #Upper/lower density
  densi.CI.upper <- (mean.CI.upper-b)/slope
  print(densi.CI.upper)
  densi.CI.lower <- (mean.CI.lower-b)/slope
  print(densi.CI.lower)
  
  pred.upper <- given.y + t.val*sqrt(mse*pred.se)
  pred.lower <- given.y - t.val*sqrt(mse*pred.se)
  
  densi.pred.upper <- (pred.upper-b)/slope
  print(densi.pred.upper)
  densi.pred.lower <- (pred.lower-b)/slope
  print(densi.pred.lower)
  
  
  upper <- data.frame(rbind(round(densi.CI.upper, 2), round(densi.pred.upper, 2)))
  lower <- data.frame(rbind(round(densi.CI.lower, 2), round(densi.pred.lower, 2)))
  fit <- data.frame(rbind(round(given.y, 2), round(given.y, 2)))
  
  results <- data.frame(cbind(lower, upper, fit), row.names = c('Mean', 'Prediction'))
  colnames(results) <- c('Lower', 'Upper', 'Fit')
  
  return(results)
}

#Predict interval values 
print(pred.int(density,log.gain, 38.6))
print(pred.int(density,log.gain, 426.7))




##############Scenario 3 #######################

#Omitting all densities 0.508 from oG data 
length(data$gain)
data_row <- which(data$density == 0.508)
data_row
data_omit <- data[-data_row, ]
data_omit


log.omit <-log(data_omit$gain)
log.omit

fit <- lm(log.omit ~ data_omit$density)
fit
#Plot with w/ omitted
plot(data_omit$density, log.omit, main = "Density vs Log(gain):Omitted")
abline(fit)

density.omi <- data_omit$density
density.omi

#Confidence Interval
new.densi = seq(min(density.omi),max(density.omi),by = 0.05)
new.omit <- which(new.densi == 0.501)
den.omit <- new.densi[-c(new.omit)]
CI <- predict(fit, newdata=data.frame(den.omit<-density.omi), interval="confidence",level = 0.95)
lines(den.omit, CI[,2], col="blue", lty=2)
lines(den.omit, CI[,3], col="blue", lty=2)

#Predicting Interval 
pred.int <- predict(fit, newdata=data.frame(den.omit<-density.omi), interval="prediction",
                    level = 0.95)
pred.int
lines(den.omit, pred.int[,2], col="orange", lty=2)
lines(den.omit, pred.int[,3], col="orange", lty=2)

#Predicting densities 
print(pred.int(density.omi,log.omit, 400.429232))


#Predict density
print(predict.densities(38.6))
print(predict.densities(400.429232))

b <- fit$coefficients[[1]]
slope <- fit$coefficients[[2]]

gain.val <- slope*(0.001) + b
gain.val <- exp(gain.val)
print(pred.int(density.omi,log.omit, gain.val))


