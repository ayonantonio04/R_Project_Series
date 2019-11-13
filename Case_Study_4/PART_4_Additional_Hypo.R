##### Additional Hypothesis ###############

data <- read.table("o-ring-erosion-or-blowby.data.txt",header = TRUE)
data

launch_temp <- data$X66
o_rings <- data$X6
num_distress <-data$X0

plot (launch_temp,num_distress)
fit<- lm(num_distress ~ launch_temp)
abline(fit)

b <- fit$coefficients[[1]]
slope <- fit$coefficients[[2]]

b
slope
#log.num_distress <- log(num_distress)

#lot(launch_temp, log.num_distress, main = "Launch temp vs Log(# experiencing thermal distress)")
#fit <- lm(log.num_distress ~ launch_temp)
#abline(fit)
#b <- fit$coefficients[[1]]
#slope <- fit$coefficients[[2]]
#slope


#corr <- cor(launch_temp, log.num_distress)

resi <- residuals(fit)
resi
fit$residuals
plot(launch_temp, fit$residuals, main ="R")
plot(resi, main = "Residuals", type ='h', ylab="Standard Residual", xlab ="Interval")
hist(fit$residuals, main ="Residual Histogram")


library(caret)

k <- 5
n <- 23
flds<-createFolds(num_distress,5)
cv.mse <- rep(0, k)
for (round in 1:5){
  test.idx<-flds[[round]]
  y <- num_distress[-test.idx]
  x <- launch_temp[-test.idx]
  fit <- lm(y ~ x)
  y.hat <- predict(fit, data.frame(x = launch_temp[test.idx]))
  cv.mse[round] <- sum((num_distress[test.idx] - y.hat)^2)/length(test.idx)
}
mean(cv.mse)



cv.degree.d <- function(k, n, d){
  flds<-createFolds(num_distress,5)
  cv.mse <- rep(0, k)
  for (round in 1:5){
    test.idx<-flds[[round]]
    y <- num_distress[-test.idx]
    x <- launch_temp[-test.idx]
    fit <- lm(y ~ poly(x, d, raw=TRUE))
    y.hat <- predict(fit, data.frame(x = launch_temp[test.idx]))
    cv.mse[round] <- sum((num_distress[test.idx] - y.hat)^2)/length(test.idx)
  }
  return (mean(cv.mse))
}

#cross validation
k <- 5
n <- 23
d.max <- 9
mse <- rep(0, d.max)
for (d in 1:d.max){
  mse[d] <- cv.degree.d(k, n, d)
}
plot(1:d.max, mse, xlab = "Degree", ylab = "MSE", lwd = 2, col = "blue", pch = 5)
lines(1:d.max, mse, type='l', lwd = 2, col = "blue")

mse



fit.best <- lm(num_distress ~ poly(launch_temp, 3, raw=TRUE))
pts <- seq(0, 600, length.out=100)
#val.d1 <- predict(fit.d1, data.frame(hp=pts))
val.best <- predict(fit.best, data.frame(launch_temp=pts))
plot(launch_temp, num_distress, pch=16)
#lines(pts, val.d1, col="blue", lwd=2)
lines(pts, val.best, col="red", lwd=2)


qqnorm(fit.best$residuals)
qqline(fit.best$residuals, col = "red")


resi.best <- residuals(fit.best)
resi.bets
fit.best$residuals
plot(launch_temp, fit.best$residuals, main ="R")
plot(resi.best, main = "Residuals", type ='h', ylab="Standard Residual", xlab ="Interval")
hist(fit.best$residuals, main ="Residual Histogram")







hist(resi.best, col = 3, main="Histogram of Residuals")
hist(num_distress)



plot(fit.best$fitted.values, fit.best$residuals, main = "Residual Plot")
abline(0,0, col="red")

plot(resi.best, main = "Residual Plot(num_distress)", ylab ="Standard Residual")
abline(0,0, col="red")


plot(resi.best, main = "Residuals(num_distress", type ='h', ylab="Standard Residual", xlab ="Interval")




#Scenrio 2

fit <- fit.best
summary(fit)
plot(launch_temp,num_distress, main = "launch_temp vs num_distress")
abline(fit)

#Confidence Interval
new.den = seq(min(launch_temp),max(launch_temp),by = 0.05)
CI <- predict(fit, newdata=data.frame(launch_temp=new.den), interval="confidence",
              level = 0.95)
lines(new.den, CI[,2], col="blue", lty=2)
lines(new.den, CI[,3], col="blue", lty=2)

#Prediction Intervals 
pred.int <- predict(fit, newdata=data.frame(launch_temp = new.den), interval="prediction",
                    level = 0.95)
pred.int
lines(new.den, pred.int[,2], col="orange", lty=2)
lines(new.den, pred.int[,3], col="orange", lty=2)

#Predict launch_temp 

predict.densities <- function(num_distress) {
  b <- fit$coefficients[[1]]
  slope <- fit$coefficients[[2]]
  pred.value <- (log(num_distress)-b)/slope
}

print(predict.densities(70))

#Predict launch_temp Intervals


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
  
  sse <- sum( (y - y.fitted)^2)
  mse <- sse / (size - 2)
  
  #95% quantile value 
  t.val <- qt(0.975, size - 2)
  #standard error of mean estimate
  
  mean.se <- (1 / size + (x.pred - mean(x))^2 / (sum((x - mean(x))^2)))
  pred.se <- (1 + (1 / size) + (x.pred - mean(x))^2 / (sum((x - mean(x))^2)))
  
  mean.CI.upper <- given.y + t.val*sqrt(mse*mean.se)
  mean.CI.lower <- given.y - t.val*sqrt(mse*mean.se)
  
  #Upper/lower launch_temp
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


print (predict.densities(100))
