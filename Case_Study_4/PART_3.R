
#Scenerio 3

#Omitting all densities 0.508 from oG data 
length(data$gain)
data_row <- which(data$density == 0.508)
data_row
data_omit <- data[-c(21:30), ]
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

