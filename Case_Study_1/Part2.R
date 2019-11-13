
################## Low Birth Weight ###### 
#smoker low weight 
low.birth <- which(data.smoker["bwt"] <= 88) 
smoker.low.weight <- data.smoker[low.birth,] 
smoker.low.weight 
hist(smoker.low.weight$bwt, main = "Histogram: Low Birth Smokers")


summary(smoker.low.weight$bwt) 
sd(smoker.low.weight$bwt) 
#QQplot smoker 
qqnorm(smoker.low.weight$bwt) 
qqline(smoker.low.weight$bwt)


#nonsmoker low weight 
low.birthnon <- which(data.nonsmoker['bwt'] <= 88) 
nonsmoker.low.weight <- data.nonsmoker[low.birthnon,] 
nonsmoker.low.weight 
hist(nonsmoker.low.weight$bwt, main = "HIstogram: Low Birth Nonsmokers") 
summary(nonsmoker.low.weight$bwt) 
sd(nonsmoker.low.weight$bwt) 
#qqplot nonsmoker 
qqnorm(nonsmoker.low.weight$bwt)
qqline(nonsmoker.low.weight$bwt)

qqplot(smoker.low.weight$bwt, nonsmoker.low.weight$bwt )
abline(c(0,1))

#Hypothesis Testing 
smoker.prop <- mean(data.smoker$bwt <= 88) 
nonsmoker.prop <- mean(data.nonsmoker$bwt <= 88)

smoker.prop90 <- mean(data.smoker$bwt <= 90) 
smoker.prop90 
nonsmoker.prop90 <- mean(data.nonsmoker$bwt <= 90) 
nonsmoker.prop90


t = (smoker.prop-0.5)/sqrt(0.5^2/484)
t 
t.non = (nonsmoker.prop-0.5)/sqrt(0.5^2/743)
t.non 

#MonteCarlo

set.seed(3) 
B = 400
mc.sample <- array(dim = c(B, 484)) 
for (i in 1:B) {
  mc.sample[i, ] <- rbinom(484,1,0.5)}
  mc.mean <- apply(X = mc.sample, MARGIN = 1, FUN = mean)
  mc.tstat<-(mc.mean-0.5)/sqrt(0.5^2/484)
  p_value=sum(mc.tstat>=t)/400
  p_value
  
  
set.seed(4) 
B = 400 
mc.sample <- array(dim = c(B, 742)) 
for (i in 1:B) {
  mc.sample[i, ] <- rbinom(742,1,0.5)}
  mc.mean <- apply(X = mc.sample, MARGIN = 1, FUN = mean) 
  mc.tstat<-(mc.mean-0.5)/sqrt(0.5^2/742) 
  p_value=sum(mc.tstat>=t)/400
  p_value
  
  
