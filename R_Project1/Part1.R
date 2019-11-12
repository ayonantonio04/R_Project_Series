#################### Scenario 1 ###########


install.packages("moments") 
library(moments)

data <- babies 
data23 <- babies23

#Data separation smokers/nonsmokers/unknown 
smoker <- which(data['smoke'] == 1) 
nonsmoker <- which(data['smoke'] == 0) 
unknown <- which(data['smoke'] == 9)

#Data weight smokers 
data.smoker <- data[smoker,] 
hist(data.smoker$bwt, main = "Histogram: Smokers - Baby Birth Weight") 
summary(data.smoker$bwt) 
sd(data.smoker$bwt)


#Data Weight nonsmokers 
data.nonsmoker <- data[nonsmoker,] 
hist(data.nonsmoker$bwt, main = "Histogram: Nonsmokers - Baby Birth Weight") 
summary(data.nonsmoker$bwt) 
sd(data.nonsmoker$bwt)

#Data weight unknown 
data.unknown <- data[unknown,] 
hist(data.unknown$bwt, main ="Histogram: Unknown - Baby Birth Weight")

#Combined Histogram 
hist(data.nonsmoker$bwt, col = rgb(0,0,1,0.5),main = "Combined smoker vs nonsmoker Histogram", xlab = "data categories") 
hist(data.smoker$bwt, col = rgb(1,1,0,0.5), add = TRUE)
legend(x = 150, y = 130, legend = c("Nonsmoker", "Smoker"), lty = c(1,1), col = c(rgb(0,0,1,0.5), rgb(1)))

#Boxplot model 
boxplot(data.smoker$bwt, data.nonsmoker$bwt,data.unknown$bwt) 
title(main = "Boxplot smoker, nonsmoker, unknown" )

#QQplot Comparison smoker -nonsmoker 
qqplot(data.smoker$bwt, data.nonsmoker$bwt, main = "Quantile-Quantile Plot") 
abline(c(0,1))

#Testing Norm - Formal Way
kurtosis(data.smoker$bwt) 
skewness(data.smoker$bwt)

kurtosis(data.nonsmoker$bwt) 
skewness(data.nonsmoker$bwt)

#CLT 
set.seed(1) 
smok.sample <- sample(data.smoker$bwt, 200, replace = FALSE) 
hist(smok.sample, main = "CLT: Smoker Data")

set.seed(2) 
nonsmk.sample <- sample(data.nonsmoker$bwt, 200, replace = FALSE) 
hist(nonsmk.sample, main = "CLT: Non-smoker Data")

                                                                                                                                                                                                           
        