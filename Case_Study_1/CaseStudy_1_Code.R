
############################# CASE STUDY 1 ###################################

#################### Scenario 1 ###################

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
hist(data.smoker$bwt,  col = rgb(1,1,0,0.5), add = TRUE)
legend(x = 150, y = 130, legend = c("Nonsmoker", "Smoker"), lty = c(1,1), col = c(rgb(0,0,1,0.5), rgb(1,1,0,0.5)))

#Boxplot model
boxplot(data.smoker$bwt, data.nonsmoker$bwt,data.unknown$bwt)
title(main = "Boxplot   smoker,    nonsmoker,     unknown" )

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




################## Scenario 2 ##############################
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



################# Scenerio 3 ##############################

data 
summary(data) 
smoker.ind <- which(data['smoke'] == 1) 
nonsmoker.ind <- which(data['smoke'] == 0)


data.smoker <- data[smoker.ind,] 
data.nonsmoker <- data[nonsmoker.ind, ] 
smoker.wt.mean <- mean(data.smoker$wt) 
smoker.wt.mean 
nonsmoker.wt.mean<- mean(data.nonsmoker$wt) 
nonsmoker.wt.mean

hist(data.smoker$age) 
hist(data.nonsmoker$age)

hist(data.smoker$wt)
hist(data.nonsmoker$wt)

boxplot(wt~smoke, data) 
qqnorm(data.smoker$wt) # q-q plot against normal distribution 
qqline(data.nonsmoker$wt)

rmarkdown::render("analysis.R") 
rmarkdown::render("analysis.R", "pdf_document")

# additional analysis
library(readr) 
dataset <- read_csv("natl2017.csv") 
View(dataset)

dataset

#dataset$dbwt 
#plot 
(dataset$dbwt)

#hist(dataset$dbwt)
features <- dataset[,c("f_cigs_1","f_cigs_2","f_cigs_3","dbwt", "sex") ,drop = FALSE]

smoke.ind <- which(features['f_cigs_1'] == 1 & features['f_cigs_2'] == 1 & features['f_cigs_3'] == 1) 
smoke_bw_m_f <- features[smoke.ind, ]

hist(smoke_bw_m_f$dbwt) 
mean(smoke_bw_m_f$dbwt) 
summary(smoke_bw_m_f$dbwt) 
sd(smoke_bw_m_f$dbwt)

smoke_bw_m_f

male.ind <- which(smoke_bw_m_f['sex'] == 'M')
male.ind 
female.ind<- which(smoke_bw_m_f['sex'] == 'F')
female.ind

smoke_bw_m <- smoke_bw_m_f[male.ind, ]
smoke_bw_f <- smoke_bw_m_f[female.ind, ]

hist(smoke_bw_m$dbwt, col = "cadetblue1") 
box()
hist(smoke_bw_f$dbwt, col = "pink") 
box()

hist(smoke_bw_m$dbwt, col = "cadetblue1")
hist(smoke_bw_f$dbwt, col = "pink", add = T)
box()

legend("topright",legend = c ("male", "female"), col = c("cadetblue1","pink"), lty = 1:2, cex = 0.8)
mean(smoke_bw_m$dbwt) 
mean(smoke_bw_f$dbwt)

sd(smoke_bw_m$dbwt) 
sd(smoke_bw_f$dbwt)
dataset



