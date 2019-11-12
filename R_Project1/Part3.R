######### Scenerio 3 #########

data <- read.table("dataset2.txt",header = TRUE ) 
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








