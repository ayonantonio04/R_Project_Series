#Bootstrapping 
install.packages("gmodels")
library(gmodels)
#Like to play videogames 
like.ind <- which((data['like'] == 2) | (data['like'] == 3))
data.like <- data[like.ind,]
data.like$like

#Not like to play videogames 
notlike.ind <- which((data['like'] != 2) & (data['like'] != 3))
data.notlike <- data[notlike.ind,]
data.notlike$like

#Sex: male like to play
male.like <- which(data.like['sex'] == 1)
male.like.data <- data.like[male.like,]
names(male.like.data)

cro(male.like.data$grade,male.like.data$email)
#case females 
female.like <- which(data.like['sex'] == 0)
female.like.data <- data.like[female.like,]
female.like.data

########################################################

#Sex: Male NOT like to play
male.notlike <- which(data.notlike['sex'] == 1)
male.notlike.data <- data.notlike[male.notlike,]
male.notlike.data
#case female notlike 
female.notlike <- which(data.notlike['sex'] == 0)
female.notlike.data <- data.notlike[female.notlike,]
female.notlike.data

##########################
#case 1 Male vs Female Grades 

#Crosstabulation
names()
CrossTable(data.like$sex, data.like$grade)

CrossTable(data.notlike$sex, data.notlike$grade)
######################

########################################

#Q-Q plot Work hrs
qqnorm(male.like.data$grade)
qqline(male.like.data$grade)

barplot(male.like.data$grade)

A = hist(male.like.data$grade, col="red")
lines(density(male.like.data$grade, bw = 0.25), col = 1)
#Notlike hist
B= hist(male.notlike.data$grade, col="blue")
lines(density(male.notlike.data$grade, bw = 0.25), col = 1)

plot(A,B)

hist(male.like.data$age)
plot(male.like.data$grade,male.like.data$work)

###################################################################
#Work like vs not like play


#Notlike playing: Dont work 
nowork.notlike <- which(data.notlike['work'] == 0)
nowork.notlike.data <- data.notlike[nowork.notlike,]
nowork.notlike.data
#case Not Like: yes work
work.notlike <- which(data.notlike['work'] != 0)
work.notlike.data <- data.notlike[work.notlike,]
work.notlike.data

#Yes like playing: no work
nowork.like <- which(data.like['work'] == 0)
nowork.like.data <- data.like[nowork.like,]
nowork.like.data
#Yes Like: yes work
work.like <- which(data.like['work'] != 0)
work.like.data <- data.like[work.like,]
work.like.data

###################################################
#CrossTab work / no work 

#Nowork
CrossTable(nowork.like.data$grade,nowork.like.data$sex)
CrossTable(nowork.notlike.data$grade, nowork.notlike.data$sex)

#Work
CrossTable(work.like.data$grade, work.like.data$sex)
CrossTable(work.notlike.data$grade, work.notlike.data$sex)

###################################################
#Own a computer OWn + Home

#Yes(1) Home + Own: like to play
PC.like <- which((data.like['home'] == 1) | (data.like['own']==1))
PC.like.data <- data.like[PC.like,]
PC.like.data
#No(0) Home + pC: Like 
noPC.like <- which((data.like['home'] == 0) | (data.like['own']==0))
noPC.like.data <- data.like[noPC.like,]
noPC.like.data

#Yes Home + Own:NOT like
PC.notlike <- which((data.notlike['home'] == 1) | (data.notlike['own']==1))
PC.notlike.data <- data.notlike[PC.notlike,]
PC.notlike.data

#No Home +PC: Not Like 
noPC.notlike <- which((data.notlike['home'] == 0) | (data.notlike['own']==0))
noPC.notlike.data <- data.notlike[noPC.notlike,]
noPC.notlike.data

##############################################
#CrossTabulation

CrossTable(PC.like.data$grade, PC.like.data$sex)
CrossTable(noPC.like.data$grade, noPC.like.data$sex)

CrossTable(PC.notlike.data$grade, PC.notlike.data$sex)
CrossTable(noPC.notlike.data$grade, noPC.notlike.data$sex)

length(PC.like.data)
length(noPC.like.data)
plot(PC.like.data$grade, noPC.like.data$grade)


A= hist(PC.like.data$grade)
B = hist(noPC.like.data$grade)

plot(A, col = "red", main = "Like + Grades: Own Pc vs No PC ")
plot(B, col = "black", add=TRUE)









