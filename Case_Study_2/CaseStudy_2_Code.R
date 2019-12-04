############### CASE STUDY 2 ########################################


## Data Variables 
N = 314
n=91
data = videodata
data[data == 99] = NA
head(data)

#SCENARIO 1

#Scenerio 1 (part1)

# Percentage played 
perc.played = length(which(data$time !=0))/n

#Confidence interval with CLT
standard.error = sqrt(perc.played * (1-perc.played)/n)
lower = perc.played - (qnorm(0.975))*standard.error
upper = perc.played + (qnorm(0.975))*standard.error
c(lower, upper)

#CI through bootstrap (Part 2)


set.seed(1)
boot.pop = rep(data$time != 0, length.out = N)
length(boot.pop)
sample1 = sample(boot.pop, size = n, replace = FALSE)
set.seed(1)
B=1000
boot.means = array(dim = c(B,91)) 

for(b in 1:B) {
  boot.means[b,] = sample(boot.pop, size =n, replace = FALSE)
}

boot.mean.vals = apply(X=boot.means, MARGIN = 1, FUN = mean)
head(boot.mean.vals)

avg.boot.mean = mean(boot.mean.vals)
avg.boot.mean

#Part 3
lower = unname(quantile(boot.mean.vals, 0.025))
upper = unname(quantile(boot.mean.vals, 0.975))
c(lower, upper)

hist(boot.mean.vals, main = 'Bootstrapped Sample Means: B=1000', xlab = 'Students who Played', col = 3)

perc.played
##################################################################
#SCENARIO 2

# Scenerio 2 (part 1)
#Comparing freq with time played 
group1 = data$time[which(data$freq==1)]
g1_mean = mean(group1)
group2 = data$time[which(data$freq==2)]
g2_mean = mean(group2)
group3 = data$time[which(data$freq==3)]
g3_mean = mean(group3)
group4 = data$time[which(data$freq==4)]
g4_mean = mean(group4)

#Plot: Average of frequency playes vs time played 
barplot(c(g1_mean, g2_mean, g3_mean, g4_mean), main = "Time vs Frequency of Play",
        names.arg = c("daily", "weekly", "monthly", "semest."))


# Avg Time of: Frequency and grade coonection
g1.grade = c(mean(data$time[which(data$grade==2 & data$freq==1)]), 
             mean(data$time[which(data$grade==3 & data$freq==1)]),
             mean(data$time[which(data$grade==4 & data$freq==1)]))

g2.grade = c(mean(data$time[which(data$grade==2 & data$freq==2)]), 
             mean(data$time[which(data$grade==3 & data$freq==2)]),
             mean(data$time[which(data$grade==4 & data$freq==2)]))

g3.grade = c(mean(data$time[which(data$grade==2 & data$freq==3)]), 
             mean(data$time[which(data$grade==3 & data$freq==3)]),
             mean(data$time[which(data$grade==4 & data$freq==3)]))

g4.grade = c(mean(data$time[which(data$grade==2 & data$freq==4)]), 
             mean(data$time[which(data$grade==3 & data$freq==4)]),
             mean(data$time[which(data$grade==4 & data$freq==4)]))


##################################################################


#SCENARIO 3


avg.time.played = mean(data$time)

#Confidence Interval with CLT
standard.error = sd(data$time) / sqrt(n)

lower = avg.time.played - (qnorm(0.975)*standard.error)
upper = avg.time.played + (qnorm(0.975)*standard.error)
c(lower, upper)

set.seed(2)
boot.pop = rep(data$time, length.out = N)
length(boot.pop)
sample1 = sample(boot.pop, size = n, replace = FALSE)

B=1000
boot.means = array(dim = c(B,91)) 

for(b in 1:B) {
  boot.means[b,] = sample(boot.pop, size =n, replace = FALSE)
}

boot.mean.vals = apply(X=boot.means, MARGIN = 1, FUN = mean)
head(boot.mean.vals)

avg.boot.mean = mean(boot.mean.vals)
avg.boot.mean

lower = unname(quantile(boot.mean.vals, 0.025))
upper = unname(quantile(boot.mean.vals, 0.975))
c(lower, upper)

hist(boot.mean.vals, main = 'Bootstrapped Sample Means: B=1000', xlab = 'Avg Time PLayed', col = 3)



##################################################################

#SCENARIO 4


#Like to play videogames 
like.ind <- which((data['like'] == 2) | (data['like'] == 3))
data.like <- data[like.ind,]
data.like$like

#Not like to play videogames 
notlike.ind <- which((data['like'] == 4) | (data['like'] == 5))
data.notlike <- data[notlike.ind,]
data.notlike$like

#Sex: male like to play
male.like <- which(data.like['sex'] == 1)
male.like.data <- data.like[male.like,]
male.like.data
#case females 
female.like <- which(data.like['sex'] == 0)
female.like.data <- data.like[female.like,]
female.like.data

#histograms:grades for like to play 
hist()
summary(data.like$freq)

library(rpart)

#Regression tree for students that like to play
set.seed(3)
model = rpart(data.like$like ~., data = data.like)
par(xpd =NA)
plot(model)
text(model, digits = 3)

#Regression tree for all data
set.seed(4)
model = rpart(data$like ~., data = data)
par(xpd =NA)
plot(model)
text(model, digits = 3)

#Type of game percentages
type_game = c(50, 28, 17, 39, 63)
#Percentage reason for playing
reason_playing = c(26, 66, 5, 24, 28, 27)
#Percentages reason not to play
reason_notplaying = c(48, 26, 6, 19, 40, 17, 17, 33)

#Side barplot for reason for playing 
barplot(reason_playing, main = "Reason Why Students Play", horiz = TRUE,
        names.arg = c("Graphics", "Relaxation", "Coordin.", "Mental Chall", "Mastery.", "Bored"),
        cex.names = 0.5, xlab = "Percentage %", col = "lightblue")

#Side barplot for reason not to play
barplot(reason_notplaying, main = "Reason Why Students Don't Play", horiz = TRUE,
        names.arg = c("Time", "Frustrating", "Lonely", "Many Rules", "Cost", "Boring", "Friends", "Pointless"),
        cex.names = 0.5, xlab = "Percentage %", col = "darkblue")



##################################################################


#SCENARIO 5 

#Cross-tables and plots 

install.packages("gmodels")
library(gmodels)

######################################
#Data minus those who never played
data = videodata
data[data == 99] = NA
data.edit = data[which(data$like != 1),]


data.edit$like[data.edit['like'] == 2 | data.edit['like'] == 3] = "like"
data.edit$like[data.edit['like'] == 4 | data.edit['like'] == 5] = "Dislike"

data.edit$work[data.edit$work > 0] = "Work"
data.edit$work[data.edit$work == 0] = "No Work"

data.edit$home[data.edit$home == 0] = "No PC"
data.edit$home[data.edit$home == 1] = "Have PC"

data.edit$sex[data.edit$sex == 0] = "Female"
data.edit$sex[data.edit$sex == 1] = "Male"

data.edit$grade[data.edit$grade == 1] = "D"
data.edit$grade[data.edit$grade == 2] = "C"
data.edit$grade[data.edit$grade == 3] = "B"
data.edit$grade[data.edit$grade == 4] = "A"

#####################################

#Crosstabulation
#Like vs Gender 
CrossTable(data.edit$like, data.edit$sex)

#Like vs Work
CrossTable(data.edit$like, data.edit$work)

#Gender vs Grade
CrossTable(data.edit$sex, data.edit$grade)

#################################

# Barplots for corresponding cross-tables

#Like vs gender 
count = table(data.edit$like, data.edit$sex)
barplot(count, main = "Like/Not Like Playing Based on Gender",
        xlab = "Number Students",
        col = c("gray8", "red2"),
        legend = rownames(count), beside = TRUE)

#Gender vs Grade
count = table(data.edit$sex, data.edit$grade)
barplot(count, main = "Expected Grades Based On Gender",
        xlab = "Number Students",
        col = c("gray8", "red2"),
        legend = rownames(count), beside = TRUE)

#Like vs Work
count = table(data.edit$like, data.edit$work)
barplot(count, main = "Like/Not Like Playing Based on Hours Worked",
        xlab = "Number Students",
        col = c("gray8", "red2"),
        legend = rownames(count), beside = TRUE)

#Boxplot hrs worked vs like playing 
boxplot(data$work~data$like, main = "Hours Worked vs Like Playing",
        xlab = "Like Playing Category",
        ylab = "Number of Hours Worked", 
        names = c("Never", "Very Much", "Somewhat", "Not Really", "Not at All"))








