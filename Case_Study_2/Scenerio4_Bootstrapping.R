#Bootstrapping 

#Like to play videogames 
like.ind <- which((data['like'] != 5) & (data['like'] != 1))
data.like <- data[like.ind,]
data.like$like

#Not like to play videogames 
notlike.ind <- which((data['like'] == 1) | (data['like'] == 5))
notlike.ind
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


