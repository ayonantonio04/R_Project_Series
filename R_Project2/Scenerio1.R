#Scenerio 1

mean.time <- mean(data$time)
mean.time
sd.time <- sd(data$time)

sqrt.n = sqrt(length(data$time))

#Function CLT
z_under <- sd.time / sqrt.n 
clt <- function(u, x_bar, z_un) {
  z_top <- x_bar - mean.time
  z.val <- z_top / z_under 
}

#For loop all Z values 
val <- vector()
i <- 0
u <- mean.time
z_un <- z_under
z.result <- vector()
for (i in 1:length(data$time)) {
  val[i] <- data$time[i]
  z.result[i] <- clt(u, val[i], z_un)

}
print(z.result)
#Regular Histogram
hist(z.result, main = "Histogram Central Limit Theorem")

#Histogram w/ Density
hist(z.result, breaks =20, probability = TRUE, density = 50,col = 3, border = 3)
lines(density(z.result, bw = 1), col = 1)
lines(density(z.result, bw = 3), col = 2)
rug(z.result)

summary(z.result)



#scenerio 1 - CLT & Bootstrap

played.ind <- which(data['time'] != 0)
print(length(played.ind))
data.played <- data[played.ind,]
print(data.played)


mean.played <- mean(data.played$time)
print(mean.played)

sd.played <- sd(data.played$time)

sqrt.n.played = sqrt(length(data.played$time))
print(sqrt.n.played)

#Function CLT
z_under.played <- sd.played / sqrt.n.played 
clt <- function(u, x_bar, z_un) {
  z_top <- x_bar - mean.time
  z.val <- z_top / z_under 
}

#For loop all Z values 
val <- vector()
i <- 0
u <- mean.played
z_un <- z_under.played
z.played <- vector()
for (i in 1:length(data.played$time)) {
  val[i] <- data.played$time[i]
  z.played[i] <- clt(u, val[i], z_un)
  
}
print(z.played)
hist(z.played, breaks = 50, main = "Histogram: Central Limit Theorem ")

hist(z.played, breaks =50, probability = TRUE, density = 25,col = 3, border = 3)
lines(density(z.played, bw = 6), col = 1)
lines(density(z.played, bw = 2), col = 2)
rug(z.played)

mean(z.played)

#Bootstraopping for only those who played 

boot.sample.played <- rep(data.played$time, length.out = 34)
length(boot.sample.played)

sample1.played <- sample(boot.sample.played, size = 15, replace = FALSE)

B= 200
boot.allsam <- array(dim = c(B, 15))
for (i in 1:B) {
  boot.allsam[i,] <- sample(boot.sample.played, size = 15, replace =FALSE)
  
}

boot.mean.played <- apply(X= boot.allsam, MARGIN =1, FUN = mean)
head(boot.mean.played)
print(boot.mean.played)

boot.sd.played <- sd(boot.mean.played)
mean.played + c(-1, 1)*1.96*boot.sd.played


hist(boot.mean, main= "Histogram: Bootstrap Played", breaks = 15, probability = TRUE, density = 20, col = 3, border = 3)
lines(density(boot.mean, adjust = 2), col = 2)
