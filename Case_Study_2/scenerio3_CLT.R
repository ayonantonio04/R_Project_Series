#Scenerio 3

mean.time <- mean(data$time)
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
hist(z.result, breaks = 50,main = "Histogram Central Limit Theorem: Time")

#Histogram w/ Density
hist(z.result, breaks =50, probability = TRUE, density = 50,col = 3, border = 3)
lines(density(z.result, bw = 1), col = 1)
lines(density(z.result, bw = 3), col = 2)
rug(z.result)





