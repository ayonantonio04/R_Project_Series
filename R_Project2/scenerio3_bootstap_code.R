#Bootstrapping 

boot.sample <- rep(data$time, length.out = 91)
length(boot.sample)

sample1 <- sample(boot.sample, size = 30, replace = FALSE)

B= 200
boot.allsam <- array(dim = c(B, 30))
for (i in 1:B) {
  boot.allsam[i,] <- sample(boot.sample, size = 30, replace =FALSE)
  
}

boot.mean <- apply(X= boot.allsam, MARGIN =1, FUN = mean)
head(boot.mean)
print(boot.mean)

hist(boot.mean, breaks = 15, probability = TRUE, density = 20, col = 3, border = 3)
lines(density(boot.mean, adjust = 2), col = 2)