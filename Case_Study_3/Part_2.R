
#===========PART 2 =============#
#===Location and Spacing =====#

#--------------Location of Palindromes ------# 
intervals <- 10 
intervals <- 57
intervals <- 30

expected.val <- rep(n/intervals, intervals)
expected.val
observed.val1 <- table(cut(sample1, breaks = seq(0, N, length.out = intervals+1),
                           include.lowest = TRUE))
observed.val2 <- table(cut(sample2, breaks = seq(0, N, length.out = intervals+1),
                           include.lowest = TRUE))
head(observed.val1, intervals)
head(observed.val2, intervals)

#Observed sample 1 values 
obs1 <- as.vector(observed.val1)
obs1

#Observed sample 2 values 
obs2 <- as.vector(observed.val2)
obs2

#Sample 1 QQ plot
qqplot(data$location, sample1, main = "QQ-Plot: Sample 1" )
abline(c(0,1))
#Histo sample 1 and location

#Sample 2 QQ plot 
qqplot(data$location, sample2, main = "QQ-Plot: Sample 2")
abline(c(0,1))

#Chi-squared & Residuals sample 1
E.val <- n/intervals
p.E <- rep(E.val/n, intervals)
chisq.test(obs1,p=p.E)

residuals <- (obs1 - E.val) / sqrt(E.val)
residuals
plot(residuals, type = 'h', ylab = "standard residuals", xlab = "interval",main ="Residual Plot Sample1")

#Chi-squared & Residuals 2 
residuals <- (obs2-E.val) / sqrt(E.val)
residuals
plot(residuals, type = 'h', ylab = "standard residuals", xlab = "interval", main ="Residual Plot Sample2")

E.val <- n/intervals
p.E <- rep(E.val/n, intervals)
chisq.test(obs2,p=p.E)


#========= Spacing Between Palindromes ===#


n <- 296
N <- 229354

#=================Consecutive values 
loc <- data$location
location.sort = sort(loc, decreasing =FALSE)
single.dist <- abs(diff(location.sort, lag =1))
single.dist
n <- n-1

#Rate for exponential: consecutive 
rate <- 1/mean(single.dist)
exponential <- rexp(n, rate)
exponential

#Histogram for plot 
hist(single.dist, breaks = 15,probability = TRUE, col = rgb(1,0,0, 0.5), xlab="Single Distance Palindrome", main = "Consecutive Palindromes")
hist(exponential, breaks = 15,probability = TRUE,col = rgb(0,1,0, 0.5), add = TRUE)
lines(density(single.dist, adjust=), col = rgb(1,0,0, 0.5))
lines(density(exponential, adjust=2), col = rgb(0,1,0, 0.5))
legend(x= 4500, y = 0.0008, legend = c("Original" , "Exponential"), lty = c(1,1), col =c(rgb(1,0,0, 0.5), rgb(0,1,0, 0.5)))

#Chi -Squared Goodness Fit
dexp(n, rate)
observed <- single.dist

p.E <- rep(dexp(n, rate), n)
chisq.test(observed, p = p.E, rescale.p = TRUE)
chisq.test(observed)


#============= Pair Value 
loc.pair <- abs(diff(location.sort, lag = 2))
loc.pair



rate <- 1 / mean(loc.pair)
rate
exponential <- rexp(n-1, rate)
exponential

#Histogram for plot 
hist(loc.pair, breaks = 15,probability = TRUE, col = rgb(1,0,0, 0.5), xlab="Pair Distance Palindrome", main = "Pair Palindromes")
hist(exponential, breaks = 15,probability = TRUE,col = rgb(0,1,0, 0.5), add = TRUE)
lines(density(single.dist, adjust=), col = rgb(1,0,0, 0.5))
lines(density(exponential, adjust=2), col = rgb(0,1,0, 0.5))
legend(x= 4500, y = 0.0004, legend = c("Original" , "Exponential"), lty = c(1,1), col =c(rgb(1,0,0, 0.5), rgb(0,1,0, 0.5)))

#Chi-Squared Goodness Fit Test
observed <- loc.pair
n <- n-1
p.E <- rep(dexp(n, rate), n)
chisq.test(observed, p = p.E, rescale.p = TRUE)


#============Triplets 

loc.triplets <- abs(diff(location.sort, lag = 3))
loc.triplets
rate <- 1 / mean(loc.triplets)

#Gamma distribution 
gamma <- rgamma(n-2, 2,rate)
hist(loc.triplets, breaks = 15,probability = TRUE, col = rgb(1,0,0, 0.5), xlab="Triplet Distance Palindrome", main = "Triplet Palindromes")
hist(gamma, breaks = 15,probability = TRUE,col = rgb(0,1,0, 0.5), add = TRUE)
lines(density(loc.triplets, adjust=), col = rgb(1,0,0, 0.5))
lines(density(gamma, adjust=2), col = rgb(0,1,0, 0.5))
legend(x= 6000, y = 0.0002, legend = c("Original" , "Gamma"), lty = c(1,1), col =c(rgb(1,0,0, 0.5), rgb(0,1,0, 0.5)))

#Chi-square G. fit of triplets 
observed <- loc.triplets 
n <- n -1
chisq.test(observed)


