
################################# CASE STUDY 3 #######################################################

############# PART 1 ###########################

data <- hcmv.263hxkx.1qhtfgz
# ==========loc distribution ====================#

library(lattice)
stripplot(data$location, pch = 16, cex = 0.25)
x <- data.frame(data$location,1) ## 1 is your "height" 
plot(x, type = 'o', pch = '|', ylab = '', main = "Original Data: Location of Palindromes")

#random sample
N <- 229354
n <- 296
set.seed(312)
gene <- seq(1, N)

#scatter plot 
set.seed(2017)
sample1 <- runif(n, min = 0, max = N)
sample2 <- runif(n, min = 0, max = N)

x <- data.frame(sample1,1) ## 1 is your "height" 
plot(x, type = 'o', pch = '|', ylab = '', main = "Sample 1: Location of Palindromes")
x <- data.frame(sample2,1) ## 1 is your "height" 
plot(x, type = 'o', pch = '|', ylab = '', main = "Sample 2: Location of Palindromes")

#=======================Distributions Samples ==================# 
set.seed(2017)

sample1 <- runif(n, min = 0, max = N)
hist(sample1 , breaks = 75, col = 3, main = "Sample 1: Uniform distribution")
#lines (density(sample, adjust = 2), col = 2)

sample2 <- runif(n, min = 0, max = N)
hist(sample2 , breaks = 75, col = 3, main = "Sample2: Uniform distribution")
#lines (density(sample, adjust = 2), col = 2)


hist(data$location , breaks = 75, col = 2, main = "Original Data")


#==============Histogram: Spacing between Palindromes ========# 
set.seed(2017)


#calculate the difference of hist
diff.og <- abs(diff(data$location))
hist(diff.og, col=2, main = "Original Data: Spacing Between Palindrome", xlab= "Spacing Difference" )


diff.sample <- abs(diff(sample1))
hist(diff.sample,xlab= "Spacing Difference", main = "Sample1: Spacing Between Palindromes ", col = 3)

diff.sample <- abs(diff(sample2))
hist(diff.sample,xlab= "Spacing Difference", main = "Sample2: Spacing Between Palindromes ", col = 3)

#================= Count ===========================#

#
regionsplit <- function(n.region, gene, site){
  count.int <- table(cut(site, breaks = seq(1, length(gene), length.out=n.region+1), include.lowest=TRUE))
  count.vector <- as.vector(count.int)
  count.tab <- table(count.vector)
  return (count.vector)
}



n.region <- 100
data.count <- regionsplit(n.region, gene,data$location)
hist(data.count,breaks = 10, col = 2, main = "Palindromes Non-Overlapping: Original Data", xlab = "Count of Palindromes")

sample.count <- regionsplit(n.region, gene,sample1)
hist(sample.count, col = 3, breaks = 10, main = "Palindromes Non-Overlapping: Sample 1", xlab = "Count of Palindromes")

sample.count <- regionsplit(n.region, gene,sample2)
hist(sample.count, col = 3, breaks = 10, main = "Palindromes Non-Overlapping: Sample 2", xlab = "Count of Palindromes")




#===========PART 2 =============#
#===Location and Spacing =====#

#--------------Location of Palindromes ------# 

#Sample 1 QQ plot
qqplot(data$location, sample1, main = "QQ-Plot: Sample 1", xlab = "Location Data" )
abline(c(0,1))
#Histo sample 1 and location

#Sample 2 QQ plot 
qqplot(data$location, sample2, main = "QQ-Plot: Sample 2", xlab = "Location Data")
abline(c(0,1))


#New Uniform plot
set.seed(2018)
sample3 <- runif(n, min = 0, max = N)
qqplot(data$location, sample3, main = "QQ-Plot: Sample 3", xlab = "Location Data")
abline(c(0,1))


#Sub-interval list
intervals = c(25, 30, 45, 57)

#All Residual Calculations and Plots 
for (vals in intervals) {
  expected.val <- rep(n/vals, vals)
  
  observed.loc <- table(cut(data$location, breaks = seq(0, N, length.out = vals+1),
                            include.lowest = TRUE))
  observed.val1 <- table(cut(sample3, breaks = seq(0, N, length.out = vals+1),
                             include.lowest = TRUE))
  #Observed location data values 
  obs1 <- as.vector(observed.loc)
  
  obs2 <- as.vector(observed.val1)
  
  
  residuals <- (obs1 - obs2) / sqrt(obs2)
  residuals
  
  #Residual values plot
  plot(residuals, type = 'h', ylab = "standard residuals", xlab = "interval",main =paste("Residual Plot Location Data: Interval ", vals))
  
}



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

#Exponential Distribution
set.seed(2019)
exponential <- rexp(n, rate)
exponential

#Histogram for plot 
hist(single.dist, breaks = 15,probability = TRUE, col = rgb(1,0,0, 0.5), xlab="Single Distance Palindrome", main = "Consecutive Palindromes")
hist(exponential, breaks = 15,probability = TRUE,col = rgb(0,1,0, 0.5), add = TRUE)
lines(density(single.dist, adjust=), col = rgb(1,0,0, 0.5))
lines(density(exponential, adjust=2), col = rgb(0,1,0, 0.5))
legend(x= 3000, y = 0.0008, legend = c("Original" , "Exponential"), 
       lty = c(1,1), col =c(rgb(1,0,0, 0.5), rgb(0,1,0, 0.5)))


#t-test both distributions
t.test(single.dist, exponential)

#Additional Test
num = sum(single.dist)
expected.prop = exponential/sum(exponential)
chi2 = sum((single.dist - num*expected.prop)^2 /(num*expected.prop))
p_val = pchisq(chi2, df=3, lower.tail = FALSE)
p_val

chisq.test(single.dist, p = expected.prop)

#============= Pair Value 
loc.pair <- abs(diff(location.sort, lag = 2))
loc.pair

#Exponetial Distribution
rate <- 1 / mean(loc.pair)
rate
set.seed(2020)
exponential <- rexp(n-1, rate)
exponential

#Histogram for plot 
hist(loc.pair, breaks = 15,probability = TRUE, col = rgb(1,0,0, 0.5), xlab="Pair Distance Palindrome", main = "Pair Palindromes")
hist(exponential, breaks = 15,probability = TRUE,col = rgb(0,1,0, 0.5), add = TRUE)
lines(density(single.dist, adjust=), col = rgb(1,0,0, 0.5))
lines(density(exponential, adjust=2), col = rgb(0,1,0, 0.5))
legend(x= 4500, y = 0.0004, legend = c("Original" , "Exponential"), lty = c(1,1), col =c(rgb(1,0,0, 0.5), rgb(0,1,0, 0.5)))

#Comparison Test
t.test(loc.pair, exponential)



#============Triplets 

loc.triplets <- abs(diff(location.sort, lag = 3))
loc.triplets
rate <- 1 / mean(loc.triplets)

#Gamma distribution 
gamma <- rgamma(n-2, 2,rate)

#Histograms 
hist(loc.triplets, breaks = 15,probability = TRUE, col = rgb(1,0,0, 0.5), xlab="Triplet Distance Palindrome", main = "Triplet Palindromes")
hist(gamma, breaks = 15,probability = TRUE,col = rgb(0,1,0, 0.5), add = TRUE)
lines(density(loc.triplets, adjust=), col = rgb(1,0,0, 0.5))
lines(density(gamma, adjust=2), col = rgb(0,1,0, 0.5))
legend(x= 6000, y = 0.0002, legend = c("Original" , "Gamma"), lty = c(1,1), col =c(rgb(1,0,0, 0.5), rgb(0,1,0, 0.5)))

#Comparison Test
t.test(loc.triplets, gamma)



############### PART 3 ##################


#######chi squared Non Overlapping ##########



chisqtable <- function(n.region, site, N){
  n <- length(site)
  # lambda estimate
  lambda.est <- n/n.region
  # cut into n.region number of non-overlapping intervals
  count.int <- table(cut(site, breaks = seq(1, length(gene), length.out=n.region+1), include.lowest=TRUE))
  # get the count levels range
  count.vector <- as.vector(count.int)
  count.range <- max(count.vector) - min(count.vector) + 1
  # create contingency table
  table <- matrix(rep(NA, count.range*3), count.range, 3)
  for (i in 1:count.range){
    offset <- min(count.vector) - 1
    # first column = count level
    table[i, 1] <- i + offset
    # second column = observed count
    table[i, 2] <- sum(count.vector == i + offset)
    # third column = expected count
    if ((i + offset == min(count.vector)) && (min(count.vector) != 0))
      table[i, 3] <- ppois(i+offset, lambda.est)*n.region
    else if (i + offset == max(count.vector))
      table[i, 3] <- (1 - ppois(i + offset - 1, lambda.est))*n.region
    else
      table[i, 3] <- (ppois(i+offset, lambda.est) - ppois(i + offset - 1, lambda.est))*n.region
  }
  return (table)
}



#======================= Scenerio 3 Count ========
#N<- 228000
N <- 229354

c.int <- c(40, 57, 100)

for (n.region in c.int) {
  gene <- seq(1,N)
  original <- regionsplit(n.region, gene, loc)
  original
  lambda <- 5.16
  observed.loc <- table(cut(data$location, breaks = seq(0, N, length.out = n.region+1),
                            include.lowest = TRUE))
  
  #Random Poisson Distribution
  set.seed(2021)
  pois.dist <- rpois(n, lambda)
  
  count <- as.vector(observed.loc)
  count
  hist(count, breaks = 15, probability = TRUE, col = rgb(1,0,0,0.5), main = paste("Histogram Count: Sub-Intervals", n.region))
  hist(pois.dist, breaks = 15, col = rgb(0,0,1,0.5), probability = TRUE, add = TRUE)
  legend(x = 8, y = 0.15, legend = c("Observed", "Poisson"), lty = c(1,1),
         col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
  
  #Chi-squared goodness Fit 
  
  site.pois <- chisqtable(n.region, loc, N)
  site.pois
  
  site.pois.tab <- matrix(rep(NA, 7*2), 7, 2)
  site.pois.tab[1,] <- colSums(site.pois[1:2, 2:3])
  site.pois.tab[2:6,] <- site.pois[3:7, 2:3]
  site.pois.tab[7,] <- colSums(site.pois[8:12, 2:3])
  site.pois.stats <- sum((site.pois.tab[,2] - site.pois.tab[,1])^2/site.pois.tab[,1])
  print(pchisq(site.pois.stats, df= 5, lower.tail=FALSE))
  
  
} 



################### PART 4 #########################


loc = data$location
N <- 229354

c.int <- seq(20, 80)
chi2_vals <- c()
lambda.vec <- c()

for (n.region in c.int) {
  gene <- seq(1,N)
  original <- regionsplit(n.region, gene, loc)
  original
  
  observed.loc <- table(cut(data$location, breaks = seq(0, N, length.out = n.region+1),
                            include.lowest = TRUE))
  
  count <- as.vector(observed.loc)
  lambda.calc <- MASS::fitdistr(count, "Poisson")
  lambda.vec[n.region] <- lambda.calc$estimate
  
  
  #Chi-squared goodness Fit 
  
  site.pois <- chisqtable(n.region, loc, N)
  site.pois
  
  site.pois.tab <- matrix(rep(NA, 7*2), 7, 2)
  site.pois.tab[1,] <- colSums(site.pois[1:2, 2:3])
  site.pois.tab[2:6,] <- site.pois[3:7, 2:3]
  site.pois.tab[7,] <- colSums(site.pois[8:12, 2:3])
  site.pois.stats <- sum((site.pois.tab[,2] - site.pois.tab[,1])^2/site.pois.tab[,1])
  chi2_vals[n.region] <- pchisq(site.pois.stats, df= 5, lower.tail=FALSE)
  
  
} 

sort(chi2_vals)
which(chi2_vals < 0.05)
chi2_vals[which(chi2_vals < 0.05)]
lambda.vec[which(chi2_vals < 0.05)]


mean.count <- mean(count)




