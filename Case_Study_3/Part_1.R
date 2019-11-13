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
hist(sample1 , breaks = 75, col = 3, main = "Uniform distribution 1")
#lines (density(sample, adjust = 2), col = 2)

sample2 <- runif(n, min = 0, max = N)
hist(sample2 , breaks = 75, col = 3, main = "Uniform distribution 2")
#lines (density(sample, adjust = 2), col = 2)


hist(data$location , breaks = 75, col = 2, main = "Original Data")


#==============Histogram: Spacing between Palindromes ========# 
set.seed(2017)

#calculate the difference of hist
diff.og <- diff(data$location)
hist(diff.og, col=2, main = "Original Data: Spacing Between Palindrome" )


diff.sample <- abs(diff(sample1))
hist(diff.sample,xlab= "Spacing Difference", main = "Sample1: Spacing Between Palindromes ")

diff.sample <- abs(diff(sample2))
hist(diff.sample,xlab= "Spacing Difference", main = "Sample2: Spacing Between Palindromes ")

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
hist(data.count,breaks = 10, col = 2, main = "Palindromes Non-Overlapping: Original Data")

sample.count <- regionsplit(n.region, gene,sample1)
hist(sample.count,breaks = 10, main = "Palindromes Non-Overlapping: Sample 1")

sample.count <- regionsplit(n.region, gene,sample2)
hist(sample.count, breaks = 10, main = "Palindromes Non-Overlapping: Sample 2")

