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
n.region <- 57
N<- 228000

n.region <- 40 #5733 base pairs 
N <- 229354

n.region <- 100
N <- 229354


gene <- seq(1,N)
original <- regionsplit(n.region, gene, loc)
original
lambda <- 5.16
observed.loc <- table(cut(loc, breaks = seq(0, N, length.out = n.region+1),
                          include.lowest = TRUE))
count <- as.vector(observed.loc)
count
hist(count, breaks = 15, probability = TRUE, col = rgb(1,0,0,0.5), main = "Histogram Count: 100 Sub-Intervals" )
pois.dist <- rpois(n, lambda)
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
site.pois.stats <- sum((site.pois.tab[,2] - site.pois.tab[,1])^2/site.pois.tab[,2])
pchisq(site.pois.stats, 7 - 2, lower.tail=FALSE)

site.pois.stats

