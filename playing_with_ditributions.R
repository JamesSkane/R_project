library(dplyr)
library(ggplot2)
library(rgl) 

setwd("...")
getwd()


# 1
norm1 <- rnorm(1000)
#mean(norm1)
#sd(norm1)
norm2 <- rnorm(1000, 8, 4)
#mean(norm2)
#sd(norm2)

par(mfrow=c(1, 2))
d1 <- density(norm1)
d2 <- density(norm2)
plot(d1, type="n",ylim=c(0,.5), xlim=c(-5,20))
polygon(d1, col="red", border="gray")
plot(d2, type="n",ylim=c(0,.5), xlim=c(-5,20))
polygon(d2, col="blue", border="gray")


#2
norm1 <- data.frame(value = rnorm(1000))
norm2 <- data.frame(value = rnorm(1000, 8, 4))

norm1$dist <- 'one'
norm2$dist <- 'two'
names(norm2)
names(norm1)
df <- rbind(norm1, norm2)
head(df)

ggplot(df, aes(x=value, fill=dist)) +
  geom_histogram(position="dodge")+
  theme(legend.position="top")

ggplot(df, aes(x=value, fill=dist)) + 
  geom_histogram(alpha=0.2, position="identity")


# 3 Generate a vector of 100 random normal numbers with
# a mean of 15 and standard deviation of 10, 
# save in a variable named norm3.
norm3 <- rnorm(100, mean=15, sd=10)

# 4 Display norm1 and norm3 with a density plot using ggplot2.
norm3 <- data.frame(value = rnorm(100, mean=15, sd=10))
norm3$dist <- 'three'
df <- rbind(norm1, norm2, norm3)
ggplot(df, aes(value, fill = dist)) + 
  geom_density()

#  Display norm1 and norm3 with a semi-transparent density plot using ggplot2.
ggplot(df, aes(value, fill = dist)) + 
  geom_density(alpha = 0.2)

# 5 
# a) 
# pnorm is the cumulative distribution function
cumfx <- function(v, cutoff){
  pnorm(cutoff, mean(v), sd(v))
}

# b)
cumfx(norm1$value, 2)
cumfx(norm2$value, 2)
# c)
# norm1 has a mean of 0 and a standard deviation of 1. What the cumfx returns is 
# the probability that a values in norm1 is less than 2. The answer we recieve
# is aproximately .97, which make sense given most of the values are between -1 and
# 1 as seen in our previous plots. Conversely,norm2 has a mean of 8 and a standard 
# deviation of 4. This means it has a wider distribution, which is also centered around
# 8. These values mostly fall between 4 and 12, making them far less likely to have
# a value of 2. 

# 6

# Reveiw of density plots shows the bulk of norm 1 falls below 2 and most
# of norm2 falls above 2.
ggplot(filter(df, dist != 'three'), aes(value, fill = dist)) + 
  geom_density(alpha = 0.2) +
  geom_vline(aes(xintercept=2, linetype="Value of 2"))

# The CDF plot also confirms what was previously discussed. This displays 
# the probability of selecting random observation with a value of 2
# from each distribution. 
ggplot(filter(df, dist != 'three'), aes(value, color = dist)) + 
  stat_ecdf() +
  geom_vline(aes(xintercept=2, linetype="Value of 2"))
  

# 7.
#cumulative normal probability for q(area under the normal curve to the 
# left of q) (http://www.statmethods.net/management/functions.html)
pnorm(1) # approximately .84


#normal quantile. value at the p percentile of normal distribution (http://www.statmethods.net/management/functions.html)
# To prove pnorm and qnorm are inverse functions, we can enter the previous output
# as an argument for qnorm. We should expect to have a return value of approximately 1.
qnorm(.84) # This verifies they are inverse functions. 


# 8.
two <- filter(df, dist == 'two') %>% select(value)
dnorm(0, mean(two$value), sd(two$value))

#9.
three <- filter(df, dist == 'three') %>% select(value)
sd(three$value)
dnorm(5, mean(three$value), sd(three$value))
# a)
cumfx(three$value, mean(three$value) + sd(three$value)) - cumfx(three$value, mean(three$value) - sd(three$value))
# b)
cumfx(three$value, mean(three$value) + (2*sd(three$value))) - cumfx(three$value, mean(three$value) - (2*sd(three$value)))
# c)
cumfx(three$value, mean(three$value) + (3*sd(three$value))) - cumfx(three$value, mean(three$value) - (3*sd(three$value)))

# 10.
unif1 <- runif(1000, min=0, max=1)
unif2 <- runif(1000, min=20, max=30)
u1 <- data.frame(value = runif(1000, min=0, max=1))
u2 <- data.frame(value = runif(1000, min=20, max=30))
u1$dist <- 'one'
u2$dist <- 'two'
df2 <- rbind(u1, u2)
head(df2)

# 11.
plot_compare <- function(dat1, dat2){
  par(mfrow=c(1, 2))
  d1 <- density(dat1)
  d2 <- density(dat2)
  plot(d1, type="n")
  polygon(d1, col="red", border="gray")
  plot(d2, type="n")
  polygon(d2, col="blue", border="gray")
}

plot_compare(u1$value,u2$value)

# 12.
# a)  Generate a vector of 1000 random numbers from the Poisson distribution
#with lambda = 1, save in a variable named pois1.
pois1 <- rpois(1000, lambda = 1)

#b) Generate a vector of 1000 random numbers from the Poisson distribution with
# lambda = 30, save in a variable named pois2.
pois2 <- rpois(1000, lambda = 30)


# 13.
#a) Call your function from #11 to create a figure for side-by-side comparison
#of the distributions of pois1and pois2.
plot_compare(pois1,pois2)


p1 <- data.frame(value = rpois(1000, lambda = 1))
p2 <- data.frame(value = rpois(1000, lambda = 30))
p1$dist <- 'one'
p2$dist <- 'two'
df3 <- rbind(p1, p2)
head(df3)
# b) 
# Poisson distributions are used for count data, and lambda signifies both the mean
# and the variance. The PDF plots show the probability of x events happening.
# When lambda is lower, it signifies the event is more rare, and therefore,
# less likely to occur. This is why we see a high density value at 0. As lambda
# increases, it signifies that that we can expect to see occurances of x more frequently. 
# When lambda is 30, we see higher densities towards the middle of the distribution,
# and smaller values become less likely to occur. 

# 14.
n1 <- data.frame(value = rnorm(1000))
u1 <- data.frame(value = runif(1000, min=0, max=1))
p1 <- data.frame(value = rpois(1000, lambda = 1))

n1$dist <- 'normal'
u1$dist <- 'uniform'
p1$dist <- 'poisson'

dist_df <- rbind(n1, u1, p1)

# 15.
normal <- filter(dist_df, dist == 'normal') %>% select(value)
pois <- filter(dist_df, dist == 'poisson') %>% select(value)

plot_compare(normal$value,pois$value)


# 16.

resultset <- group_by(dist_df, dist)

summarize(resultset,
          min.value = min(value, na.rm=T),
          max.value = max(value, na.rm=T),
          mean.value = mean(value, na.rm = T),
          med.value = median(value, na.rm=T),
          sd.value = sd(value, na.rm=T))
#17.

summarize(resultset,
          min.value = min(value, na.rm=T),
          max.value = max(value, na.rm=T),
          mean.value = mean(value, na.rm = T),
          med.value = median(value, na.rm=T),
          sd.value = sd(value, na.rm=T),
          pnorm.value = pnorm(3, mean.value, sd.value))
          
