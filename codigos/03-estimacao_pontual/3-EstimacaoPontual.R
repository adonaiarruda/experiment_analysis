# clean workspace
rm(list=ls())

# make this example reproducible
set.seed(0)

# create random variable with sample size of 1000 that is uniformally distributed
data <- runif(n=1000, min=2, max=6)

# data <- rexp(n=1000, rate=2.2)  # exponential distribution


mean(data)  # mean of sample 
# create histogram to visualize distribution of sampled data
hist(data, col='steelblue', main='Histogram of data')



# take N random samples of size n from a distribution
means <- c() # empty vector
N = 1000  # change N to larger values to see the CLT result
n = 100
for (i in 1:N){
  means[i] = mean(runif(n, min = 2, max = 6))  # uniform
  # means[i] = mean(rexp(n, rate=2.2))  # exponential
}

#create histogram to visualize sampling distribution of sample means
hist(means, col ='steelblue', xlab='observed means')

mean(means)  # mean of the N observed means.

