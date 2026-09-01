# clean workspace
rm(list=ls())

library(confintr) # package for confidence intervals
library(ggplot2)

sample <- c(51.1, 46.2, 43.6, 51.6, 46.4, 51.2, 47.7, 48.9,
             47.5, 46.9, 47.3, 47.2, 45.1, 50.2, 50.8, 46.1,
             47.4, 47.5, 48.7, 48.1, 45.5, 47.1, 49.7, 50.4,
             47.8)

hist(sample, freq=TRUE)

mean(sample)
sd(sample)


ci = ci_mean(sample, probs = c(0.025, 0.975),
             type = c("t"))

print(ci)


# generate N confidence intervals from samples of size n

i <- 1
N <- 20  # number of experiments
n <- 5  # sample size
mean <- rep(0, N)  # vector of zeros and size N
L <- rep(0, N)
U <- rep(0, N)

for (i in 1:N) {
  # generate normally distributed sample of size n
  rsample <- rnorm(n, mean = 50, sd = 5)  
  # calculate confidence interval
  ci = ci_mean(rsample, probs = c(0.025, 0.975),
               type = c("t"))
  mean[i] <- ci$estimate
  L[i] <- ci$interval[1]
  U[i] <- ci$interval[2]
}

data <- data.frame(x = 1:N,
                   y = mean,
                   low = L,
                   up = U)

ggplot(data, aes(x, y)) + geom_point() + 
       geom_errorbar(aes(ymin = low, ymax = up))
# 
# p
# 
# p + labs(x = "Intervalos")
# p + labs(title = "Intervalos de Confian?a 95%")
