################################################################################
######  Demonstration file 1 of Statistics II: Introduction to Inference  ######
################################################################################
rm(list = ls())
set.seed(1) # You may set a different seed
library(truncnorm) # install this R-package first
# parameters of the true distribution
mu = 0
sigma = 1
# lower and upper truncation points
a = -2
b = 3
# Taking samples
N.max = 100000 
N = c(100, 1000, 10000, 100000)
y = rtruncnorm(N.max, a = a, b = b, mean = mu, sd = sigma)
# Functions
density.TN = function(x){ return(dtruncnorm(x, a = a, b = b)) }
cdf.TN = function(x){ return(ptruncnorm(x, a = a, b = b)) }
Prop = Avg = rep(0, length(N))
# For each value of i, we have n = N[i] samples
for(i in 1:length(N))
{
  # sample size
  n = N[i]
  # samples
  x = sample(y, n, replace = FALSE)
  par(mfrow = c(1, 2))
  # histogram of the samples with the region of interest (0,1] in purple
  hist_breaks = hist(x, breaks = (10* log(n) + 10), plot = FALSE)$breaks
  color_link = rep("gray", length(hist_breaks))
  color_link[(hist_breaks >= 0)&(hist_breaks <= 1)] = "purple"
  
  hist(x, breaks = (10* log(n) + 10), probability = TRUE, 
       main = paste0("Histogram with n=", n, " samples"), ylim = c(0, 0.5), 
       col = color_link)
  par(new = T)
  # compare it with the population density
  curve(density.TN, from = a, to = b, col = "red", lwd = 2, xlab = " ",
        ylab = " ", ylim = c(0, 0.5))
  
  # empirical cumulative distribution function
  emp.dist <- ecdf(x)
  plot(emp.dist, main = paste0("Empirical CDF with n=", n, " samples"), 
       ylim = c(0,1), xlim = c(-2, 3), cex =.2, lwd = 2)
  par(new = T)
  # compare it with the population CDF
  curve(cdf.TN, from = a, to = b, col = "red", lwd = 2, xlab = " ", ylab = " ",
        ylim = c(0, 1), xlim = c(-2, 3))
  # mean of the samples
  Avg[i] = mean(x^2)
  Prop[i] = length(x[(x > 0) & (x <= 1)])/n
  Sys.sleep(1)
}
True.expectation.X2 = 1 - ((b * dnorm(b) - a * dnorm(a))/(pnorm(b) - pnorm(a)))
True.proportion = (pnorm(1) - pnorm(0))/(pnorm(b) - pnorm(a))
print(paste0("The true expectation is: ",
              round(True.expectation.X2, digits = 4)))
print(paste0("The sample means for sample size ", N, " is ", 
             round(Avg, digits = 4), ", with bias ", 
             round(Avg - True.expectation.X2, digits = 4)))

print(paste0("The true proportion is: ", round(True.proportion, digits = 4)))
print(paste0("The sample proportions for sample size ", N, " is ", 
             round(Prop, digits = 4), ", with bias ", 
             round(Prop - True.proportion, digits = 4)))
################################################################################

################################################################################
######  Demonstration file 2 of Statistics II: Introduction to Inference  ######
################################################################################
rm(list = ls())
set.seed(04022025)
n = 15
R =10000
T1 = T2 = T3 = matrix(data = NA, nrow = 3, ncol =R)
m = 20
pp = c(0.25, 0.5, 0.75)
for(r in 1:R)
{
  for(s in 1:3)
  {
    X = rbinom(n, size = m, prob = pp[s])
    T1[s,r] = mean(X)/m
    T2[s,r] = var(X)/m + (mean(X)^2)/m^2
    T3[s,r] = X[1]/m
  }
}
par(mfrow = c(1,3))
hist(T1[1,], breaks = 50, main = expression(paste("Distribution of T1 when ", 
            theta, " = 0.25")), xlim = c(0,1),
     xlab = "", ylab = "relative frequency", probability = TRUE) 
hist(T1[2,], breaks = 50, main = expression(paste("Distribution of T1 when ", 
                                        theta, " = 0.5")), xlim = c(0,1), 
     xlab = "", ylab = "relative frequency", probability = TRUE)
hist(T1[3,], breaks = 50, main = expression(paste("Distribution of T1 when ", 
                                theta, " = 0.75")), xlim = c(0,1), 
     xlab = "", ylab = "relative frequency", probability = TRUE)


par(mfrow = c(1,3))
hist(T2[1,], breaks = 50, main = expression(paste("Distribution of T2 when ", 
                                                  theta, " = 0.25")), xlim = c(0,1),
     xlab = "", ylab = "relative frequency", freq = FALSE) 
hist(T2[2,], breaks = 50, main = expression(paste("Distribution of T2 when ", 
                                            theta, " = 0.5")), xlim = c(0,1), 
     xlab = "", ylab = "relative frequency", freq = FALSE)
hist(T2[3,], breaks = 50, main = expression(paste("Distribution of T2 when ", 
                                                  theta, " = 0.75")), xlim = c(0,1), 
     xlab = "", ylab = "relative frequency", freq = FALSE)

par(mfrow = c(1,3))
hist(T3[1,], breaks = 50, main = expression(paste("Distribution of T3 when ", 
                                                  theta, " = 0.25")), xlim = c(0,1),
     xlab = "", ylab = "relative frequency", freq = FALSE) 
hist(T3[2,], breaks = 50, main = expression(paste("Distribution of T3 when ", 
                                                  theta, " = 0.5")), xlim = c(0,1), 
     xlab = "", ylab = "relative frequency", freq = FALSE)
hist(T3[3,], breaks = 50, main = expression(paste("Distribution of T3 when ", 
                                                  theta, " = 0.75")), xlim = c(0,1), 
     xlab = "", ylab = "relative frequency", freq = FALSE)

