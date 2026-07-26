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
