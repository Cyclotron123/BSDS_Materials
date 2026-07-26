library(stats4)

# Set seed and generate data
set.seed(20251902)
x <- rbeta(100, 2.5, 5)

# Log-likelihood function
log_likelihood <- function(a, b) {
  
  if (a <= 0 || b <= 0) return(-Inf)
  
  
  log_lk <- sum((a - 1) * log(x) + (b - 1) * log(1 - x)) - length(x) * log(beta(a, b))
  return(-log_lk)  
}


mle_result <- mle(log_likelihood, start = list(a = 1, b = 1), 
                  method = "L-BFGS-B", lower = c(1e-6, 1e-6))

mle_a <- coef(mle_result)["a"]
mle_b <- coef(mle_result)["b"]
cat("MLE for a:", mle_a, "\n")
cat("MLE for b:", mle_b, "\n")