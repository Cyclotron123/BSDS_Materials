

n <- 5000

z <- logical(n)
for (i in 1:n) {
  A <- rnorm(1); B <- rnorm(1); C <- rnorm(1)
  D <- B^2 - 4 * A * C
  z[i] <- D >= 0
}
p_hat <- sum(z) / n
p_hat


wilson_ci <- function(x, n, k = 4) {
  p_hat <- x / n
  ## Let the quadratic that we need to solve be ap^2 + bp + c
  a <- 1 + k / n
  b <- -(2 * p_hat + k / n)
  c <- p_hat^2
  d <- b^2 - 4 * a * c
  interval <- (-b + c(-1, 1) * sqrt(d)) / (2 * a)
  conf.level <- pnorm(sqrt(k)) - pnorm(-sqrt(k))
  list(interval = interval,
       level = conf.level,
       description = "Wilson CI for proportion",
       p.hat = p_hat,
       n = n)
}

wilson_ci(sum(z), length(z), k = 2^2)


