library(plotly)


X <- read.csv("Q4_data.csv")
x_data <- X$x

gaussian_f <- function(u, s) {
  sum(log(s) + ((x_data - u)^2) / (2 * s^2))
}

gaussian_gradf <- function(u, s) {
  grad_u <- - (1 / s^2) * sum(x_data - u)
  grad_s <- (1 / s^3) * sum(s * 2 - (x_data - u)^2)
  c(grad_u, grad_s)
}

gaussian_norm_gradf <- function(u, s) {
  sqrt(gaussian_gradf(u, s)[1]^2 + gaussian_gradf(u, s)[2]^2)
}

eta_gaussian <- 0.01
eps_gaussian <- 10^(-5)

u0_gaussian <- 0
s0_gaussian <- 1

iter_gaussian <- 0

u_vals <- c()
s_vals <- c()
gaussian_loss_vals <- c()
gaussian_iteration_index <- c()

while (gaussian_norm_gradf(u0_gaussian, s0_gaussian) >= eps_gaussian) {
  u_vals <- c(u_vals, u0_gaussian)
  s_vals <- c(s_vals, s0_gaussian)
  gaussian_loss_vals <- c(gaussian_loss_vals, gaussian_f(u0_gaussian, s0_gaussian))
  
  u0_gaussian <- u0_gaussian - gaussian_gradf(u0_gaussian, s0_gaussian)[1] * eta_gaussian
  s0_gaussian <- s0_gaussian - gaussian_gradf(u0_gaussian, s0_gaussian)[2] * eta_gaussian
  
  iter_gaussian <- iter_gaussian + 1
}

print(paste("Optimal u: ", u0_gaussian, "  Optimal s: ", s0_gaussian))
print(paste("Number of iterations: ", iter_gaussian))

plot_ly(x = u_vals, y = s_vals, z = gaussian_loss_vals, marker = list(size = 2), mode = "lines+markers")
