library(plotly)

quad_f <- function(x1, x2) {
  2 * x1^2 + 4 * x2^2 - 4 * x1 - 8 * x2
}

quad_gradf <- function(x1, x2) {
  grad_x1 <- 4 * x1 - 4
  grad_x2 <- 8 * x2 - 8
  c(grad_x1, grad_x2)
}

quad_norm_gradf <- function(x1, x2) {
  sqrt(quad_gradf(x1, x2)[1]^2 + quad_gradf(x1, x2)[2]^2)
}

eta_quad <- 0.1
eps_quad <- 10^(-6)

x0_quad <- 0
x1_quad <- 0

iter_quad <- 0

x0_vals <- c()
x1_vals <- c()
quad_loss_vals <- c()
quad_iteration_index <- c()

while (quad_norm_gradf(x0_quad, x1_quad) >= eps_quad) {
  x0_vals <- c(x0_vals, x0_quad)
  x1_vals <- c(x1_vals, x1_quad)
  quad_loss_vals <- c(quad_loss_vals, quad_f(x0_quad, x1_quad))
  quad_iteration_index <- c(quad_iteration_index, iter_quad)
  
  x0_quad <- x0_quad - quad_gradf(x0_quad, x1_quad)[1] * eta_quad
  x1_quad <- x1_quad - quad_gradf(x0_quad, x1_quad)[2] * eta_quad
  
  iter_quad <- iter_quad + 1
}

print(paste("Optimal x: ", x0_quad, ",", x1_quad))
print(paste("Number of iterations: ", iter_quad))

plot_ly(x = x0_vals, y = x1_vals, z = quad_loss_vals, marker = list(size = 2), mode = "lines+markers")
