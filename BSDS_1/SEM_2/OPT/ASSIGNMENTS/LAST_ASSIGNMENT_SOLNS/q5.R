library(plotly)


rosenbrock_f <- function(x, y) {
  (1 - x)^2 + 100 * (y - x^2)^2
}

rosenbrock_gradf <- function(x, y) {
  grad_x <- -2 * (1 - x) - 400 * x * (y - x^2)
  grad_y <- 200 * (y - x^2)
  c(grad_x, grad_y)
}

rosenbrock_norm_gradf <- function(x, y) {
  sqrt(rosenbrock_gradf(x, y)[1]^2 + rosenbrock_gradf(x, y)[2]^2)
}

x0_rosenbrock <- -1
y0_rosenbrock <- 1

eta_rosenbrock <- 0.001
eps_rosenbrock <- 10^(-6)

x_rosenbrock_vals <- c()
y_rosenbrock_vals <- c()
rosenbrock_loss_vals <- c()
rosenbrock_iteration_index <- c()

iter_rosenbrock <- 0

while (rosenbrock_norm_gradf(x0_rosenbrock, y0_rosenbrock) >= eps_rosenbrock) {
  x_rosenbrock_vals <- c(x_rosenbrock_vals, x0_rosenbrock)
  y_rosenbrock_vals <- c(y_rosenbrock_vals, y0_rosenbrock)
  rosenbrock_loss_vals <- c(rosenbrock_loss_vals, rosenbrock_f(x0_rosenbrock, y0_rosenbrock))
  
  x0_rosenbrock <- x0_rosenbrock - rosenbrock_gradf(x0_rosenbrock, y0_rosenbrock)[1] * eta_rosenbrock
  y0_rosenbrock <- y0_rosenbrock - rosenbrock_gradf(x0_rosenbrock, y0_rosenbrock)[2] * eta_rosenbrock
  
  iter_rosenbrock <- iter_rosenbrock + 1
}
print(paste("Optimal x: ", x0_rosenbrock, "  Optimal y: ", y0_rosenbrock))
print(paste("Number of iterations: ", iter_rosenbrock))

plot_ly(x = x_rosenbrock_vals, y = y_rosenbrock_vals, z = rosenbrock_loss_vals, marker = list(size = 2), mode = "lines+markers")
