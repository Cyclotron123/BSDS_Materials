library(plotly)


data <- read.csv("Q2_data.csv")

logistic_sigma <- function(z) {
  1 / (1 + exp(-z))
}

x1_data <- data$x1
x2_data <- data$x2
y_data <- data$y

beta0 <- 0
betx1 <- 0

logistic_loss_function <- function(b0, b1) {
  xtB <- x1_data * b0 + x2_data * b1
  sum(log(1 + exp(-y_data * (xtB))))
}

logistic_gradient_function <- function(b0, b1) {
  xtB <- x1_data * b0 + x2_data * b1
  grad_b0 <- -sum(y_data * x1_data * (1 - logistic_sigma(y_data * xtB)))
  grad_b1 <- -sum(y_data * x2_data * (1 - logistic_sigma(y_data * xtB)))
  c(grad_b0, grad_b1)
}

logistic_gradient_magnitude <- function(b0, b1) {
  sqrt(logistic_gradient_function(b0, b1)[1]^2 + logistic_gradient_function(b0, b1)[2]^2)
}

eta_logistic <- 0.05
eps_logistic <- 10^(-5)
iter_logistic <- 0

beta0_vals <- c()
betx1_vals <- c()
logistic_loss_vals <- c()
logistic_iteration_index <- c()

while (logistic_gradient_magnitude(beta0, betx1) >= eps_logistic) {
  beta0_vals <- c(beta0_vals, beta0)
  betx1_vals <- c(betx1_vals, betx1)
  logistic_loss_vals <- c(logistic_loss_vals, logistic_loss_function(beta0, betx1))
  logistic_iteration_index <- c(logistic_iteration_index, iter_logistic)
  
  beta0 <- beta0 - logistic_gradient_function(beta0, betx1)[1] * eta_logistic
  betx1 <- betx1 - logistic_gradient_function(beta0, betx1)[2] * eta_logistic
  
  iter_logistic <- iter_logistic + 1
}

print(paste("Optimal Beta = ", beta0, ",", betx1))
print(paste("Number of Iterations = ", iter_logistic))

plot_ly(x = beta0_vals, y = betx1_vals, z = logistic_loss_vals, marker = list(size = 2))
