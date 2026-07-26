library(plotly)

x1 <- numeric(100) + 1
x2 <- rnorm(100, 1, 2)

target <- rnorm(100, 2 + (3 * x2), 5)

par(mfrow = c(1, 2))
attach(mtcars)

plot(x2, target, col = "red", pch = 20, xlab = "x2", main = "Built-in Linear Model Plot")

lm_model <- lm(target ~ x2)
coef(lm_model)

predictions <- predict(lm_model)
abline(lm_model)

thetx1 <- 0
thetx2 <- 0

learning_rate <- 0.01
convergence_threshold <- 10^(-6)

loss_function <- function(t1, t2) {
  (1 / (2 * 100)) * sum((target - (x1 * t1 + x2 * t2))^2)
}

gradient_function <- function(t1, t2) {
  grad_t1 <- -(1 / 100) * sum((target - (x1 * t1 + x2 * t2)) * x1)
  grad_t2 <- -(1 / 100) * sum((target - (x1 * t1 + x2 * t2)) * x2)
  c(grad_t1, grad_t2)
}

gradient_magnitude <- function(t1, t2) {
  grad_vals <- gradient_function(t1, t2)
  sqrt(sum(grad_vals^2))
}

iter_count <- 0
thetx1_vals <- c()
thetx2_vals <- c()
loss_vals <- c()
iteration_index <- c()

while (gradient_magnitude(thetx1, thetx2) >= convergence_threshold) {
  thetx1_vals <- c(thetx1_vals, thetx1)
  thetx2_vals <- c(thetx2_vals, thetx2)
  loss_vals <- c(loss_vals, loss_function(thetx1, thetx2))
  iteration_index <- c(iteration_index, iter_count)
  
  thetx1 <- thetx1 - learning_rate * gradient_function(thetx1, thetx2)[1]
  thetx2 <- thetx2 - learning_rate * gradient_function(thetx1, thetx2)[2]
  
  iter_count <- iter_count + 1
}

print(c(thetx1, thetx2))
print(paste("Number of Iterations = ", iter_count))

plot(x2, target, col = "blue", pch = 20, xlab = "x2", main = "Gradient Descent (Custom Implementation)")
abline(thetx1, thetx2)

plot_ly(x = thetx1_vals, y = thetx2_vals, z = loss_vals, marker = list(size = 2))
