
library(graphics)
library(lattice)
library(ggplot2)

plot(y1 ~ x1, data = anscombe, pch = 16)
xyplot(y1 ~ x1, data = anscombe, pch = 16)
ggplot(data = anscombe) + geom_point(aes(x = x1, y = y1))

par(mfrow = c(2, 2))
plot(y1 ~ x1, data = anscombe, pch = 16)
plot(y2 ~ x2, data = anscombe, pch = 16)
plot(y3 ~ x3, data = anscombe, pch = 16)
plot(y4 ~ x4, data = anscombe, pch = 16)

anscombe.long <- 
  with(anscombe,
       data.frame(x = c(x1, x2, x3, x4), 
                  y = c(y1, y2, y3, y4),
                  which = rep(c("1", "2", "3", "4"), each = 11)))

xyplot(y ~ x | which, data = anscombe.long, pch = 16, grid = TRUE)

ggplot(data = anscombe.long) + facet_wrap(~ which) + geom_point(aes(x = x, y = y))



for (w in c("1", "2", "3", "4")) {
  plot(y ~ x, data = anscombe.long, pch = 16, subset = (which == w))
  abline(lm(y ~ x, data = anscombe.long, subset = (which == w)))
}

ggplot(data = anscombe.long) + facet_wrap(~ which) +
  geom_smooth(aes(x = x, y = y), method = "lm", se = FALSE) + 
  geom_point(aes(x = x, y = y))

xyplot(y ~ x | which, data = anscombe.long, pch = 16,
       panel = panel.lmline)

displayFunction <- function(x, y) {
  panel.grid(h = -1, v = -1)               ## add a reference grid
  panel.points(x, y, pch = 16)             ## draw the points
  panel.lmline(x, y, col = "grey50")       ## draw linear regression line
}

xyplot(y ~ x | which, data = anscombe.long, pch = 16,
       panel = displayFunction)



