
str(ToothGrowth)

## Preliminary exploration

geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), width = 0.2)
ebars
ebars +
  geom_point(aes(x = dose, y = len),
             data = subset(ToothGrowth, supp == "VC"),
             col = "blue")
ebars +
  geom_point(aes(x = dose, y = len),
             data = subset(ToothGrowth, supp == "VC"),
             col = "orange")
xyplot(len ~ supp | factor(dose), data = ToothGrowth)
xyplot(len ~ supp | factor(dose), data = ToothGrowth)
xyplot(len ~ dose | supp, data = ToothGrowth)
xyplot(len ~ dose | supp, data = ToothGrowth, grid = TRUE)
fm4 <- lm(len ~ 0 + supp, data = ToothGrowth)
fm4
mean(ToothGrowth$len)
mean(ToothGrowth$len[ToothGrowth$supp == "OJ"])
with(ToothGrowth, mean(len[supp == "OJ"]))
xtabs(~ supp, ToothGrowth)
xtabs(len ~ supp, ToothGrowth)
xtabs(len ~ supp, ToothGrowth) / xtabs(~ supp, ToothGrowth)
split(ToothGrowth, ~supp)
sdf <- split(ToothGrowth, ~supp)
str(sdf)
for (d in sdf) print(mean(d$len))
lapply(sdf, function(d) mean(d$len))
split(ToothGrowth, ~supp + dose)
sdf <- split(ToothGrowth, ~supp + dose)
str(sdf)
sdf <- split(ToothGrowth, ~ supp + dose)
lapply(sdf, function(d) mean(d$len))
lapply(sdf, function(d) c(mean = mean(d$len), median = median(d$len))
)
tapply(ToothGrowth, ~ supp + dose, function(d) mean(d$len))
getCI <- function(y) {
  fm <- lm(y ~ 1)
  predict(fm, newdata = data.frame(z = 1),
          interval = "confidence")
}
getCI(ToothGrowth$len)
tapply(ToothGrowth, ~ supp + dose, function(d) getCI(d$len))
res <- tapply(ToothGrowth, ~ supp + dose, function(d) getCI(d$len))
str(res)
res[1, 1]
array2DF(res)
res <- tapply(ToothGrowth, ~ supp + dose, function(d) getCI(d$len))
str(res)
getCI <- function(y) {
  fm <- lm(y ~ 1)
  predict(fm, newdata = data.frame(z = 1),
          interval = "confidence") |> as.data.frame()
}
res <- tapply(ToothGrowth, ~ supp + dose, function(d) getCI(d$len))
array2DF(res)
df_ci <- array2DF(res)
str(df_ci)

## Systematic modeling + prediction

library(lattice)

xyplot(len ~ dose | supp, data = ToothGrowth)
xyplot(len ~ supp | factor(dose), data = ToothGrowth)
qq(supp ~ len | factor(dose), ToothGrowth)

len_1_VC <- subset(ToothGrowth, 
                   dose == 1 & supp == "VC")$len
tt <- t.test(len_1_VC)
tt$conf.int

## Alternative using lm(response ~ predictors)

fm1 <- lm(len_1_VC ~ 1)
fm1
summary(fm1)

fm2 <- lm(len ~ 1, data = ToothGrowth, 
          subset = (dose == 1 & supp == "VC"))
summary(fm2)
predict(fm2, newdata = data.frame(dose = 1, supp = "VC"))

predict(fm2, 
        newdata = data.frame(dose = 1, supp = "VC"),
        interval = "confidence")
## compare with
tt$conf.int 

predict(fm2, 
        newdata = data.frame(dose = 1, supp = "VC"),
        interval = "predict")

fm3 <- lm(len ~ 1 + dose, data = ToothGrowth, 
          subset = (supp == "VC"))
summary(fm3)

dd3 <- data.frame(dose = c(0.5, 1, 2), supp = "VC")

dd3 <- 
  cbind(dd3, 
        predict(fm3, 
                newdata = dd3,
                interval = "confidence"))

library(ggplot2)

ebars <- 
  ggplot(dd3) + geom_point(aes(x = dose, y = fit)) + 
    geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), width = 0.2)

ebars

## overlay on top of plot of data

ebars + 
  geom_point(aes(x = dose, y = len), 
             data = subset(ToothGrowth, supp == "VC"),
             col = "orange")

## separate mean for all combinations

fm4 <- lm(len ~ 0 + supp, data = ToothGrowth)

getCI <- function(y) {
  fm <- lm(y ~ 1)
  predict(fm, newdata = data.frame(z = 1), 
          interval = "confidence") |> as.data.frame()
}

res <- tapply(ToothGrowth, ~ supp + dose, function(d) getCI(d$len))
df_ci <- array2DF(res)
df_ci <- dplyr::mutate(df_ci, dose = as.numeric(dose))

ggplot(df_ci) + facet_wrap(~ supp) + 
  geom_point(aes(x = dose, y = fit)) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), width = 0.2) + 
  geom_point(aes(x = dose, y = len), 
             data = ToothGrowth,
             col = "orange")

fm5 <- lm(len ~ 0 + interaction(dose, supp), data = ToothGrowth)
fm5$coefficients 

dd5 <- expand.grid(dose = c(0.5, 1, 2), supp = c("OJ", "VC"))

dd5 <- 
  cbind(dd5, 
        predict(fm5, 
                newdata = dd5,
                interval = "confidence"))

ggplot(dd5) + facet_wrap(~ supp) + 
  geom_point(aes(x = dose, y = fit)) + 
  geom_line(aes(x = dose, y = fit)) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), width = 0.2) + 
  geom_point(aes(x = dose, y = len), 
             data = ToothGrowth,
             col = "orange")



## linear regression by subgroup

fm6 <- lm(len ~ 1 + dose * supp, data = ToothGrowth)
dd6 <- expand.grid(dose = c(0.5, 1, 2), supp = c("OJ", "VC"))

dd6 <- 
  cbind(dd6, 
        predict(fm6, 
                newdata = dd6,
                interval = "confidence"))

ggplot(dd6) + facet_wrap(~ supp) + 
  geom_point(aes(x = dose, y = fit)) + 
  geom_line(aes(x = dose, y = fit)) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), width = 0.2) + 
  geom_point(aes(x = dose, y = len), 
             data = ToothGrowth,
             col = "orange")


## model with transformed predictor

fm7 <- lm(len ~ 1 + log(dose) * supp, data = ToothGrowth)
dd7 <- expand.grid(dose = seq(0, 3, by = 0.1), supp = c("OJ", "VC"))

dd7 <- 
  cbind(dd7, 
        predict(fm7, 
                newdata = dd7,
                interval = "confidence"))

ggplot(dd7) + facet_wrap(~ supp) + 
  geom_point(aes(x = (dose), y = fit)) + 
  geom_line(aes(x = (dose), y = fit)) + 
  geom_errorbar(aes(x = (dose), ymin = lwr, ymax = upr), width = 0.2) + 
  geom_point(aes(x = (dose), y = len), 
             data = ToothGrowth,
             col = "orange")

## model with log-transformed response

fm8 <- lm(log(len) ~ 1 + log(dose) * supp, data = ToothGrowth)
dd8 <- expand.grid(dose = seq(0, 3, by = 0.1), supp = c("OJ", "VC"))

dd8 <- 
  cbind(dd8, 
        predict(fm8, 
                newdata = dd8,
                interval = "confidence"))

ggplot(dd8) + facet_wrap(~ supp) + 
  geom_point(aes(x = (dose), y = fit)) + 
  geom_line(aes(x = (dose), y = fit)) + 
  geom_errorbar(aes(x = (dose), ymin = lwr, ymax = upr), width = 0.2) + 
  geom_point(aes(x = (dose), y = log(len)), 
             data = ToothGrowth,
             col = "orange")


## A similar plot using lattice

library(lattice)
library(latticeExtra)

segplot(dose ~ lwr + upr | supp, data = dd8, 
        horizontal = FALSE, center = fit) + 
    xyplot(log(len) ~ dose | supp, ToothGrowth, grid = TRUE, 
         jitter.x = TRUE, pch = 16, col = "orange")


## Continuous data example

library(NHANES)

nhsub <- subset(NHANES, Age >= 21 & Age < 80) # Race as well?
nhsub <- nhsub[c("BPSysAve", "Age", "Gender")]
str(nhsub)

xyplot(BPSysAve ~ Age | Gender, data = nhsub, grid = TRUE, alpha = 0.2)

fm9 <- lm(BPSysAve ~ 1 + Age, data = nhsub)

## Questions: 
## - How can we calculate and display error bounds
## - Is it important to fit different lines for males and females
## - Is a "linear" function appropriate?

udf <- expand.grid(Age = seq(21, 79, by = 1), 
                   Gender = unique(nhsub$Gender), 
                   KEEP.OUT.ATTRS = FALSE)

pdf9 <- cbind(udf, 
              predict(fm9, newdata = udf, 
                      interval = "confidence"))

ggplot(pdf9) + facet_wrap(~ Gender) + 
  geom_point(aes(x = Age, y = BPSysAve), data = nhsub, 
             alpha = 0.2, cex = 0.5) + 
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), 
                width = 0, col = "blue")

## Separate lines for males and females

fm10 <- lm(BPSysAve ~ 1 + Age * Gender, data = nhsub)

## fm10 <- lm(BPSysAve ~ 1 + factor(Age) * Gender, data = nhsub)


pdf10 <- cbind(udf, 
              predict(fm10, newdata = udf, 
                      interval = "confidence"))

ggplot(pdf10) + facet_wrap(~ Gender) + 
  geom_point(aes(x = Age, y = BPSysAve), data = nhsub, 
             alpha = 0.2, cex = 0.5) + 
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), 
                data = pdf9,
                width = 0, col = "yellow") + 
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), 
                width = 0, col = "red")

anova(fm10)

## add covariate to fit two separate linear 
## regression models for Age <= 40 and Age > 40

fm11 <- lm(BPSysAve ~ 1 + Age * Gender * cut(Age, c(20, 40, 80)), 
           data = nhsub)

pdf11 <- cbind(udf, 
               predict(fm11, newdata = udf, 
                       interval = "confidence"))

ggplot(pdf11) + facet_wrap(~ Gender) + 
  geom_point(aes(x = Age, y = BPSysAve), data = nhsub, 
             alpha = 0.2, cex = 0.5) + 
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), 
                data = pdf10,
                width = 0, col = "yellow") + 
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), 
                width = 0, col = "red")

ggplot(pdf11) + facet_wrap(~ Gender) + 
  geom_point(aes(x = Age, y = BPSysAve), data = nhsub, 
             alpha = 0.2, cex = 0.5) + 
  geom_line(aes(x = Age, y = fit), 
                data = pdf10,
                width = 0, col = "yellow") + 
  geom_line(aes(x = Age, y = fit), 
                width = 0, col = "red")


## instead make the curve quadratic

fm12 <- lm(BPSysAve ~ 1 + poly(Age, 2) * Gender,
           data = nhsub)

pdf12 <- cbind(udf, 
               predict(fm12, newdata = udf, 
                       interval = "confidence"))

ggplot(pdf12) + facet_wrap(~ Gender) + 
  geom_point(aes(x = Age, y = BPSysAve), data = nhsub, 
             alpha = 0.2, cex = 0.5) + 
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), 
                data = pdf10,
                width = 0, col = "blue", alpha = 0.5) +
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), 
                width = 0, col = "red", alpha = 0.5)



