

## data preparation and summarization

library(NHANES)

## Task 1: Subset / filter

nhsub <- subset(NHANES, Age >= 21)
nhsub <- nhsub[c("Race1", "Age", "Gender", "Height", "Weight")]

## other methods
## - subset()

## The above is equivalent to:

nhsub <- subset(NHANES, 
                subset = Age >= 21, 
                select = c("Race1", "Age", "Gender", "Height", "Weight"))


nhsub <- subset(NHANES, 
                subset = Age >= 21, 
                select = c(Race1, Age, Gender, Height, Weight))

## dplyr - grammar for data manipulation

## - dplyr::filter()
## - dplyr::select()

nhsub <- dplyr::filter(NHANES, Age >= 21)
nhsub <- dplyr::select(nhsub, c("Race1", "Age", "Gender", "Height", "Weight"))

## - pipeline

subset(NHANES, 
       subset = Age >= 21, 
       select = c("Race1", "Age", "Gender", "Height", "Weight")) |> str()


NHANES |>
  dplyr::filter(Age >= 21) |> 
  dplyr::select(c(Race1, Age, Gender, Height, Weight)) |>
  str()

NHANES |>
  subset(subset = Age >= 21) |>
  subset(select = c(Race1, Age, Gender, Height, Weight)) |>
  str()

## Another efficient approach to all these things: data.table package 
## (which we will not discuss)

## Task 2: Add derived variables

## Example: calculate BMI = kg / m^2

## - dplyr::mutate()

NHANES |>
  dplyr::filter(Age >= 21) |> 
  dplyr::select(c(Race1, Age, Gender, Height, Weight)) |>
  dplyr::mutate(HeightM = Height / 100, 
                BMI = Weight / HeightM^2) |>
  dplyr::select(!Height) |>
  str() |>
  system.time()

## doin the same thing stepwise

nhsub1 <- 
  NHANES |>
  dplyr::filter(Age >= 21) |> 
  dplyr::select(c(Race1, Age, Gender, Height, Weight))

nhsub2 <- 
  nhsub1 |>
  dplyr::mutate(HeightM = Height / 100, 
                BMI = Weight / HeightM^2)

str(nhsub)

## - transform()

NHANES |>
  subset(Age >= 21) |> 
  subset(select = c(Race1, Age, Gender, Height, Weight)) |>
  transform(HeightM = Height / 100) |> 
  transform(BMI = Weight / HeightM^2) |>
  str()
## |> system.time()

## - within()

NHANES |>
  subset(Age >= 21) |> 
  subset(select = c(Race1, Age, Gender, Height, Weight)) |>
  within({
    HeightM <- Height / 100
    BMI <- Weight / HeightM^2
    rm(Height)
  }) |>
  str()
## |> system.time()

## Task 3: summarize by groups

## Example: Find mean BMI, median BMI, normal CI, 
##   correlation between height and weight

nhsub <- 
  NHANES |>
    subset(Age >= 21) |> 
    subset(select = c(Race1, Age, Gender, Height, Weight)) |>
    transform(HeightM = Height / 100) |> 
    transform(BMI = Weight / HeightM^2)

str(nhsub)  

## - tapply()

tt <- xtabs(~ Race1 + Gender + cut(Age, c(20, 40, 60, 100)), nhsub)
tt[1:2, 1:2, 1:2]

xtabs(~ Race1 + Gender + cut(Age, c(20, 40, 60, 100)), nhsub) |> array2DF()

## - dplyr::group_by()
## - dplyr::summarise()

nhsub |>
  dplyr::group_by(Race1, Gender, AgeGrp = cut(Age, c(20, 40, 60, 100))) |> 
  dplyr::summarize(length(BMI))

## calculate mean

nhsub |>
  dplyr::group_by(Race1, Gender) |> 
  dplyr::summarize(mean(BMI, na.rm = TRUE))

tapply(nhsub, ~ Race1 + Gender, function(d) mean(d$BMI, na.rm = TRUE)) |> 
  array2DF()

tapply(nhsub, ~ Race1 + Gender, with, mean(BMI, na.rm = TRUE)) |> array2DF()

## calculate median

nhsub |> dplyr::group_by(Gender, Race1) |> dplyr::summarize(median(BMI, na.rm = TRUE))
tapply(nhsub, ~ Race1 + Gender, with, median(BMI, na.rm = TRUE)) |> array2DF()

## confidence interval

t_confint <- function(x) {
  ci <- t.test(x)$conf.int
  data.frame(lower = ci[1], upper = ci[2])
}

nhsub |> dplyr::group_by(Gender, Race1) |> dplyr::summarize(t_confint(BMI))
tapply(nhsub, ~ Race1 + Gender, with, t_confint(BMI)) |> array2DF()

## correlation 

corr <- function(x, y) {
  cor(x, y, use = "pairwise.complete")
}

nhsub |> dplyr::group_by(Gender, Race1) |> dplyr::summarize(Corr = corr(HeightM, Weight))
tapply(nhsub, ~ Race1 + Gender, with, corr(HeightM, Weight)) |> array2DF()

library(lattice)
tapply(nhsub, ~ Race1 + Gender, with, corr(HeightM, Weight)) |> array2DF() |> 
  dotplot(Race1 ~ Value, groups = Gender, auto.key = list(columns = 2),
          xlab = "Correlation between height and weight")

tapply(nhsub, ~ Race1 + Gender, with, mean(BMI, na.rm = TRUE)) |> array2DF() |> 
  dotplot(reorder(Race1, Value) ~ Value, groups = Gender, auto.key = list(columns = 2),
          xlab = "Mean BMI")

## Why use with?

## Non-standard evaluation

nhsub <- tibble::as_tibble(nhsub)

## non-standard evaluation
nhsub2 <- dplyr::group_by(nhsub, Gender, Race1)
dplyr::summarize(nhsub2, median(BMI, na.rm = TRUE))

## tapply does not have NSE

split(nhsub, ~ Race1 + Gender) |> str()
split(nhsub, ~ Race1 + Gender) |> sapply(nrow)
tapply(nhsub, ~ Race1 + Gender, nrow)
tapply(nhsub, ~ Race1 + Gender, nrow) |> array2DF()

meanBMI <- function(df) {
  mean(df$BMI, na.rm = TRUE)
}

tapply(nhsub, ~ Race1 + Gender, meanBMI) |> array2DF()

meanBMI <- function(df) {
  with(df,  mean(BMI, na.rm = TRUE))
}

tapply(nhsub, ~ Race1 + Gender, meanBMI) |> array2DF()
tapply(nhsub, ~ Race1 + Gender, with, mean(BMI, na.rm = TRUE)) |> array2DF()





