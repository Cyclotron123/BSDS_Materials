## =============================================================================
## Statistical Learning I -- Session 1
## OLS vs LAD, Robustness, and IRWLS
## BSDS, 3rd Year, Indian Statistical Institute
##
## Companion script to Session1_Manual.pdf (theory) and Session1_Beamer.pdf
## (slides). Every section below is tagged with the step number from the
## master lesson plan and the Act it belongs to.
##
## Run top to bottom. Every figure is first DRAWN LIVE (shows up in the R
## console / RStudio Plots pane -- useful for presenting in class), and then
## immediately saved as a PDF into the working directory, via the pattern:
##    draw_figXX <- function() { ...plotting code... }
##    draw_figXX()                                  # live, on-screen
##    pdf("FigXX....pdf"); draw_figXX(); dev.off()   # saved to disk
## =============================================================================

set.seed(1)

## -----------------------------------------------------------------------
## 0. HELPER FUNCTIONS
## -----------------------------------------------------------------------

## ---- OLS through the origin: Y = beta*X + eps.  Closed form. ----
ols_origin <- function(x, y) sum(x * y) / sum(x^2)

## ---- Weighted median: minimizer of sum_i w_i |v_i - m| ----
weighted_median <- function(values, weights) {
  ord <- order(values)
  v <- values[ord]; w <- weights[ord]
  cw <- cumsum(w) / sum(w)
  v[which(cw >= 0.5)[1]]
}

## ---- LAD through the origin: EXACT solution.
## sum|y_i - b x_i| = sum |x_i| * |y_i/x_i - b|  ==>  weighted median of
## r_i = y_i/x_i with weights w_i = |x_i|.  See Act 1 / Act 3 of the manual. ----
lad_origin <- function(x, y) {
  r <- y / x
  w <- abs(x)
  weighted_median(r, w)
}

## ---- OLS with intercept: normal equations, Y = alpha + beta*X + eps ----
ols_intercept <- function(x, y) {
  X <- cbind(1, x)
  as.numeric(solve(t(X) %*% X, t(X) %*% y))   # c(alpha_hat, beta_hat)
}

## ---- LAD with intercept via IRWLS (Act 5). ----
## theta^(0)      = OLS
## r^(t)          = y - X theta^(t)
## w_i^(t)        = 1 / max(|r_i^(t)|, delta)
## theta^(t+1)    = (X'WX)^{-1} X'Wy      (weighted least squares)
## stop when ||theta^(t+1)-theta^(t)|| < tol
irwls_lad <- function(x, y, tol = 1e-8, maxit = 100, delta = 1e-6, trace = FALSE) {
  X <- cbind(1, x)
  theta <- ols_intercept(x, y)
  path <- matrix(theta, nrow = 1, dimnames = list(NULL, c("alpha", "beta")))
  converged <- FALSE
  for (t in 1:maxit) {
    r <- as.numeric(y - X %*% theta)
    w <- 1 / pmax(abs(r), delta)
    W <- diag(w)
    theta_new <- as.numeric(solve(t(X) %*% W %*% X, t(X) %*% W %*% y))
    path <- rbind(path, theta_new)
    if (trace) {
      cat(sprintf("  iter %2d :  alpha = %8.4f   beta = %8.4f   ||delta|| = %.2e\n",
                   t, theta_new[1], theta_new[2],
                   sqrt(sum((theta_new - theta)^2))))
    }
    if (sqrt(sum((theta_new - theta)^2)) < tol) { theta <- theta_new; converged <- TRUE; break }
    theta <- theta_new
  }
  list(theta = theta, path = path, iters = t, converged = converged)
}

## ---- generic bias / variance / MSE summary for a vector of estimates ----
summarize_estimates <- function(estimates, truth) {
  c(bias     = mean(estimates) - truth,
    variance = var(estimates),
    mse      = mean((estimates - truth)^2))
}


## =============================================================================
## ACT 1  /  STEPS 1-3
## Regression through the origin: Y_i = beta*X_i + eps_i.  Generate one data
## set, compute OLS and LAD, plot true / OLS / LAD lines.
## =============================================================================

beta0 <- 2.5

## NOTE on sample sizes: the Monte Carlo experiments below (Steps 4-6, 13)
## use n = 50 -- enough replications and points per replication for stable
## bias/variance/MSE estimates. The single-dataset ILLUSTRATIVE PLOTS
## (Fig01-Fig03, Fig05) instead use a small n_demo = 12: with 50 points a
## single perturbed observation is diluted and the OLS-vs-LAD contrast is
## hard to see on a scatter plot. With n_demo = 12 the same one-point
## perturbation is impossible to miss visually (and the scatter still looks
## like a believable little data set), which is the whole point of showing
## a picture. See Session1_Manual.pdf, Sections 4-5, for why this does not
## change any of the conclusions (the weighted-median argument is exact for
## any n).
n_demo <- 12

set.seed(7)
x_demo <- runif(n_demo, 2, 10)
y_demo <- beta0 * x_demo + rnorm(n_demo, sd = 1.2)

b_ols0 <- ols_origin(x_demo, y_demo)
b_lad0 <- lad_origin(x_demo, y_demo)

cat("=================================================================\n")
cat("ACT 1 / STEPS 1-3: no-intercept model, one clean data set (n=12)\n")
cat("=================================================================\n")
cat(sprintf("true beta = %.3f | OLS beta = %.4f | LAD beta = %.4f\n\n",
            beta0, b_ols0, b_lad0))

draw_fig01 <- function() {
  plot(x_demo, y_demo, pch = 19, col = "grey30", cex = 1.3,
       main = "No intercept, Normal errors, no outliers (n=12)",
       xlab = "X", ylab = "Y")
  abline(0, beta0,  col = "black", lwd = 2)
  abline(0, b_ols0, col = "blue",  lwd = 2, lty = 2)
  abline(0, b_lad0, col = "red",   lwd = 2, lty = 3)
  legend("topleft", c("true line", "OLS", "LAD"),
         col = c("black", "blue", "red"), lty = 1:3, lwd = 2, bty = "n")
}

draw_fig01()   # live plot -- shows in the R console / Plots pane
pdf("Fig01_baseline_no_outlier.pdf", width = 6, height = 5.2); draw_fig01(); dev.off()


## =============================================================================
## STEP 4
## Monte Carlo: bias, variance, MSE of OLS and LAD under NORMAL errors,
## no outliers. Expect OLS to win (lower MSE) -- it is BLUE / efficient here.
## =============================================================================

## generic MC engine for the no-intercept model. outlier_fun(x,y) may
## corrupt one replication's data before the estimators see it.
mc_compare_origin <- function(n, beta0, eps_gen, n_rep = 2000, outlier_fun = NULL) {
  b_ols <- numeric(n_rep)
  b_lad <- numeric(n_rep)
  for (r in 1:n_rep) {
    x <- runif(n, 1, 10)
    y <- beta0 * x + eps_gen(n)
    if (!is.null(outlier_fun)) {
      out <- outlier_fun(x, y)
      x <- out$x; y <- out$y
    }
    b_ols[r] <- ols_origin(x, y)
    b_lad[r] <- lad_origin(x, y)
  }
  rbind(OLS = summarize_estimates(b_ols, beta0),
        LAD = summarize_estimates(b_lad, beta0))
}

cat("=================================================================\n")
cat("STEP 4: Monte Carlo comparison, Normal(0, 2^2) errors, NO outliers\n")
cat("=================================================================\n")
res_normal <- mc_compare_origin(n = 50, beta0 = beta0,
                                 eps_gen = function(n) rnorm(n, sd = 2),
                                 n_rep = 2000)
print(round(res_normal, 4))
cat("--> Under Normal errors with no contamination, OLS has the smaller\n")
cat("    MSE (it is the efficient / BLUE estimator here); LAD pays a small\n")
cat("    price in variance for no benefit since there is nothing to be\n")
cat("    robust against.\n\n")


## =============================================================================
## STEP 5  (Act 2)
## Repeat with a Y-SPACE outlier: one ordinary-X, wild-Y point per replicate.
## Expect: LAD wins decisively (small bias/variance/MSE); OLS is dragged.
## =============================================================================

y_outlier_fun <- function(x, y, shift = 40) {
  i <- sample(seq_along(x), 1)          # ordinary x, contaminate y only
  y[i] <- y[i] + shift * sample(c(-1, 1), 1)
  list(x = x, y = y)
}

cat("=================================================================\n")
cat("STEP 5: Monte Carlo comparison, ONE Y-space outlier per replicate\n")
cat("=================================================================\n")
res_youtlier <- mc_compare_origin(n = 50, beta0 = beta0,
                                   eps_gen = function(n) rnorm(n, sd = 2),
                                   n_rep = 2000,
                                   outlier_fun = y_outlier_fun)
print(round(res_youtlier, 4))
cat("--> OLS: bias/variance/MSE all inflated by the single Y-outlier.\n")
cat("    LAD: barely disturbed relative to Step 4 -- the weighted-median\n")
cat("    view (Act 3) explains why: an outlying r_i=y_i/x_i with ordinary\n")
cat("    weight |x_i| cannot move a weighted median by much.\n\n")

## Illustrative picture: SAME small clean data set as Fig01, with the
## middle-X point's Y pushed far away (deterministic choice, not random,
## so the picture is reproducible and the effect is guaranteed visible).
i1 <- order(x_demo)[ceiling(n_demo / 2)]
x1 <- x_demo
y1 <- y_demo
y1[i1] <- y1[i1] + 45

b_ols1 <- ols_origin(x1, y1); b_lad1 <- lad_origin(x1, y1)

draw_fig02 <- function() {
  plot(x1, y1, pch = 19, col = "grey30", cex = 1.3,
       main = "One Y-space outlier (n=12)", xlab = "X", ylab = "Y")
  points(x1[i1], y1[i1], pch = 17, col = "darkorange", cex = 2.2)
  abline(0, beta0,  col = "black", lwd = 2)
  abline(0, b_ols1, col = "blue",  lwd = 2, lty = 2)
  abline(0, b_lad1, col = "red",   lwd = 2, lty = 3)
  legend("topleft", c("true line", "OLS", "LAD"),
         col = c("black", "blue", "red"), lty = 1:3, lwd = 2, bty = "n")
}

draw_fig02()   # live plot -- shows in the R console / Plots pane
pdf("Fig02_Y_space_outlier.pdf", width = 6, height = 5.2); draw_fig02(); dev.off()
cat(sprintf("Fig02 check: true beta=%.2f, OLS=%.3f (pulled up), LAD=%.3f (barely moved)\n\n",
            beta0, b_ols1, b_lad1))


## =============================================================================
## STEP 6  (Act 3)
## Repeat with an X-SPACE outlier (bad leverage point): extreme X AND a Y
## value inconsistent with the line. Expect: BOTH OLS and LAD are pulled.
## =============================================================================

x_outlier_fun <- function(x, y, x_mult = 4, y_shift = -30) {
  i <- which.max(x)
  x[i] <- x[i] * x_mult
  y[i] <- y[i] + y_shift
  list(x = x, y = y)
}

cat("=================================================================\n")
cat("STEP 6: Monte Carlo comparison, ONE X-space (bad leverage) outlier\n")
cat("=================================================================\n")
res_xoutlier <- mc_compare_origin(n = 50, beta0 = beta0,
                                   eps_gen = function(n) rnorm(n, sd = 2),
                                   n_rep = 2000,
                                   outlier_fun = x_outlier_fun)
print(round(res_xoutlier, 4))
cat("--> Now BOTH OLS and LAD show inflated bias/variance/MSE. A bad\n")
cat("    leverage point carries a large weight |x_i| in the weighted-median\n")
cat("    formula, so it can dominate the weighted median just as it\n")
cat("    dominates the OLS normal equations.\n\n")

## STEP 7 (discussion, no code): ask students WHY LAD, robust in Step 5,
## fails here. Answer: LAD's robustness is bounded influence in the
## Y-direction only; the weight |x_i| in the weighted-median representation
## grows with X, so an extreme X dominates regardless of the residual size.

## Same small clean data set again; this time push the largest-X point's
## X out to a fixed, moderately dominant value -- close enough to the data
## cloud that the picture stays readable, but with enough weighted-median
## weight |X_i| to visibly pull both fits. We ALSO give it a moderately
## (not wildly) inconsistent Y, by directly targeting a ratio Y/X well
## below the true beta rather than an arbitrary large shift: this keeps the
## distortion noticeable without collapsing the fitted lines to near-zero.
j2 <- which.max(x_demo)
x2 <- x_demo
y2 <- y_demo
x2[j2] <- 60                 # moderately large X (was 90 -- brought closer in)
y2[j2] <- 1.7 * x2[j2]       # ratio Y/X = 1.7, noticeably below true beta = 2.5

b_ols2 <- ols_origin(x2, y2); b_lad2 <- lad_origin(x2, y2)

draw_fig03 <- function() {
  plot(x2, y2, pch = 19, col = "grey30", cex = 1.3,
       main = "One X-space (bad leverage) outlier (n=12)", xlab = "X", ylab = "Y")
  points(x2[j2], y2[j2], pch = 17, col = "darkorange", cex = 2.2)
  abline(0, beta0,  col = "black", lwd = 2)
  abline(0, b_ols2, col = "blue",  lwd = 2, lty = 2)
  abline(0, b_lad2, col = "red",   lwd = 2, lty = 3)
  legend("topleft", c("true line", "OLS", "LAD"),
         col = c("black", "blue", "red"), lty = 1:3, lwd = 2, bty = "n")
}

draw_fig03()   # live plot -- shows in the R console / Plots pane
pdf("Fig03_X_space_outlier.pdf", width = 6, height = 5.2); draw_fig03(); dev.off()
cat(sprintf("Fig03 check: true beta=%.2f, OLS=%.3f, LAD=%.3f (both pulled down noticeably)\n\n",
            beta0, b_ols2, b_lad2))


## =============================================================================
## STEP 8  (no outliers -- SKEWED errors instead)
## eps ~ chisq(k) - k  (mean 0, right/positively skewed)
## eps ~ -(chisq(k) - k)  (mean 0, left/negatively skewed, same variance)
##
## Key point: OLS estimates the CONDITIONAL MEAN (E[eps]=0 by construction,
## so OLS is centred on the true line regardless of skew). LAD estimates the
## CONDITIONAL MEDIAN. For a skewed distribution, median != mean, so LAD's
## fitted line is shifted relative to OLS's, and the direction of the shift
## flips with the sign of the skew.
## =============================================================================

k <- 4
eps_pos_skew <- function(n) rchisq(n, df = k) - k      # mean 0, skew > 0
eps_neg_skew <- function(n) -(rchisq(n, df = k) - k)   # mean 0, skew < 0
median_offset <- qchisq(0.5, df = k) - k               # theoretical median of eps_pos_skew

cat("=================================================================\n")
cat("STEP 8: Effect of SKEWED errors (no outliers) on OLS vs LAD\n")
cat("=================================================================\n")
cat(sprintf("median(chisq(%d) - %d) = %.4f   (mean is exactly 0 by construction)\n",
            k, k, median_offset))
cat("--> Positive skew has NEGATIVE median: LAD's line is pulled BELOW OLS's.\n")
cat("--> Negating the noise flips the skew and the sign of the median shift.\n\n")

n8 <- 300
x8 <- runif(n8, 1, 10)

y8_pos <- beta0 * x8 + eps_pos_skew(n8)
y8_neg <- beta0 * x8 + eps_neg_skew(n8)

b_ols_pos <- ols_origin(x8, y8_pos); b_lad_pos <- lad_origin(x8, y8_pos)
b_ols_neg <- ols_origin(x8, y8_neg); b_lad_neg <- lad_origin(x8, y8_neg)

x_ref <- mean(x8)
cat(sprintf("Positively skewed eps: OLS beta = %.4f, LAD beta = %.4f  ->  at X = %.2f, OLS predicts %.3f vs LAD %.3f  (OLS HIGHER)\n",
            b_ols_pos, b_lad_pos, x_ref, b_ols_pos * x_ref, b_lad_pos * x_ref))
cat(sprintf("Negatively skewed eps: OLS beta = %.4f, LAD beta = %.4f  ->  at X = %.2f, OLS predicts %.3f vs LAD %.3f  (OLS LOWER)\n\n",
            b_ols_neg, b_lad_neg, x_ref, b_ols_neg * x_ref, b_lad_neg * x_ref))

## STEP 9 (discussion, no code): ask WHY. Answer: OLS targets E[Y|X]=beta*X
## exactly (noise has mean 0 by construction); LAD targets the conditional
## MEDIAN = beta*X + median(eps). Skew separates mean and median.

draw_fig04 <- function() {
  par(mfrow = c(1, 2))
  plot(x8, y8_pos, pch = 19, col = "grey60", cex = 0.6,
       main = "Positively skewed errors: chisq(4) - 4", xlab = "X", ylab = "Y")
  abline(0, beta0,     col = "black", lwd = 2)
  abline(0, b_ols_pos, col = "blue",  lwd = 2, lty = 2)
  abline(0, b_lad_pos, col = "red",   lwd = 2, lty = 3)
  legend("topleft", c("true", "OLS", "LAD"), col = c("black","blue","red"), lty = 1:3, lwd = 2, bty = "n")

  plot(x8, y8_neg, pch = 19, col = "grey60", cex = 0.6,
       main = "Negatively skewed errors: -(chisq(4) - 4)", xlab = "X", ylab = "Y")
  abline(0, beta0,     col = "black", lwd = 2)
  abline(0, b_ols_neg, col = "blue",  lwd = 2, lty = 2)
  abline(0, b_lad_neg, col = "red",   lwd = 2, lty = 3)
  legend("topleft", c("true", "OLS", "LAD"), col = c("black","blue","red"), lty = 1:3, lwd = 2, bty = "n")
  par(mfrow = c(1, 1))
}

draw_fig04()   # live plot -- shows in the R console / Plots pane
pdf("Fig04_skewed_errors.pdf", width = 10, height = 5.2); draw_fig04(); dev.off()


## =============================================================================
## ACT 4-5  /  STEPS 10-12
## Model with an intercept: Y_i = alpha + beta*X_i + eps_i.
## OLS: normal equations (closed form, ols_intercept() above).
## LAD: no closed form -- solved by IRWLS (irwls_lad() above).
## Below: a step-by-step numeric trace on a small data set, for the slide
## "IRWLS: step-by-step trace".
## =============================================================================

alpha0 <- 3.0
cat("=================================================================\n")
cat("STEPS 10-12: intercept model, IRWLS trace on a small data set (n=12)\n")
cat("=================================================================\n")
set.seed(21)
n_tr <- 12
x_tr <- runif(n_tr, 1, 10)
y_tr <- alpha0 + beta0 * x_tr + rnorm(n_tr, sd = 2)

cat("theta^(0) = OLS =", round(ols_intercept(x_tr, y_tr), 4), "\n")
fit_trace <- irwls_lad(x_tr, y_tr, trace = TRUE)
cat("Converged in", fit_trace$iters, "iterations to theta =",
    round(fit_trace$theta, 4), "\n\n")


## =============================================================================
## STEP 13  (Act 6)
## Repeat Steps 3-6 under the intercept model: baseline, MC bias/var/MSE,
## Y-space outlier, X-space outlier -- now comparing OLS to LAD-via-IRWLS.
## =============================================================================

mc_compare_intercept <- function(n, alpha0, beta0, eps_gen, n_rep = 2000, outlier_fun = NULL) {
  a_ols <- b_ols <- a_lad <- b_lad <- numeric(n_rep)
  for (r in 1:n_rep) {
    x <- runif(n, 1, 10)
    y <- alpha0 + beta0 * x + eps_gen(n)
    if (!is.null(outlier_fun)) {
      out <- outlier_fun(x, y)
      x <- out$x; y <- out$y
    }
    th_ols <- ols_intercept(x, y)
    th_lad <- irwls_lad(x, y)$theta
    a_ols[r] <- th_ols[1]; b_ols[r] <- th_ols[2]
    a_lad[r] <- th_lad[1]; b_lad[r] <- th_lad[2]
  }
  list(alpha = rbind(OLS = summarize_estimates(a_ols, alpha0),
                      LAD = summarize_estimates(a_lad, alpha0)),
       beta  = rbind(OLS = summarize_estimates(b_ols, beta0),
                      LAD = summarize_estimates(b_lad, beta0)))
}

y_outlier_fun_i <- function(x, y, shift = 40) {
  i <- sample(seq_along(x), 1)
  y[i] <- y[i] + shift * sample(c(-1, 1), 1)
  list(x = x, y = y)
}
x_outlier_fun_i <- function(x, y, x_mult = 4, y_shift = -30) {
  i <- which.max(x)
  x[i] <- x[i] * x_mult
  y[i] <- y[i] + y_shift
  list(x = x, y = y)
}

cat("=================================================================\n")
cat("STEP 13a: intercept model, baseline (Normal errors, no outliers)\n")
cat("=================================================================\n")
res_i_base <- mc_compare_intercept(n = 50, alpha0 = alpha0, beta0 = beta0,
                                    eps_gen = function(n) rnorm(n, sd = 2),
                                    n_rep = 500)   # 500 reps: IRWLS is iterative, keep runtime sane
cat("alpha:\n"); print(round(res_i_base$alpha, 4))
cat("beta:\n");  print(round(res_i_base$beta, 4))
cat("\n")

cat("=================================================================\n")
cat("STEP 13b: intercept model, ONE Y-space outlier per replicate\n")
cat("=================================================================\n")
res_i_y <- mc_compare_intercept(n = 50, alpha0 = alpha0, beta0 = beta0,
                                 eps_gen = function(n) rnorm(n, sd = 2),
                                 n_rep = 500, outlier_fun = y_outlier_fun_i)
cat("alpha:\n"); print(round(res_i_y$alpha, 4))
cat("beta:\n");  print(round(res_i_y$beta, 4))
cat("--> LAD-via-IRWLS again resists the Y-outlier much better than OLS.\n\n")

cat("=================================================================\n")
cat("STEP 13c: intercept model, ONE X-space (bad leverage) outlier\n")
cat("=================================================================\n")
res_i_x <- mc_compare_intercept(n = 50, alpha0 = alpha0, beta0 = beta0,
                                 eps_gen = function(n) rnorm(n, sd = 2),
                                 n_rep = 500, outlier_fun = x_outlier_fun_i)
cat("alpha:\n"); print(round(res_i_x$alpha, 4))
cat("beta:\n");  print(round(res_i_x$beta, 4))
cat("--> Both OLS and LAD-via-IRWLS degrade -- same lesson as the\n")
cat("    no-intercept model: bad leverage beats both estimators.\n\n")

## Illustrative pictures, intercept model -- same small-n_demo philosophy
## as the no-intercept figures above: one clean small data set, then a
## deterministic Y-outlier and a deterministic (fixed-X) bad leverage point.
set.seed(9)
x3 <- runif(n_demo, 2, 10)
y3 <- alpha0 + beta0 * x3 + rnorm(n_demo, sd = 1.2)
th_ols3 <- ols_intercept(x3, y3); th_lad3 <- irwls_lad(x3, y3)$theta

i3y <- order(x3)[ceiling(n_demo / 2)]
y3y <- y3
y3y[i3y] <- y3y[i3y] + 40
th_ols3y <- ols_intercept(x3, y3y); th_lad3y <- irwls_lad(x3, y3y)$theta

j3x <- which.max(x3)
x3x <- x3; y3x <- y3
x3x[j3x] <- 40               # moderately large X (was 90 -- brought closer in)
y3x[j3x] <- y3x[j3x] - 20
th_ols3x <- ols_intercept(x3x, y3x); th_lad3x <- irwls_lad(x3x, y3x)$theta

draw_fig05 <- function() {
  par(mfrow = c(1, 3))

  plot(x3, y3, pch = 19, col = "grey30", cex = 1.3, main = "(a) Baseline (n=12)", xlab = "X", ylab = "Y")
  abline(alpha0, beta0, col = "black", lwd = 2)
  abline(th_ols3[1], th_ols3[2], col = "blue", lwd = 2, lty = 2)
  abline(th_lad3[1], th_lad3[2], col = "red",  lwd = 2, lty = 3)
  legend("topleft", c("true", "OLS", "LAD (IRWLS)"), col = c("black","blue","red"), lty = 1:3, lwd = 2, bty = "n", cex = 0.8)

  plot(x3, y3y, pch = 19, col = "grey30", cex = 1.3, main = "(b) Y-space outlier", xlab = "X", ylab = "Y")
  points(x3[i3y], y3y[i3y], pch = 17, col = "darkorange", cex = 2.2)
  abline(alpha0, beta0, col = "black", lwd = 2)
  abline(th_ols3y[1], th_ols3y[2], col = "blue", lwd = 2, lty = 2)
  abline(th_lad3y[1], th_lad3y[2], col = "red",  lwd = 2, lty = 3)
  legend("topleft", c("true", "OLS", "LAD (IRWLS)"), col = c("black","blue","red"), lty = 1:3, lwd = 2, bty = "n", cex = 0.8)

  plot(x3x, y3x, pch = 19, col = "grey30", cex = 1.3, main = "(c) X-space (bad leverage) outlier", xlab = "X", ylab = "Y")
  points(x3x[j3x], y3x[j3x], pch = 17, col = "darkorange", cex = 2.2)
  abline(alpha0, beta0, col = "black", lwd = 2)
  abline(th_ols3x[1], th_ols3x[2], col = "blue", lwd = 2, lty = 2)
  abline(th_lad3x[1], th_lad3x[2], col = "red",  lwd = 2, lty = 3)
  legend("topleft", c("true", "OLS", "LAD (IRWLS)"), col = c("black","blue","red"), lty = 1:3, lwd = 2, bty = "n", cex = 0.8)

  par(mfrow = c(1, 1))
}

draw_fig05()   # live plot -- shows in the R console / Plots pane
pdf("Fig05_intercept_model_outliers.pdf", width = 13, height = 4.6); draw_fig05(); dev.off()
cat(sprintf("Fig05 check: baseline OLS=(%.2f,%.2f) LAD=(%.2f,%.2f)\n",
            th_ols3[1], th_ols3[2], th_lad3[1], th_lad3[2]))
cat(sprintf("             Y-outlier OLS=(%.2f,%.2f) LAD=(%.2f,%.2f)  [LAD should barely move]\n",
            th_ols3y[1], th_ols3y[2], th_lad3y[1], th_lad3y[2]))
cat(sprintf("             X-outlier OLS=(%.2f,%.2f) LAD=(%.2f,%.2f)  [both should collapse]\n\n",
            th_ols3x[1], th_ols3x[2], th_lad3x[1], th_lad3x[2]))


## =============================================================================
## STEP 14: ASSIGNMENTS  (prompts for students -- solution reference below)
## =============================================================================
## A1. Generate eps from a Cauchy distribution (rcauchy). Neither the mean
##     nor the variance of a Cauchy random variable exists. What happens to
##     the OLS estimator's sampling distribution across replications? Does
##     "bias" or "variance" even make sense to report? What happens to LAD?
## A2. In Step 6/13c, vary x_mult over {1,2,4,8,16}. At what point does the
##     leverage point start to dominate BOTH estimators? Relate this to the
##     hat-matrix leverage h_ii (Session 2 preview).
## A3. Re-derive the subgradient conditions for LAD with an intercept
##     (Act 4) and show they reduce to the weighted-median condition when
##     alpha is fixed at 0.
## A4. What happens to IRWLS if, at some iterate, two residuals are
##     numerically equal? What if a residual is exactly 0? (This is why the
##     delta floor exists in irwls_lad().)
## A5. Replace irwls_lad()'s weight function w_i = 1/max(|r_i|,delta) with
##     Huber's weight (bounded psi) and see how the fitted line changes on
##     the X-space outlier data set. (Optional, beyond today's scope.)
## -----------------------------------------------------------------------

cat("=================================================================\n")
cat("STEP 14 (A1) -- INSTRUCTOR REFERENCE: Cauchy errors, no outliers\n")
cat("=================================================================\n")
cat("Neither E[eps] nor Var(eps) exists for a Cauchy r.v., so 'bias' and\n")
cat("'variance' of OLS are not well defined either -- we report the median\n")
cat("and the inter-quartile range (IQR) of the sampling distribution\n")
cat("instead, which DO exist because the estimators themselves have\n")
cat("well-defined (if heavy-tailed) distributions.\n\n")

n_rep_cauchy <- 500
b_ols_cauchy <- numeric(n_rep_cauchy)
b_lad_cauchy <- numeric(n_rep_cauchy)
for (r in 1:n_rep_cauchy) {
  x <- runif(50, 1, 10)
  y <- beta0 * x + rcauchy(50, location = 0, scale = 1)
  b_ols_cauchy[r] <- ols_origin(x, y)
  b_lad_cauchy[r] <- lad_origin(x, y)
}
cat(sprintf("OLS beta: median = %.3f, IQR = %.3f, range = [%.2f, %.2f]  <- some huge outlying values\n",
            median(b_ols_cauchy), IQR(b_ols_cauchy), min(b_ols_cauchy), max(b_ols_cauchy)))
cat(sprintf("LAD beta: median = %.3f, IQR = %.3f, range = [%.2f, %.2f]  <- stays well-behaved\n\n",
            median(b_lad_cauchy), IQR(b_lad_cauchy), min(b_lad_cauchy), max(b_lad_cauchy)))
cat("--> LAD is the natural M-estimator when errors are heavy-tailed:\n")
cat("    the sample mean of Cauchy noise does not even converge, but its\n")
cat("    median does, and LAD is exactly a (weighted, regression) median.\n\n")

## =============================================================================
## STEP 16: CONSOLIDATED SUMMARY -- every bias/variance/MSE table generated
## above, gathered in one place. Neither Session1_Manual.tex nor
## Session1_Beamer.tex currently embeds these numbers (points 4, 5, 6, 13 of
## the lesson plan ask for them explicitly) -- they are only narrated
## qualitatively there. Until that LaTeX is updated, this block is the single
## authoritative, reproducible source: it re-prints every table already
## computed above, and additionally writes them to a plain-text file so they
## can be copy-pasted into the manual/slides later without re-running
## anything. The global set.seed(1) at the top of this script plus the fixed,
## unchanged order of the random draws above already make every number below
## exactly reproducible on any machine -- no additional seeding needed.
## =============================================================================

summary_lines <- character(0)
add_block <- function(title, tbl_or_text) {
  summary_lines <<- c(summary_lines, "", paste0("== ", title, " =="),
                       capture.output(print(tbl_or_text)))
}

add_block("Step 4: baseline, Normal errors, no outliers (n=50, n_rep=2000) -- beta",
          round(res_normal, 4))
add_block("Step 5: ONE Y-space outlier per replicate (n=50, n_rep=2000) -- beta",
          round(res_youtlier, 4))
add_block("Step 6: ONE X-space (bad leverage) outlier per replicate (n=50, n_rep=2000) -- beta",
          round(res_xoutlier, 4))
add_block("Step 13a: intercept model, baseline (n=50, n_rep=500) -- alpha",
          round(res_i_base$alpha, 4))
add_block("Step 13a: intercept model, baseline (n=50, n_rep=500) -- beta",
          round(res_i_base$beta, 4))
add_block("Step 13b: intercept model, ONE Y-space outlier (n=50, n_rep=500) -- alpha",
          round(res_i_y$alpha, 4))
add_block("Step 13b: intercept model, ONE Y-space outlier (n=50, n_rep=500) -- beta",
          round(res_i_y$beta, 4))
add_block("Step 13c: intercept model, ONE X-space outlier (n=50, n_rep=500) -- alpha",
          round(res_i_x$alpha, 4))
add_block("Step 13c: intercept model, ONE X-space outlier (n=50, n_rep=500) -- beta",
          round(res_i_x$beta, 4))
add_block("Step 14 (A1): Cauchy errors, no outliers (n=50, n_rep=500) -- beta, median/IQR/range",
          data.frame(
            estimator = c("OLS", "LAD"),
            median = c(median(b_ols_cauchy), median(b_lad_cauchy)),
            IQR    = c(IQR(b_ols_cauchy),    IQR(b_lad_cauchy)),
            min    = c(min(b_ols_cauchy),    min(b_lad_cauchy)),
            max    = c(max(b_ols_cauchy),    max(b_lad_cauchy))
          ))

cat("\n=================================================================\n")
cat("STEP 16: CONSOLIDATED BIAS/VARIANCE/MSE SUMMARY (all tables above)\n")
cat("=================================================================\n")
cat(paste(summary_lines, collapse = "\n"))
cat("\n\n--> Point 4 (\"show LS is better under Normal errors\") is confirmed by\n")
cat("    the Step-4 table: OLS's MSE is the smaller of the two there. Point 5\n")
cat("    (Y-outlier, LAD wins) and point 6 (X-outlier, both fail) are\n")
cat("    confirmed the same way by the Step-5/Step-6 tables, and points\n")
cat("    13a-13c repeat the same three comparisons for the intercept model.\n")
cat("    These numbers are reproducible exactly (global set.seed(1), fixed\n")
cat("    call order) and are ready to be pasted into Session1_Manual.tex /\n")
cat("    Session1_Beamer.tex as tables next to the corresponding figures.\n\n")

writeLines(summary_lines, "MC_Summary_Tables.txt")
cat("Consolidated summary also written to MC_Summary_Tables.txt\n\n")

cat("Figures written to the working directory:\n")
cat("  Fig01_baseline_no_outlier.pdf\n")
cat("  Fig02_Y_space_outlier.pdf\n")
cat("  Fig03_X_space_outlier.pdf\n")
cat("  Fig04_skewed_errors.pdf\n")
cat("  Fig05_intercept_model_outliers.pdf\n")
