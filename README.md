# Assignment 04

Instructions

1. [Fork this repository](https://help.github.com/articles/using-pull-requests/) to your GitHub account.
2. Write your solutions in R Markdown in a file named `index.Rmd`.
3. Compile your solutions to an HTML file, `index.html`. You can view it at `https://{username}.github.io/Assignment_04`.
3. When you are ready to submit your assignment, [initiate a pull request](https://help.github.com/articles/using-pull-requests/#initiating-the-pull-request). Title your
pull request "Submission".

To update your fork from the upstream repository:

1. On your fork, e.g. `https://github.com/jrnold/Assignment_04` click on "New Pull request"
2. Set your fork `jrnold/Assignment_04` as the base fork on the left, and `UW-POLS503/Assignment_04` as the head fork on the right. In both cases the branch will be master. This means, compare any canes in the head fork that are not in the base fork. You will see differences between the `US-POLS503` repository and your fork. Click on "Create Pull Request", and if there are no issues, "Click Merge" A quick way is to use this link, but change the `jrnold` to your own username: `https://github.com/jrnold/Assignment_04/compare/gh-pages...UW-POLS504:gh-pages`.

We'll use these packages,

```r
library("MASS")
library("pols503")
library("foreign")
library("dplyr")
library("broom")
library("ggplot2")
```


These are some utility functions used in this assignment

```r
mvnormal_data <- function(n,
                          mu = 0,
                          sigma = rep(1, length(mu)),
                          cor = diag(length(mu)),
                          empirical = TRUE,
                          colnames = paste0("x", seq_along(mu))
                          ) {
  setNames(as.data.frame(MASS::mvrnorm(n, mu = mu,
                                       sdcor2cov(sigma, cov2cor(cor)),
                              empirical = empirical)),
           colnames)
}
```
Given $\hat{\vec{y}}$ and $R^2$ calculate a regression standard deviation,

```r
r2_to_sigma <- function (yhat, r2) {
  ybar <- mean(yhat)
  ssm <- sum((yhat - ybar)^2)
  sse <- (1 - r2)/r2 * ssm
  sqrt(sse/ length(yhat))
}
```

Summarize the results of the simulations:

```r
summarize_params <- function(.data) {
  ret <- .data %>%
    group_by(term) %>%
    summarize(estimate_mean = mean(estimate),
              estimate_sd = sd(estimate),
              std_error_mean = mean(std.error),
              std_error_sd = sd(std.error),
              estimate_mean_se = sd(estimate) / sqrt(n()),
              estimate_sd_se = sd(estimate) / sqrt(2 * (n() - 1)),
              std_error_mean_sd = sd(std.error) / sqrt(n()),
              std_error_sd_se = sd(std.error) / sqrt(2 * (n() - 1)),
              iter = length(estimate))
  ret
}
```


# Monte-Carlo Simulations

In each question, we will perform a Monte Carlo simulation in which 
we

1. draw a sample from a population (that often violates a CLR assumption)
2. estimate OLS on that sample
3. Repeat steps 1-2 many times to generate a sampling distribution of the coefficients
4. Repeat steps 1-3 many for several different populations to understand how the 
    sampling distribution varies with features such as the number of observations in the sample,
    correlation between covariates, etc.


## Omitted Variables

Simulate data from:
$$
Y = X_1 + \beta_2 X_2 + \varepsilon
$$
where $\Cor(X_1, X_2) = \rho$.
However, you estimate a regression with an omitted variable,
$$
y_i = \hat{\beta}_1 x_{1,i} + \hat\varepsilon
$$

In these simulations we will vary:

- $n$: The sample size
- $rho$: The correlation between $x_1$ and $x_2$
- $\beta_2$: The coefficient on $x_2$


```r
simulate_ovb <- function(iter, n, rho, beta2) {
  n <- 100
  rho <- 0
  r2 <- 0.5  
  beta <- c(0, 1, beta2)
  cormat <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
  dat <- mvnormal_data(n, mu = c(0, 0), cor = cormat)
  yhat <- model.matrix(~ x1 + x2, data = dat)
  sigma <- r2_to_sigma(yhat, r2)
  results <- vector(mode = "list", length = iter)
  for (i in seq_len(iter)) {
    # Simulate y
    dat[["y"]] <- yhat + rnorm(n, sd = sigma)
    # Estimate OLS
    results <- tidy(lm(y ~ x1, data = dat))
  }
  summarize_params(results)
}

sims_ovb <-
  expand.grid(rho = c(0, 0.3, .7),
              beta2 = c(1, 0.5, 0),
              obs = c(30, 100, 1000)) %>%
  group_by(rho, beta2, obs) %>%
  do({
    simulate_ovb(2048, n = .$obs, rho = .$rho, beta2 = .$beta2)
  })
save(sims_ovb, file = "data/sims_ovb.RData")
```

Use the simulation results to answer the following questions. Use plots in your answers.
How do the values of $n$, $\rho$, and $\beta_2$ affect the following:

- Bias of $\hat{beta}_1$, $E(\hat{\beta}_1) - \beta_1$
- The standard deviation of $\hat{\beta}_1$, i.e. $\sd(\hat{\beta})$.
- The bias of the standard error of $\hat{\beta}_1$, i.e. $\E(\se(\hat{\beta}_1)) - \sd(\hat{\beta}_1)$


## Measurement Error

Population regression function
$$
Y = \beta_0 + \beta_1 \vec{x}_1 + \beta_2 + \varepsilon
$$
where $\E(\vec{x}_1) = \E(\vec{x}_2) = 0$, $\Var(\vec{x}_1) = \Var(\vec{x}_2) = 1$, and $\Cov(\vec{x}_1, \vec{x}_2) = \rho$.

The sample regression function is
$$
y_i = \hat\beta_0 + \hat\beta_1 z_{1,i} + \hat\beta_2 z_{2,i} + \varepsilon
$$
Instead of observing variables $x_{1,i}$ ($x_{2,i}$) we observe $z_{1,i}$ ($z_{2,i}$), which is $z_{1,i}$ ($z_{2,i}$) with measurement error,
$$
\begin{aligned}[t]
z_{1,i} = x_{1,i} + \delta_{i,i} \\
z_{2,i} = x_{2,i} + \delta_{2,i}
\end{aligned}
$$
where $\delta_{1,i} \sim N(0, \tau_1)$ and $\delta_{2,i} \sim N(0, \tau_1)$.
The measurement errors are uncorrelated with the $x$'s and $\varepsilon$.

Rather than vary the variances of $\tau$ between simulations, we will focus on the reliability of each variable.
The reliability is the ratio between the variance of $x$ and $z$. Since in this case, the 
variance of the $x$'s is 1, the reliability is
$$
r = 1 / 1 + \tau^2
$$
When the reliability is 1, there is no measurement error. As the relability goes to zero, there the measurement error overwhelms the variable.

In these simulations we will vary

- $n$: The sample size
- $r_1$: The reliability of $x_1$
- $r_2$: The reliability of $x_2$
- $\rho$: The correlation between $x_1$ and $x_2$


```r
simulate_measurement_error <- function(iter, n, rho, reliability) {
  # Regression coefficients
  beta <- c(0, 1, 1)
  # Correlation between X
  cormat <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
  # desired population R^2 (used to generate sigma)
  r2 <- 0.5
  # X drawn from a multivariate normal distribution
  dat <- mvnormal_data(n, mu = c(0, 0), cor = cormat)
  # yhat = X b
  yhat <- model.matrix(~ x1 + x2, data = dat) %*% beta
  # Regression standard deviation
  sigma <- r2_to_sigma(yhat, r2)
  results <- vector(mode = "list", length = iter)
  meas_sd <- sqrt((1 - reliability) / reliability)
  dat2 <- dat
  for (i in seq_len(iter)) {
    # Simulate y
    dat2[["y"]] <- yhat + rnorm(n, sd = sigma)
    for (j in 1:2) {    
      dat2[[paste0("x", j)]] <- dat[[paste0("x", j)]] + rnorm(n, sd = meas_sd[j])  
    }
    # Estimate OLS
    results <- tidy(lm(y ~ x1 + x2, data = dat2))
  }
  summarize_params(results)
}

sims_measurement_error <-
  expand.grid(reliability1 = c(.01, .7, 1),
              reliability2 = c(.01, .7, 1),
              rho = c(0, 0.3, .7),
              obs = c(30, 100, 1000)) %>%
  group_by(rho, obs, reliability1, reliability2) %>%
  do({
    simulate_measurement_error(2048,
                          rho = .$rho,
                          n = .$obs,
                          reliability = c(.$reliability1,
                                          .$reliability2))
  })
save(sims_measurement_error, file = "data/sims_measurement_error.RData")
```


Use the simulation results to answer the following questions. Use plots in your answers.
How do the values of $n$, $r_1$, $r_2$, and $\rho$ affect the following:

- Bias of $\hat{beta}_1$, $E(\hat{\beta}_1) - \beta_1$. In particular, focus on the following cases:

  - The bias of $\hat\beta_{1}$ changes with $r_1$ when $r_2 = 1$ 
  - The bias of $\hat\beta_{2}$ changes with $r_2$ when $r_2 = 1$
  - The bias of $\hat\beta_{1}$ changes with $r_2$ when $x_1$ has some measurement error, $r_1 < 1$.

- The standard deviation of $\hat{\beta}_1$, i.e. $\sd(\hat{\beta})$.
- The bias of the standard error of $\hat{\beta}_1$, i.e. $\E(\se(\hat{\beta}_1)) - \sd(\hat{\beta}_1)$

 

## Multi-collinearity

In these simulations our population is
$$
Y = \beta_0 + \beta_1 \vec{x}_1 + \beta_2 + \varepsilon
$$
where $\E(\vec{x}_1) = \E(\vec{x}_2) = 0$, $\Var(\vec{x}_1) = \Var(\vec{x}_2) = 1$, and $\Cov(\vec{x}_1, \vec{x}_2) = \rho$.
We also estimate the correct regression for each sample,
$$
Y = \beta_0 + \beta_1 \vec{x}_1 + \beta_2 + \varepsilon
$$


In these simulations we will vary:

- $n$: The sample size
- $rho$: The correlation between $x_1$ and $x_2$


```r
simulate_multicollinearity <- function(iter, n, rho) {
  beta <- c(0, 1, 1)
  r2 <- 0.5  
  cormat <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
  dat <- mvnormal_data(n, mu = c(0, 0), cor = cormat)
  yhat <- model.matrix(~ x1 + x2, data = dat) %*% beta
  sigma <- r2_to_sigma(yhat, r2)
  results <- vector(mode = "list", length = iter)
  for (i in seq_len(iter)) {
    # Simulate y
    dat[["y"]] <- yhat + rnorm(n, sd = sigma)
    # Estimate OLS
    results <- tidy(lm(y ~ x1 + x2, data = dat))
  }
  summarize_params(results)
}

sims_multicollinearity <- expand.grid(rho = c(0, 0.7, 0.95),
            obs = c(30, 100, 1000)) %>%
  group_by(rho, obs) %>%
  do({
    simulate_multicollinearity(2048, rho = .$rho, n = .$obs)
  })
save(sims_multicollinearity, file = "data/sims_multicollinearity.RData")
```

Use the simulation results to answer the following questions. Use plots in your answers.
How do the values of $n$ and $\rho$ affect the following?

- Bias of $\hat{beta}_1$, $E(\hat{\beta}_1) - \beta_1$. In particular, focus on the following cases:
- The standard deviation of $\hat{\beta}_1$, i.e. $\sd(\hat{\beta})$.
- The bias of the standard error of $\hat{\beta}_1$, i.e. $\E(\se(\hat{\beta}_1)) - \sd(\hat{\beta}_1)$


# More Nunn and Wantchekon (2011)

1. How do Nunn and Wantchekon handle omitted variable bias? Replicate their calculations for at least one regression?
2. How would measurement error problems in the measure of trust used by Wantchekon affect their estimate of the effect of slave exports on trust? In their measures of slave exports? In the control variables?
