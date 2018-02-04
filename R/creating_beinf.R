library(gamlss)
## generate zero-one-inflated beta regression
# set parameters
n <- 5000
mu <- 0.40
sigma <- 0.60
p0 <- .13
p1 <- .17
p2 <- 1 - p0 - p1
a <- mu * (1 - sigma ^ 2) / (sigma ^ 2)
b <- a * (1 - mu) / mu

# generate data
set.seed(1839)
x <- vector("numeric", n)
for (i in 1:n) {
  rand <- runif(1)
  if (rand <= p0) {
    x[i] <- 0
  } else if ((p0 < rand) & (rand <= p0 + p1)) {
    x[i] <- 1
  } else {
    x[i] <- rbeta(1, a, b)
  }
}

# run model with just intercepts
fit <- gamlss(x ~ 1, ~ 1, ~ 1, ~ 1, family = BEINF())
summary(fit)

# transform back to original scale
inv_logit <- function(x) exp(x) / (1 + exp(x))

fit_mu <- inv_logit(fit$mu.coefficients)
fit_mu
mu

fit_sigma <- inv_logit(fit$sigma.coefficients)
fit_sigma
sigma

fit_nu <- exp(fit$nu.coefficients)
fit_tau <- exp(fit$tau.coefficients)

fit_p0 <- fit_nu / (1 + fit_nu + fit_tau)
fit_p0
p0

fit_p1 <- fit_tau / (1 + fit_nu + fit_tau)
fit_p1
p1
