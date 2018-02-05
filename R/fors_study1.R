library(tidyverse)
library(gamlss)
library(ggExtra)
library(gridExtra)
forsdat <- read_csv("../data/fors_study1.csv")
ggplot(forsdat, aes(x = sdo)) +
  theme_minimal() +
  geom_histogram(bins = 30) +
  labs(x = "Social Dominance Orientation", y = "Count") +
  theme(text = element_text(size = 16))
prop.table(table(forsdat$sdo == 1))
cor.test(~ sdo + rw_polid, data = forsdat)

fit_ols_yhat <- lm(sdo ~ rw_polid, forsdat)$fitted.values
yhat_stdres <- ggplot(mapping = aes(
  x = fit_ols_yhat, y = rstandard(lm(sdo ~ rw_polid, forsdat))
  )) +
  theme_minimal() +
  geom_jitter(shape = 1, width = .1, height = .1) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(x = "Predicted Values",
       y = "Standardized Residuals") +
  theme(text = element_text(size = 16))
ggMarginal(yhat_stdres, type = "histogram", margins = "y")

beta_norm <- function(y, l, u) (y - l) / (u - l)
forsdat$sdo <- beta_norm(forsdat$sdo, 1, 7)
range(forsdat$sdo)
fit_beinf <- gamlss(
  sdo ~ rw_polid,
  ~ rw_polid,
  ~ rw_polid,
  ~ rw_polid,
  data = forsdat,
  family = BEINF()
)
summary(fit_beinf)

fit_ols <- lm(sdo ~ rw_polid, forsdat)
ols_plot <- ggplot(forsdat, aes(x = rw_polid, y = sdo)) +
  theme_minimal() +
  geom_jitter(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_line(aes(y = fit_ols$fitted.values + summary(fit_ols)$sigma), 
            size = .75, linetype = 2) +
  geom_line(aes(y = fit_ols$fitted.values - summary(fit_ols)$sigma), 
            size = .75, linetype = 2) +
  labs(x = "Conservatism", 
       y = "Social Dominance Orientation") +
  theme(text = element_text(size = 16))
b_sig <- sqrt(fit_beinf$sigma.fv ^ 2 * fit_beinf$mu.fv * (1 - fit_beinf$mu.fv))
beinf_plot <- ggplot(forsdat, aes(x = rw_polid, y = sdo)) +
  theme_minimal() +
  geom_jitter(shape = 1) +
  geom_line(aes(y = fit_beinf$mu.fv), 
            size = 1.0, linetype = 1) +
  geom_line(aes(y = fit_beinf$mu.fv + b_sig), 
            size = .75, linetype = 2) +
  geom_line(aes(y = fit_beinf$mu.fv - b_sig), 
            size = .75, linetype = 2) +
  labs(x = "Conservatism", 
       y = "Social Dominance Orientation") +
  theme(text = element_text(size = 16))
ols_plot
beinf_plot
