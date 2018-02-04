library(tidyverse)
rawdat <- read_csv("../data/variance_norm.csv")
nrow(rawdat)
responses <- t(summarise_all(rawdat, funs(sum(!is.na(.)))))
min(responses)
max(responses)
mean(responses)

medians <- rawdat %>% 
  summarise_all(funs(median(., na.rm = TRUE))) %>%
  gather(var, value) %>% 
  separate(var, c("var", "group")) %>% 
  spread(var, value) %>% 
  transmute(group = group, accept_m = accept, prej_m = (100 - prej))

variances <- rawdat %>% 
  summarise_all(funs(var(., na.rm = TRUE))) %>%
  gather(var, value) %>% 
  separate(var, c("var", "group")) %>% 
  spread(var, value) %>% 
  transmute(group = group, accept_var = accept, prej_var = prej)

dat <- full_join(medians, variances)

group_names <- read_csv("../data/groups.csv") %>% 
  mutate(group = as.character(1:n()))

dat <- full_join(dat, group_names)

## primary analysis: norms produce invariance
ggplot(dat, aes(x = accept_m, y = prej_var)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              se = FALSE, color = "black") +
  theme_minimal() +
  labs(x = "Social Acceptability of Prejudice (Median)", 
       y = "Reported Prejudice (Variance)") +
  theme(text = element_text(size = 16))

lin <- lm(prej_var ~ accept_m, dat)
summary(lin)
quad <- lm(prej_var ~ poly(accept_m, 2), dat)
summary(quad)
summary(quad)$r.squared - summary(lin)$r.squared

## replicating crandall, eshleman, & o'brien (2002)
ggplot(dat, aes(x = accept_m, y = prej_m)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(x = "Social Acceptability of Prejudice (Median)", 
       y = "Reported Prejudice (Median)") +
  theme(text = element_text(size = 16))

cor.test(~ accept_m + prej_m, dat)

## showing pc index, from crandall (1994)
pcdat <- rawdat %>% 
  select(starts_with("prej_")) %>% 
  summarise_all(funs(sum(. == 100, na.rm = TRUE))) %>% 
  gather(var, value) %>% 
  transmute(group = gsub("[^0-9]", "", var), pc_index = value) %>% 
  full_join(medians[, -3], by = "group")

pcfit <- MASS::glm.nb(pc_index ~ accept_m, pcdat)
summary(pcfit)

fake_x <- seq(0, 100, .25)
pcpred <- predict(pcfit, data.frame(accept_m = fake_x), type = "response")

ggplot() +
  theme_minimal() +
  geom_point(aes(x = pcdat$accept_m, y = pcdat$pc_index), shape = 1) +
  geom_line(aes(x = fake_x, y = pcpred), size = 1) +
  labs(x = "Social Acceptability of Prejudice (Median)", y = "PC Index") +
  theme(text = element_text(size = 16))
