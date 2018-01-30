library(tidyverse)
medians <- read_csv("../data/variance_norm.csv") %>% 
  summarise_all(funs(median(., na.rm = TRUE))) %>%
  gather(var, value) %>% 
  separate(var, c("var", "group")) %>% 
  spread(var, value) %>% 
  transmute(group = group, accept_m = accept, prej_m = (100 - prej))

variances <- read_csv("../data/variance_norm.csv") %>% 
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
  geom_point(color = "midnightblue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              se = FALSE, color = "firebrick") +
  geom_smooth(method = "loess", se = FALSE, color = "forestgreen") +
  theme_minimal() +
  labs(x = "Social Acceptability of Prejudice (Median)", 
       y = "Reported Prejudice (Variance)") +
  theme(text = element_text(size = 16))

lin <- lm(prej_var ~ accept_m, dat)
quad <- lm(prej_var ~ poly(accept_m, 2), dat)
summary(quad)
summary(quad)$r.squared - summary(lin)$r.squared

## replicating crandall, eshleman, & o'brien (2002)
ggplot(dat, aes(x = accept_m, y = prej_m)) +
  geom_point(color = "midnightblue") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  theme_minimal() +
  labs(x = "Social Acceptability of Prejudice (Median)", 
       y = "Reported Prejudice (Median)") +
  theme(text = element_text(size = 16))

cor.test(~ accept_m + prej_m, dat)
