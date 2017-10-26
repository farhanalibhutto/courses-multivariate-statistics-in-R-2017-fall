# Lecture 5 - Covariation, correlaion, linear regression

library(tidyverse)
library(forcats)
library(stringr)

# Generate data for plots
set.seed(1)
rand_num <- rep(1:5*10, 20)
corr_df <- 
    data_frame(x = rnorm(length(rand_num), rand_num, 5), 
               positive = rnorm(length(rand_num), rand_num, 10), 
               group = rand_num,
               negative = -positive + 50,
               no_correlation = rnorm(length(rand_num), 25, 50),
               weak_correlation = x + rnorm(length(x), 20, 40),
               medium_correlation = x + rnorm(length(x), 12, 25),
               strong_correlation = positive) %>% 
    gather(key, value, -x, -group) %>% 
    mutate(key = fct_relevel(key, "positive","negative","no_correlation","weak_correlation","medium_correlation","strong_correlation"))

# Plot correlations of different direction
corr_df %>% 
    filter(str_detect(key, "_correlation")) %>% 
    ggplot() + 
        aes(x = x, y = value) +
        geom_point(alpha = .4) +
        facet_wrap(~key)

# Check correlations of different direction
corr_df %>% 
    ggplot() + 
        aes(x = x, y = value) +
        geom_point(alpha = .4) +
        facet_wrap(~fct_rev(key)) +
        geom_smooth(method = "lm", color = "red", size = 1.5)

# It is possible that a different variable is responsible for the correlation, 
corr_df %>% 
    filter(key == "positive") %>% 
    ggplot() + 
        aes(x = x, y = value) +
        geom_point(alpha = .4) +
        facet_wrap(~group, ncol = 5) +
        geom_smooth(method = "lm", color = "red", size = 1.5)

# Plot correlations of different magnitude
corr_df %>% 
    filter(str_detect(key, "_correlation")) %>% 
    ggplot() + 
        aes(x = x, y = value) +
        geom_point(alpha = .4) +
        geom_smooth(method = "lm") +
        facet_wrap(~key, scales = "free_y")

# Calculate the correlations
corr_df %>% 
    filter(str_detect(key, "_correlation")) %>% 
    group_by(key) %>% 
    summarise(r = cor(x = x, y = value) %>% round(2))





