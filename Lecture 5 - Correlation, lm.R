# Lecture 5 - Covariation, correlaion, linear regression
library(tidyverse)

# Read the cocktail dataset from my github repo
cocktails <- read_tsv("https://raw.github.com/nthun/cocktail-balance/master/cocktail_data.tsv")
cocktails <- read_tsv("http://bit.ly/2zbj7kA") # Same stuff, but shortened url

# Let's examine how alcohol content(abv), acid content(acid) are associated in the data.
# As a bonus, color the data points by type, and also write the names

# Task solution
cocktails %>% 
    ggplot() +
    aes(x = acid, y = abv, label = name) +
    geom_point(aes(color = type), size = 3) +
    geom_smooth(method = "lm")

# Generate data for plots
set.seed(1) # Sets up the random seed number
rand_num <- rep(1:5*10, 20) # Creates 20 repetitions of the numbers 10 to 50 with 10 increments
# Use random distributions for showing different correlation directions and magnitudes
corr_df <- 
    data_frame(x = rnorm(length(rand_num), rand_num, 5), 
               positive = rnorm(length(rand_num), rand_num, 10), 
               group = rand_num,
               negative = -positive + 50,
               no_correlation = rnorm(length(rand_num), 25, 25),
               weak_correlation = x + rnorm(length(x), 20, 40),
               moderate_correlation = x + rnorm(length(x), 12, 25),
               strong_correlation = positive) %>% 
    gather(key, value, -x, -group) %>% 
    mutate(key = fct_relevel(key, "positive","negative","no_correlation","weak_correlation","moderate_correlation","strong_correlation"))

# Load these packages
library(forcats)
library(stringr)

# Plot correlations of different direction
corr_df %>% 
    filter(key %in% c("positive","negative","no_correlation")) %>% 
    ggplot() + 
        aes(x = x, y = value) +
        geom_point(alpha = .4) +
        facet_wrap(~key)

# Plot correlations of different magnitude
corr_df %>% 
    filter(str_detect(key, "_correlation")) %>% 
    ggplot() + 
    aes(x = x, y = value) +
    geom_point(alpha = .4) +
    geom_smooth(method = "lm", color = "red", size = 1.5) +
    facet_wrap(~key, scales = "free_y")

# It is possible that a different variable is responsible for the correlation, 
corr_df %>% 
    filter(key == "positive") %>% 
    ggplot() + 
        aes(x = x, y = value) +
        geom_point(alpha = .4) +
        facet_wrap(~group, ncol = 5) +
        geom_smooth(method = "lm", color = "red", size = 1.5)

# Plotting the meaning of covariance
if (!require(ggrepel)) install.packages("ggrepel")
library(ggrepel) # This is used to plot non-overlapping text labels
cocktails %>% 
    select(index, name, abv, sugar) %>% 
    head(10) %>% 
    gather(property, value, abv:sugar) %>% 
    group_by(property) %>% 
    mutate(mean_value = mean(value)) %>% 
    ggplot() +
        aes(x = index, y = value, label = name) +
        geom_point(size = 3) +
        geom_hline(aes(yintercept = mean_value), size = 1.5, alpha = .5) +
        geom_segment(aes(xend = index, yend = mean_value), linetype = "dashed", color = "red", size = 1.2) +
        geom_text_repel() +
        facet_wrap(~property, nrow = 2, scales = "free_y")


## Correlation in R

# The simplest way is the built in cor() function
cor(cocktails$abv, cocktails$sugar)

# To get a correlation matrix, simly do it on a df
# You can set the method as pearson, spearman, or kendall correlation
cocktails %>% 
    select(abv:sugar) %>% 
    cor(method = "spearman")

# But to get p values, you need to run cor.test(). This can only be done in pairs of variables.
# This returns a more verbose output
cor.test(cocktails$abv, cocktails$sugar)

# Calculate the correlations in the tidyverse way, you have to use summarise. But you have to reshape the data first
cocktails %>% 
    select(name:sugar) %>% 
    gather(property, value, -name, -abv) %>% 
    group_by(property) %>% 
    summarise(r = cor(x = abv, y = value) %>% round(2),
              p = cor.test(x = abv, y = value, method = "pearson")$p.value)

cocktails %>% 
    select(name:sugar) %>% 
    # gather(property, value, -name, -abv) %>% 
    group_by(property) %>% 
    do(cor.test(x = abv, y = value) %>% tidy)

# To get significance of the values in the matrix, we should use the psych paclage. corr.test() returns correlations, sample size, and p-values. It can also correct the significance for multipe comparisions, and calculate confidence intervals. 
if (!require(psych)) install.packages("psych")
library(psych)
cocktails %>% 
    select(abv:sugar) %>% 
    corr.test()

# Visualize correlation matrices. Best to use the GGally package.
# By default, ggpairs() shows scatter plots (all variables by all variables), density plots, and the actual correlation values. You can also add several features, check: https://ggobi.github.io/ggally/
if (!require(GGally)) install.packages("psych")
library(GGally)

cocktails %>% 
    select(abv:sugar) %>% 
    ggpairs(lower = list(continuous = "smooth", color = "red"))






