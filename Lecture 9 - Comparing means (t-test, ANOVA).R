# Lecture 9 - Comparing means: t-test and ANOVA
library(tidyverse)
library(broom)

# 

# Plots for the slides
# Generate some random data for example plots
set.seed(1) # Set random seed for reproducibility
stud <- tibble(x = rt(500, 6))

single_sample <- tibble(x = rnorm(500, 1, 1))

paired_sample <- 
    tibble(sample = "x", 
           value = rnorm(500, 2, 1)) %>% 
    bind_rows(tibble(sample = "y", 
                     value = rnorm(500, 0, 1)))

independent_sample <- 
    tibble(sample = "x", 
           value = rnorm(500, 2, 1)) %>% 
    bind_rows(tibble(sample = "y", 
                     value = rnorm(250, 0, 1)))

# Showing the t-distribution (histogram) against normal distribution (density)
ggplot(stud) +
    aes(x = x) +
    geom_histogram(aes(y = ..density..), binwidth = .5) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(stud$x), sd = sd(stud$x)),
                  lwd = 1.5, 
                  col = "red") +
    labs(x = NULL, y = NULL)

# Plot different types of t-tests
# Single sample t-test
ggplot(single_sample) +
    aes(x = x) +
    geom_histogram(fill = "lightblue") +
    geom_vline(aes(xintercept = 0), size = 1.5, color = "red", linetype = "dashed") +
    labs(x = NULL, y = NULL)

# Paired sample t-test
ggplot(paired_sample) +
    aes(x = value, fill = sample, alpha = I(.8)) +
    geom_histogram(data = paired_sample %>% filter(sample == "x"),fill = "salmon") +
    geom_histogram(data = paired_sample %>% filter(sample == "y"),fill = "skyblue") +
    labs(x = NULL, y = NULL)

# Independent samples t-test
ggplot(independent_sample) +
    aes(x = value, fill = sample, alpha = I(.8)) +
    geom_histogram(data = independent_sample %>% filter(sample == "x"),fill = "salmon") +
    geom_histogram(data = independent_sample %>% filter(sample == "y"),fill = "skyblue") +
    labs(x = NULL, y = NULL)

# Doing t-test in R
# Single sample t-test
# Using the attitude dataset
?attitude

ggplot(attitude) +
    aes(x = rating) +
    geom_histogram(binwidth = 5) +
    geom_vline(aes(xintercept = 50), size = 1.5, color = "red", linetype = "dashed")

# Check if rating is significantly higher than 50
t.test(attitude$rating, mu = 50, alternative = "greater")

# Check if rating is significantly lower than 50
t.test(attitude$rating, mu = 50, alternative = "less")

# Check if there is a significant difference from 50 to any direction
single_result <- t.test(attitude$rating, mu = 50)
tidy(single_result)

psych::t2r(6.584486, 29)

# Paired t-test
# We are using the sleep dataset
?sleep
sleep <- 
    sleep %>% 
    as_tibble()

# Plot the data
ggplot(sleep) +
    aes(x = extra, fill = group) +
    geom_density(alpha = .6)

ggplot(sleep) +
    aes(x = group, y = extra, fill = group) +
    geom_boxplot()

# Running the paired t-test
paired_result <- t.test(extra ~ group, paired = TRUE, data = sleep)

sleep %>% 
    group_by(group) %>% 
    summarise(extra_mean = mean(extra),
              extra_sd = sd(extra))

# You can tidy up the results as usual using the broom::tidy()
paired_result %>% tidy()
psych::t2d(-4.0621, n = 10)
# Independent samples t-test
# Using the ToothGrowth dataset
?ToothGrowth

ggplot(ToothGrowth) +
    aes(x = supp, y = len, fill = supp) +
    geom_boxplot()

# Check the assumption of variance homogeneity (F-test)
# Non-significant effect shows that the variances are not different in the two groups
var.test(len ~ supp, data = ToothGrowth)

independent_result <- t.test(len ~ supp, var.equal = TRUE, data = ToothGrowth)
tidy(independent_result)

# Welch two sample t-test
independent_result <- t.test(len ~ supp, var.equal = TRUE, data = ToothGrowth)

# Get means and sd-s 
ToothGrowth %>% 
    group_by(supp) %>% 
    summarise(len_mean = mean(len) %>% round(2),
              len_sd = sd(len) %>% round(2),
              len_se = (len_sd/sqrt(n())) %>% round(2),
              n = n())

# Calculating effect sizes
# Get an r value
psych::t2r(t = 1.9153, df = 58)
# Get a Cohen's d value
psych::t2d(t = 1.9153, n = 59)

# Using ANOVA to compare three groups
# Using the plantgrowth dataset
?PlantGrowth
ggplot(PlantGrowth) +
    aes(x = weight, fill = group) +
    geom_density(alpha = .6)

ggplot(PlantGrowth) +
    aes(x = group, y = weight, fill = group) +
    geom_boxplot()


# Checking hoogeneity of variance
bartlett.test(weight ~ group, data = PlantGrowth)

# Run the ANOVA
anova_result <- aov(weight ~ group, data = PlantGrowth)
summary(anova_result)

# But of course, tidy() is better
tidy(anova_result)

PlantGrowth %>% 
    group_by(group) %>% 
    summarise(weight_mean = mean(weight),
              weight_sd = sd(weight))

sjstats::eta_sq(anova_result)

## Doing the same using linear regression
lm_result <- lm(weight ~ group, data = PlantGrowth)
summary(lm_result)
tidy(lm_result)

# To get the group effect without the pairwise comparisons, use anova()
# As you can see, anova can lead to false assumptions because it can answer to a different question



