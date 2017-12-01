# Lecture 9 - Comparing means: t-test and ANOVA
library(tidyverse)


stud <- tibble(x = rt(500, 499))
single_sample <- tibble(x = rnorm(500, 1, 1))
independent_sample <- 
    tibble(x = rnorm(500, 2, 1),
           y = rnorm(500, 0, 1)) %>% 
    gather(sample, value)

# Showing the t-distribution (histogram) against normal distribution (density)
ggplot(stud) +
    aes(x = x) +
    geom_histogram(aes(y = ..density..), binwidth = .25) +
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

# Single sample t-test
ggplot(independent_sample) +
    aes(x = value, fill = sample, alpha = I(.8)) +
    geom_histogram(data = independent_sample %>% filter(sample == "x"),fill = "salmon") +
    geom_histogram(data = independent_sample %>% filter(sample == "y"),fill = "skyblue") +
    labs(x = NULL, y = NULL)

# Single sample t-test
ggplot(independent_sample) +
    aes(x = value, fill = sample) +
    geom_density(alpha = .7) +
    labs(x = NULL, y = NULL)
