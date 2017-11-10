# Lecture 6 - Linear regression
# Load these packages
library(tidyverse)
library(psych)
library(broom)

# Read the cocktails dataset
cocktails <- read_tsv("https://raw.github.com/nthun/cocktail-balance/master/cocktail_data.tsv")
cocktails <- read_tsv("http://bit.ly/2zbj7kA") # Same stuff, but shortened url

# Create simple linear regression of cocktail acidity on alcohol content
# This only returns intercept and slope
lm(abv ~ acid, data = cocktails)

# Store the model in a variable to be able to get details, predictions, etc.
acid_lm <- lm(abv ~ acid, data = cocktails)
summary(acid_lm)

# This also works without storing the results. However when you use pipes, mind that in lm(), data is not the first parameter
cocktails %>% 
    lm(abv ~ acid, data = .) %>% 
    summary()

# Plot the linear regression
cocktails %>% 
    ggplot() +
    aes(y = abv, x = acid) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) 

# To get clean results, use the broom package.
# tidy() puts returns the model summary in a neat data frame format
tidy(acid_lm)
# Augments adds important new columns to your data frames, such as the residuals (.resid), and predicted values corresponding your independent variables. These can be useful for residual diagnostics.
augment(acid_lm, cocktails)
# Glimpse returns important model performance metrics.
glance(acid_lm)

# To get the standardized coefficients (scale free), you need to standardize the output and predictor variables
# This 
acid_lm_std <- lm(scale(abv) ~ scale(acid), data = cocktails)
summary(acid_lm_std)
# You can check that the slope of acid now matches the correlation between abv and acid
cor(cocktails$abv, cocktails$acid)

## Predicting values based on the model
# Create data with new 
newdata <- tibble(acid = c(0.2, 0.3, 0.4))
# predict returns a vector of predictions
predict(acid_lm, newdata)
# modelr::add_predictions() returns a data frame. This one is the preferable.
modelr::add_predictions(newdata, acid_lm)

## Model fit measures
# See model fitting "game" at http://www.dangoldstein.com/regression.html

# Plot the residuals (error term from the model prediction)
# Ignore the warnings, thet are known developer bugs
# This plot shows the unexplained variance of the model (summary of the red lines)
cocktails %>% 
    augment(lm(abv ~ acid, data = .), .) %>% 
    ggplot() +
    aes(y = abv, x = acid) +
    geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "black") +
    geom_segment(aes(xend = acid, yend = .fitted), linetype = "dashed", color = "red", size = 1.2) +
    geom_point(size = 3)

# All variability in the outcome variable (variance)
# This plots shows the total variance of the outcome variable (summary of the blue lines)
cocktails %>% 
    mutate(mean_abv = mean(abv)) %>% 
    ggplot() +
    aes(y = abv, x = acid) +
    geom_hline(aes(yintercept = mean_abv), size = 1.5) +
    geom_segment(aes(xend = acid, yend = mean_abv), linetype = "dashed", color = "blue", size = 1.2) +
    geom_point(size = 3)

## Diagnostics
# The residuals should not have an underlying pattern: they should have a normal distribution
cocktails %>% 
    augment(lm(abv ~ acid, data = .), .) %>% 
    ggplot() +
    aes(.resid) +
    geom_histogram(bins = 10)

# We can also do a normality test on the residuals
# The Shapiro-Wilks test shows that the residuals are normally distributed
cocktails %>% 
    augment(lm(abv ~ acid, data = .), .) %>% 
    pull(.resid) %>% 
    shapiro.test(.)

# Plotting the residuals vs. fitted (pedicted) values
# This tells that there is a non-linear pattern that was not explained by the model
augment(lm(abv ~ acid, data = cocktails), cocktails) %>% 
    ggplot() +
    aes(x = .fitted, y = .resid) +
    geom_point() +
    geom_smooth(se = FALSE)

# We are actually better off to use the ggfortify package to make us diagnostic plots, using the autoplot() function
if (!require(ggfortify)) install.packages("ggfortify")
library(ggfortify)
autoplot(acid_lm, which = 1:6, label.size = 3)
### TODO: EXPLAIN DIAGNOSTIC PLOTS


# We can single out observations with the clice() function
odd_cases <-
cocktails %>% 
    slice(c(9, 44,45))


## Using categorical variables
# To be able to use catagorical variables in statistical models, we may need to convert the categories to dummy variables. This means that the variable name will be the category name, and if this category is true for the observation, the value of the variable will be 1, or otherwise 0.
# Task: create dummy coded variables in the cocktail dataset  from the type variable
cocktails %>% distinct(type)
dummy_cocktails <-
    cocktails %>% 
    mutate(value = 1) %>% # This defines what will be the spread across variables
    spread(type, value, fill = 0) #%>% # Add fill for binary coding, otherwise it will be NA
    # select(name, blended:stirred) # Just to show the important variables, otherwise, don't use this

# This is just the inner working of how categorical variables can be included in a linear regression model, because R does this automatically

cocktail_fit_1 <- lm(abv ~ type, data = cocktails)
summary(cocktail_fit_1)

# The only thing 
cocktail_fit_2 <- lm(abv ~ blended + built + carbonated + eggwhite + shaken + stirred, data = dummy_cocktails)
summary(cocktail_fit_2)

## MULTIPLE REGRESSION
# Syntax for interactions
# Add multiple predictors: <outcome variable> ~ <predictor 1> + <predictor 2>
lm(abv ~ acid + sugar, data = cocktails) %>% summary()
# Add multiple predictors AND their interactions: <outcome variable> ~ <predictor 1> * <predictor 2>
lm(abv ~ acid * sugar, data = cocktails) %>% summary()
# Add ONLY the interaction of predictors: <outcome variable> ~ <predictor 1> : <predictor 2>
lm(abv ~ acid : sugar, data = cocktails) %>% summary()
# You can combine these
lm(abv ~ acid : sugar + type, data = cocktails) %>% summary()

# Model selection
