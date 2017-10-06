# Data visualization
# Quick tutorial for R Studio

x <- 127:987
x_mean <- mean(x)
x - x_mean
# What should be the mean of this vector? Now, calculate this in the console. Obviously, it's 0! 
# Congrats, you've just learned centring! This helps to get rid of baseline differences.

# Let's also calucluate the standard deviation, and name it x_sd!
x_sd <- sd(x)
# And devide each value of x with the sd of the vector
x/x_sd
# This is called scaling. This helps to get rid of differences in a scale, for eg. if someone is using 

# When using centering and scaling together, it is called standardisation, or z-score. This helps to make 
(x - x_mean)/x_sd
# This gives the same result as
scale(x) # Both centers and scales. You don't have to use parameters, as centering and scaling is the same
# You can choose to have only centering, or scaling 

scale(x, center = TRUE, scale = FALSE) # Only centers
scale(x, center = FALSE, scale = T) # Only scales (mind that you can abbreviate TRUE to T, but this is case sensitive!)


# Summary tables
# Most common plots, and how to understand them (scatter plot/bin plot, histogram/density plot, time series plot, bar plot, pie/tile chart, area plot/stacked bar chart, box/violin plot, heatmap, connectivity plot, geoplot, surface plot)
# Reporting data
# Infographics, data journalism and graphic storytelling

library(ggplot2) # Load the package that we will use for visualization

# Load the data from the internet
# We can read a data frame from a file or directly from the internet.
# As this file is an R data object, reading it instantly adds it to the environment
load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))

# Alternatively, you can download the data file, and read it from your hard drive. But in this case, you need to assign it to a variable
movies <- read.csv("datasets/movies.csv") # Don't forget to specify the path of the file!

# Checking the data
movies
# Also, you can check the data in a scrollable way in the upper right corner, when clicking on the "movies" text. Btw, this is the same as typing `View(movies)` in the consol (mind the capital V).

head(movies) # This shows the first 5 rows of the dataset

# So, how much the audience and critics score agree?
ggplot(data = movies) + # Call the plotting, define the dataset # Elements can be added to a plot using `+`
    aes(x = audience_score, y = critics_score) + # Define aesthetics, that have a variable value
    geom_point() # geoms are the actual plots

# So we created a scatter plot
plot1 <- # We can store the plot in a variable, so it will only be plotted if we explicitly print the variable
ggplot(data = movies) +
    aes(x = audience_score, y = critics_score, color = genre) + # Let's add color, based on genre
    geom_point(aes(shape = mpaa_rating)) # We can also add aesthetics in the geom. Let's add shape too, based on mpaa_rating rating. Don't forget to wrap the aesthetic into aes()!

plot1 # This will print the plot

ggplot(data = movies) +
    aes(x = audience_score, y = critics_score, color = genre) + # Let's add color, based on genre
    geom_point(shape = "+", aes(size = log(imdb_num_votes))) # If you want to give constant values, you don't have to use aes(). In that case, define these constants in the geom. You can use contant and variable aesthetics in the same time, but not for the same aesthetic.



movies %>% 
    arrange(-imdb_rating) %>% 
    as.data.frame() %>% 
    head()











