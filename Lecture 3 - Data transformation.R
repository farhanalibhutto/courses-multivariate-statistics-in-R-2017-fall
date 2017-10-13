### Lecture 3
# Install the tidyverse package. All the packages that we will use today are included in it
install.packages("tidyverse")

# Let's try the pipe operator
# The pipe is in several packages, for e.g. the magrittr package
library(magrittr)

# Take the following vector
x <- c(55:120, 984, 552, 17, 650)

# Creating a pipeline of commands. Of course, the sorting does not change the result
x %>%
    sort() %>%
    subtract(5) %>%
    divide_by(3) %>%
    sd()

# [1] 46.02999
# Let's load the dplyr package, which is for data transformation in data frames
library(dplyr) 

# Let's use the ToothGrowth data once again, and practice with the pipe opeartor
# Filter the rows that 
ToothGrowth %>%
    filter(supp == "OJ")

# Let's creare a new variable, which which returns tooth length in cm, not mm. Use mutate()
ToothGrowth %>%
    mutate(len_cm = len / 10)

# Let's see the average tooth length in centimeters, use summarise()
# This takes several data and summarizes it accoring to our wishes
# The result will not contain the original data
ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    summarise(mean_len_cm = mean(len_cm))

# Let's also calculate the nuber of cases, using the function n()
ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    summarise(mean_len_cm = mean(len_cm),
              cases = n())

# It makes the most sense if we also create groups but BEFORE summarise, using group_by()
tooth_results <-
    ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    group_by(supp) %>%
    summarise(mean_len_cm = mean(len_cm),
              cases = n())

# You ca also use the grouping with mutate. Then it adds the group means and number of cases to the original data
# This way, the result will also contain the original data AND the summary statistics with redundancy
ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    group_by(supp) %>%
    mutate(mean_len_cm = mean(len_cm),
           cases = n())

# We can also arrange the results based on a variable
tooth_results %>% 
    arrange()


# Practice on the gapminder data. First install it, than load the data 
install.packages("gapminder")
gapminder <- gapminder::gapminder

# Gapminder contains the data of several countries at tifferent times.
# It is in tidy format. Check the codebook
?gapminder::gapminder

# Task:
gapminder %>% 
    filter(year == c(1952, 1957)) %>% 
    group_by(continent) %>% 
    summarise(life_exp_med = median(lifeExp)) %>% 
    arrange(-life_exp_med)
    


