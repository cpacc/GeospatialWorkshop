########################################################################################
# WORKSHOP IN GEOSPATIAL METHODS - SUPPLEMENTARY MATERIALS 
#   INTRODUCTION TO WORKING WITH DATA IN R
#
# Purpose: Illustration of basic data manipulation using tidy data tools.  This script
#   is meant to accompany the presentation, 'An Introduction to Working with Data in R".
# This script was written using R version 3.6.1 (2019-07-05) -- "Action of the Toes"
#
# Note: The R code below can be collapsed or expanded into sections for easy navigation:
#    collapse code - Alt+O
#    expand code - Shift+Alt+O
#
# Author: Michael Otterstatter, PhD
#
# Created: Nov 5, 2019
#
# 
########################################################################################



#### SETUP ####

library(tidyverse)


# by default, R will display 10 rows of output when data are stored as a tibble (rectangular dataset)
#   to change the number of rows dispalyed, set the following to a specific number or use 'Inf' 
#   to display all data rows

options(tibble.print_max = 10)



#### EXAMPLE DATASET #### 

# Using tibbles

# here, we use the example dataset 'iris' (included in base R).  These data exist as a 'data.frame', 
#   which is the  traditional style of rectangular dataset in R.  The modern and enhanced version of 
#   a dataframe is the 'tibble', or 'tbl' for short, which we use here. 

# start by creating a new object 'iris.t', which is a tibble version of the raw iris dataset 

iris.t <- as_tibble(iris)



# Viewing a dataset

# there are 3 tidy ways to view a dataset (or any object) in R:

# 1 - enter the name of the object (by default, R will show the first 10 rows) 

iris.t

# 2 - use the glimpse() command to see a listing of the variables and formats

glimpse(iris.t)

# 3 - use the View() command to open a spreadsheet version of the dataset

View(iris.t)



# You can also generate a quick summary of dataset using the summary() command,
#   which can be particularly useful for numeric variables

summary(iris.t)




#### TIDY DATA USING DPLYR ####

# if you want to see all of the output in the following examples, use the following

options(tibble.print_max = Inf)


# filter()

# subset rows of data using filter()

filter(iris.t, Species == 'virginica')

filter(iris.t, Species %in% c('setosa', 'virginica'))

filter(iris.t, Petal.Length > 6.0 & Petal.Width < 2)



# select()

# subset columns of data using select()

select(iris.t, Species, Petal.Width)

select(iris.t, -Petal.Width, -Petal.Length)

select(iris.t, contains("Length")) 

# note the helper function contains() -- for more helper functions see the data wrangling cheatsheet
#   at https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf



# mutate()

# add new variables (columns) using mutate()

mutate(iris.t, Sepal.Area = Sepal.Length * Sepal.Width)

mutate(iris.t, Sepal.Petal.Diff = Sepal.Length - Petal.Length)

mutate(iris.t, Avg.Petal.Length = mean(Petal.Length), Dev.from.Avg = Petal.Length - Avg.Petal.Length)



# group_by() and summarise()

# generate overall and group-level summaries with group_by() and summarise()

summarise(iris.t, Avg.Petal.Length = mean(Petal.Length))

# group_by() creates an invisible stratification in a dataset, and all operations
#   performed afterwards are done by the new strata (groups)

iris.grouped <- group_by(iris.t, Species)
summarise(iris.grouped, Avg.Petal.Length = mean(Petal.Length)) # calcualte the average petal length, by species

iris.grouped <- group_by(iris.t, Species)
filter(iris.grouped, rank(desc(Sepal.Width)) <= 2) # filter to only keep the top 2 in sepal width, by species



# Piping

# Data operations can be efficiently linked togetger using the 'piping' operator: %>%

# Rather than create a number of temporary datasets, we specify the original dataset once
#   and then pass it down a sequence of processing steps (using the piping operator) that 
#   result in our desired end product

# specify dataset:
avg_dev_by_species <- iris.t %>% 
  # then calculate deviation for each individual:
  mutate(Dev.from.Average = Petal.Length - mean(Petal.Length)) %>% 
  # then group by species:
  group_by(Species) %>% 
  # then calculate average deviation for each species group:
  summarise(Avg.Dev = mean(Dev.from.Average)) 

avg_dev_by_species



# left_join(), inner_join() and full_join()

# combining datasets

# for illustration, we a create unique ID for each individual and then 
#   create separate datasets for petal measurements and sepal measurements

iris.id <- iris.t %>%
  mutate(ID = row_number())

iris.id

petal.data <- iris.id %>%
  select(ID, Species, Petal.Length, Petal.Width) %>%
  arrange(Petal.Length)

sepal.data <- iris.id %>%
  select(ID, Species, Sepal.Length, Sepal.Width) %>%
  arrange(Sepal.Length)

petal.data

sepal.data


# join the two datasets back together by species and ID so that the original combined dataset is recovered

joined_data <- full_join(petal.data, sepal.data, by = c("Species", "ID"))

joined_data

# here, we only provide an example of a full join (all rows retained); for other examples of joins
#   see the cheatsheet at https://stat545.com/join-cheatsheet.html




#### TIDY DATA USING TIDYR ####

# gather()

# reshaping data from wide-to-long format: collect separate columns together in key-value pairs 
#   using the gather() function

iris.t %>%
  select(Species, Sepal.Length, Sepal.Width) %>%
  gather(key = "Sepal.Attribute", value = "Measurement", Sepal.Length, Sepal.Width)

# the original iris.t dataset is not 'tidy' because related features (flower characteristics) have been placed 
#   in separate variables (columns), rather than being placed together. The gather() example above has made this 
#   dataset more 'tidy' by grouping sepal attributes together in a single column.  

# Ideally, we would group all flower characteristics together, not just the sepal attributes:

tidy_data <- iris.t %>%
  gather(key = 'Attribute', value = "Measurement", Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

tidy_data



# spread()

# reshaping data from long-to-wide format: separate a column with multiple groups using key-value pairs 
#   so that each group has it's own column, using the spread() function

# for example, create dataset of mean values, by species and attribute...
mean_attributes <- tidy_data %>% 
  group_by(Species, Attribute) %>%
  summarise(mean.value= mean(Measurement))

# ...and reshape from long-to-wide so that each species has its own column
mean_attributes %>%
  spread(key = Species, value = mean.value)

