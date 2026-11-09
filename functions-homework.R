#PSYC 259 Homework - Writing functions and iteration
#For full credit, answer at least 5/7 questions

#List names of students collaborating with: 
# Aylin, Maddy, Megan 

### SETUP: RUN THIS BEFORE STARTING ----------
library(tidyverse)
set.seed(1)
id <- rep("name", 30)
x <- runif(30, 0, 10)
y <- runif(30, 0, 10)
z <- runif(30, 0, 10)
ds <- tibble(id, x, y, z)

### Question 1 ---------- 

#Vectors x, y, and z contain random numbers between 1 and 10. 

#1.Write a function called "limit_replace" that replaces values less than 2 or greater than 8 with NA

limit_replace <- function (vector) {
  ifelse(vector< 2 | vector>8, NA, vector)
}
#The function should take a vector as input and return the replaced-vector as an output

#2.Then, run the function on x and save the results to a new vector "x_replace" to show it worked

x_replace <- limit_replace(x)
x_replace
#it looks like it worked here!

### Question 2 ---------- 

#1.Make a new version of limit_replace includes arguments for a lower boundary and an upper boundary
#so that the limits can be specified (instead of being hard coded as 2 and 8)
new_limit_replace <- function(y, lower =4, upper=6) {x[!is.na(y)& (y<lower|y>upper)] <-NA
y
}

#2.Run the function on vector y with boundaries 4 and 6, saving the results to a new vector "y_replace"

new_limit_replace <- function(vector,lower, upper) {
  ifelse( vector < lower | vector > upper, NA, vector)
}

y_replace <- new_limit_replace(y,4,6)
y_replace



### Question 3 ----------

#Write a function called "plus_minus_SD" that can take one of the vectors (x/y/z) as input
#and "num_of_SDs" as an input and returns the boundaries +/- num_of_SDs around the mean.

#plus_minus_SD(x, 1) would give +/- 1SD around the mean of x, plus_minus_SD(y, 2) would give +/- 2SDs around the mean 


#Make num_of_SDs default to 1

#The output should be a named list two values, lower and upper.

#Running plus_minus_SD(x) should produce: $lower 2.142724 $upper 8.048324

plus_minus_SD <- function(vector, num_of_SDs = NULL){
  num <- ifelse(is.null(num_of_SDs), 1, num_of_SDs)
  SD <- sd(vector)
  SDs <- num * SD
  mean <- mean(vector)
  lower <- mean- SD
  upper <- mean + SD
  list <- c(lower, upper)
  return(list)
}
plus_minus_SD (x)


### Question 4 ----------
# Use a map function to run the new function on x, y, and z with 1 SD

vectors <- list(x,y,z)
map(vectors, plus_minus_SD)


### Question 5 ----------

# Write an another new version of limit_replace
# This time, make the upper and lower boundaries optional arguments
# If they are not given, use +/- 1 SD as the boundaries (from your plus_minus_SD function)
# Apply the function to each column in ds, and save the results to a new tibble called "ds_replace"
# Hint: you will need an if() statement to handle what to do if boundaries are not given

limit_replace3 <- function(vector, lower=NULL, upper= NULL) {
  SD<- sd(vector)
  mean<- mean(vector)
  L<- mean-SD
  U <- mean+ SD
  lower <- ifelse(is.null(lower), L, lower)
  upper <- ifelse(is.null(upper), U, upper)
  return(ifelse(vector <lower | vector > upper, NA, vector))
}

ds_replace <- map(vectors, limit_replace3)

### Question 6 ----------
# Question 6 will use the iris dataset
# Write a species_summary function that takes in a tibble and a species name
# For example, species_summary(ds_iris, species_name = "setosa")
# The function should filter to the named species and then calculate  
# the min, mean, max, and sd for every numeric variable
# Before returning the summary stats, the function should also write the 
# summary stats to a file named "SPECIESNAME_summary.csv"

ds_iris <- iris


### Question 7 ----------
# Use some type of iteration to run the species_summary() function 
# on each of the species in ds_iris, capturing the resulting summary data
# into a tibble


# If correct, you will have a 3 observation by 16 variable tibble (first 3 columns below)
# Sepal.Length_min Sepal.Length_mean  Sepal.Length_max 
# 4.3              5.006              5.8       
# 4.9              5.936              7.0       
# 4.9              6.588              7.9       
# And 3 separate CSV files saved as output