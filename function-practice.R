birddog_sum <- function(bird, dog) {
  pets = bird + dog
  return(pets)
}


total_pets <- birddog_sum(bird = 2, dog = 5)


#create a function to double values
double_it <- function(x) {
  print(2 * x)
}

double_it(5)

double_it(c(1,0.5,-200))

exclaim_age <- function(age){
  print(paste("I am", age, "years old!"))
}

exclaim_age(10)

animal_age <- function(animal, age){
  if(animal == "dog"){
    print(age*7)
  } else if(animal == "goat")
    print(age * 4.7)
}
animal_age(animal = "cow", age = 8)

#write updated animal age function with error messages

animal_age_stop <- function(animal, age){
  if(!animal %in% c("dog", "goat")) {
    stop("Oops! Animal must be dog or goat.")} 
  if(is.numeric(age) == FALSE){
    stop("The age must be a number")
  }
  if(age <= 0 | age > 50) {
    warning("are you sure about your animal's age?")
  }
if(animal == "dog"){
    print(age*7)
  } else if(animal == "goat")
    print(age * 4.7)
}

animal_age_stop("elephant", 10)
animal_age_stop("dog", 51)
  
# functions meet for loops



#all dataframes in the function are df
df_means <- function(df) {
  for (i in 1:ncol(df)) {
    if(is.numeric(df[[i]]))
    col_mean <- mean(df[[i]])
    column_name <- colnames(df[i])
    print(paste("The mean value of", column_name, "is", col_mean))
  }
}

df_means(df = mtcars)


df_means(palmerpenguins::penguins)


#logistic growth example

logistic_growth <- function(N0, K, r, time){
  Nt <- K / (1 + ((K-N0)/N0)*exp(-r * time))
  print(Nt)
}
logistic_growth(100,6000,0.27,40)

#working on an example with just time
time_vec <- seq(from = 0, to = 35, by = 0.1)

pop_35 <- logistic_growth(100, 6000, 0.27, time_vec)
#combining time steps and population size into a data frame

pop_time_25 <- data.frame(time_vec, pop_35)
 # plot it
library(tidyverse)
ggplot(pop_time_25, aes(time_vec, pop_35)) +
  geom_line(size = 0.5)

pop_35_vec <- vector(mode = "numeric", length = length(time_vec))
# alternatively with a internal for loop
for(i in seq_along(time_vec)){
  population <- logistic_growth(100, 6000, 0.27, time = time_vec[i])
  pop_35_vec[i] <- population
}

# now building to estimating across growth rates
r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)


out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))
for(i in seq_along(time_vec)){
  for(j in seq_along(r_seq)){
    population <- logistic_growth(100, 6000, r_seq[j], time_vec[i])
    out_matrix[i,j] <- population
  }
}

# data wrangling to data frame

#adding time as variable
out_df <- data.frame(out_matrix, time = time_vec)

#update column names for growth rates
colnames(out_df) <- c(paste0("gr_", r_seq),"time")

# pivet longer

out_df_long <- out_df %>% 
  pivot_longer(cols = -time, names_to = "growth_rate",
               values_to = "population")

# plot it

ggplot(out_df_long, aes(time, population, color = growth_rate)) +
  geom_line() +
  theme_classic()
 