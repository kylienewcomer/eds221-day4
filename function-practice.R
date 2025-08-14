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
  


