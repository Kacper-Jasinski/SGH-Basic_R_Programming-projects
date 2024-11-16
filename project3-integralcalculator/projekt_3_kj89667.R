### Kacper Jasinski kj89667 ###

### Clean list

rm(list=ls())

#################### FUNCTION DEFINITION ####################

### integrate3d function is used to find integral approximation for functions taking only positive values
integrate3d <- function(f, over, n){
  ### Assign x and y values from over to x/y_min/max 
  x_min <- over[[1]][1]
  x_max <- over[[1]][2]
  y_min <- over[[2]][1]
  y_max <- over[[2]][2]
  
  ### Modify function so it takes one two dimensional argument instead of two arguments (necessary to use optim function)
  modified_f <- function(xy){
    x <- xy[1]
    y <- xy[2]
    return(f(x,y))
  }
  
  ### Find max for the given function and assign min value to 0 
  f_max <- optim(par = c(c(0,0),c(0,0)), fn = modified_f, method = "L-BFGS-B", lower = c(x_min,y_min), upper = c(x_max,y_max), control = list(fnscale = -1))$value
  f_min <- 0
  
  ### Generate samples for x and y
  x_samples <- runif(n, min = x_min, max = x_max)
  y_samples <- runif(n, min = y_min, max = y_max)
  f_samples <- runif(n, min = f_min, max = f_max)
  
  ### Calculate f values for given samples
  f_values <- f(x = x_samples, y = y_samples)
  
  ### Calculate volume for analysis rectangle
  s_volume <- prod(x_max - x_min, y_max - y_min, f_max)
  
  ### Calculate integral approximation
  # Calculate ratio for f_values that are under f_samples
  i_part <- f_samples[f_samples <= f_values] 
  ratio <- length(i_part) / length(f_samples)
  
  # Calculate integral approximation
  i_approx <- ratio * s_volume
  
  return (i_approx)
}

#################### EXAMPLES FROM http://michal.ramsza.org/lectures_20231/pl_basic_r/proj3/project.html ####################

### Example 1
f1 = function(x,y) {cos(x) * y}
over1 = list(x = c(0, pi / 2), y = c(0, 1))

### Example 2
f2 = function(x,y) {(cos(x) + 2) * (sin(y) + 1)}
over2 = list(x = c(0, pi), y = c(0, pi))

### Example n values
n1 = 10^2
n2 = 10^5

#################### FUNCTION CALLS FOR EXAMPLES ####################

print("### EXAMPLES FROM http://michal.ramsza.org/lectures_20231/pl_basic_r/proj3/project.html ###")
print("Example 1.1 n = 10^2")
print(integrate3d(f = f1, over = over1, n = n1))
print("Example 1.2 n = 10^5")
print(integrate3d(f = f1, over = over1, n = n2))
print("Example 2.1 n = 10^2")
print(integrate3d(f = f2, over = over2, n = n1))
print("Example 2.2 n = 10^5")
print(integrate3d(f = f2, over = over2, n = n2))
