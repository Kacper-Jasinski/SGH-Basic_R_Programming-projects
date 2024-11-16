### Kacper Jasinski kj89667 ###

### Libraries
library(magick)

### Clean list
rm(list=ls())

#################### FUNCTION DEFINITION ####################

### pathQ function is an implementation of (sort of) A* path finding algorithm
pathQ <- function(map, start_point, end_region){
  ### Start timer
  start_time <- Sys.time()
  
  ### Assign nrow and ncol to variables
  n_rows <- nrow(map)
  n_cols <- ncol(map)
  
  ### Define an internal function to check if point is within the end region
  in_end_region <- function(x, y){
    if(x >= min(end_region$x) && x <= max(end_region$x) &&
       y >= min(end_region$y) && y <= max(end_region$y)){
      return (TRUE)
    }
    return (FALSE)
  }
  
  ### Define an internal function to check valid neighbors of the point
  get_neighbors <- function(x, y){
    # Get all of the neighbors
    neighbors <- cbind(x + c(1, -1, 0, 0), y + c(0, 0, 1, -1))
    # Validate the neighbors
    valid_neighbors <- neighbors[(neighbors[, 1] >= 1) & (neighbors[, 1] <= n_rows) &
                                   (neighbors[, 2] >= 1) & (neighbors[, 2] <= n_cols), ]
    return(valid_neighbors)
  }
  
  ### Initialize queue variable to an empty list
  queue <- list()
  
  ### Initialize visited matrix
  visited <- matrix(FALSE, n_rows, n_cols)
  
  ### Initialize the starting point
  start_x <- start_point$x
  start_y <- start_point$y
  
  ### If the point is valid add it to the top of the queue and mark as visited
  if (start_x >= 1 && start_x <= n_rows && start_y >= 1 && start_y <= n_cols && map[start_x, start_y]){
    queue <- c(queue, list(c(start_x, start_y)))
    visited[start_x, start_y] <- FALSE
  }
  
  ### Do while queue is not empty
  while (length(queue) > 0){
    # Assign top of queue to current variable
    current <- queue[[1]]
    # Dequeue queue from the top
    queue <- queue[-1]
    
    # Assign current coordinates to x and y
    x <- current[1]
    y <- current[2]
    
    ### Check if current point is in the end region
    if (in_end_region(x, y)){
      # Stop timer when point reached the end region
      end_time <- Sys.time() - start_time
      print(paste("Runtime: ", end_time, "seconds"))
      return(TRUE)
    }
    
    ### Check neighbors
    neighbors <- get_neighbors(x, y)
    
    ### Iterate through every neighbor
    for (neighbor in 1:length(neighbors[,1])){
      # Assign x and y coordinates from neighbor
      neighbor_x <- neighbors[neighbor,][1]
      neighbor_y <- neighbors[neighbor,][2]
      
      ### Check if neighbor is valid
      if (neighbor_x >= 1 && neighbor_x <= n_rows && neighbor_y >= 1 && neighbor_y <= n_cols && !visited[neighbor_x, neighbor_y] && map[neighbor_x, neighbor_y]){
        # Add neighbor to queue
        queue <- c(queue, list(c(neighbor_x, neighbor_y)))
        # Mark neighbor as visited
        visited[neighbor_x, neighbor_y] <- TRUE
      }
    }
  }
  
  end_time <- Sys.time() - start_time
  print(paste("Runtime", end_time, "seconds"))
  return(FALSE)
}
#################### READ THE IMAGE FROM http://michal.ramsza.org/lectures_20231/pl_basic_r/proj1/project.html ####################

d0 <- image_read(path = "./wallpaper.png")

### Get only the relevant part
dims <- rev(dim(as.raster(d0)))
d1 <- image_crop(image = d0, geometry = "800x800+560+140")

### Quantizing
d2 <- as.raster(image_quantize(image_normalize(d1), max = 2))

### Changing to TRUE / FALSE matrix
d3 <- array(
  ifelse(as.vector(d2) == unique(as.vector(d2))[1], TRUE, FALSE),
  dim(d2)
)

### Saving
saveRDS(object = d3, file = "./maze.RDS")

### Reading in the maze
d0 <- readRDS(file = "./maze.RDS")

#################### EXAMPLES FROM http://michal.ramsza.org/lectures_20231/pl_basic_r/proj1/project.html ####################
### StartPoint
startPoint <- list(x = 1, y = 1)

### Example 1
logoPosition <- list(x = 387:413, y = 322:348)
### Example 2
endPosition <- list(x = 220:230, y = 325:335)

#################### FUNCTION CALLS FOR EXAMPLES ####################

print("### EXAMPLES FROM http://michal.ramsza.org/lectures_20231/pl_basic_r/proj1/project.html ###")
print("Example 1 end_region = logoPosition")
print(pathQ(map = d0, start_point = startPoint, end_region = logoPosition))
print("Example 2 end_region = endPosition")
print(pathQ(map = d0, start_point = startPoint, end_region = endPosition))
