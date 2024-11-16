### Kacper Jasinski kj89667 ###

### Clean list

rm(list=ls())

#################### FUNCTION DEFINITION ####################

### getAllPureStrategyNE function is used to find all Nash Equilibria in a normal finite game
getAllPureStrategyNE <- function(game){
  ### Initialize nPlayers variable
  nPlayers <- length(game)
  
  ### Ensure proper usage
  # Check if there are at least two players
  if (nPlayers < 2){
    stop("The game should be defined with at least two players")
  }
  
  # Define the number of dimensions in the game
  gameDim <- nrow(as.matrix(game))
  
  
  # Initialize names vector with empty string and empty nStrategies list
  names <- ""
  nStrategies <- list()
  
  # Iterate through all of the players
  for (n in 1:nPlayers){
    # Save player name and number of strategies for that player to name and strategies variables
    name <- paste0("player ", n)
    strategies <- list(1:2)
    
    # Extend names and nStrategies vectors with name and strategies values
    names <- c(names, name)
    nStrategies <- append(nStrategies, strategies, after = length(nStrategies))
  }
  
  # Clear names deleting the initialization value
  names <- names[2:length(names)]
  
  # Create profiles
  profiles <- expand.grid(nStrategies)
  names(profiles) <- names
  
  ### Create payoffs matrix for all of the combinations
  # Initialize payoffs
  payoffs <- 0
  
  # Iterate through every row in profiles

  for (r in 1:nrow(profiles)){
    # Iterate through every column
    for (c in 1:ncol(profiles)){
      payoffs <- c(payoffs, game[[c]][r])
    }
  }
  
  # Clean payoffs
  payoffs <- payoffs[2:length(payoffs)]
  
  # Create matrix of payoffs for every player depending on strategy profile
  payoffs <- matrix(payoffs, nrow = nrow(profiles), ncol = ncol(profiles), byrow = TRUE)
  
  ### Find all of Nash equilibria
  # Define equlibria variable to an empty list
  equilibria = list()
  
  # Iterate through every row in payoffs
  for (c in 1:ncol(payoffs)){
    # Iterate through every column in payoffs
    for (r in 1:nrow(payoffs)){
      if (all(payoffs[r, c] == apply(payoffs, 2, max)[c])) {
        equilibria[[length(equilibria) + 1]] <- c(profiles[r,])
      }
    }
  }
  
  ### Transform equilibria matrix
  # Keep only distinct values
  equilibria <- unique(equilibria)
  # Transform equilibria to matrix
  equilibria <- matrix(unlist(equilibria), nrow = length(equilibria), ncol = length(equilibria[[1]]), byrow=TRUE)
  
  # Convert equilibria to final list
  NE <- list()
  names <- ""
  for (r in 1:nrow(equilibria)){
    name <- ""
    equilibrium <- 0
    for (c in 1:ncol(equilibria)){
      equilibrium <- c(equilibrium, equilibria[r,c])
      name <- paste0(name, equilibria[r,c])
    }
    equilibrium <- equilibrium[2:length(equilibrium)]
    names <- c(names, name)
    NE <- append(NE, list(equilibrium))
  }
  
  # Clear names vector
  names <- names[2:length(names)]
  
  # Change names for Nash Equilibria
  names(NE) <- names
  
  return (NE)
}

#################### EXAMPLES FROM http://michal.ramsza.org/lectures_20231/pl_basic_r/proj2/project.html ####################

### Example 1 (coordination game for two players)
game1 <- list(
  "player 1" = array(c(1, 0, 0, 1), dim = c(2, 2)),
  "player 2" = array(c(1, 0, 0, 1), dim = c(2, 2))
)

### Example 2 (anti-coordination game for three players)
game2 <- list(
  "player1" = array(c(0, 1, 1, 1, 1, 1, 1, 0), dim = c(2, 2, 2)),
  "player2" = array(c(0, 1, 1, 1, 1, 1, 1, 0), dim = c(2, 2, 2)),
  "player3" = array(c(0, 1, 1, 1, 1, 1, 1, 0), dim = c(2, 2, 2))
)

### Example 3 (coordination game for four players)
game3 <- list(
  "player1" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2)),
  "player2" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2)),
  "player3" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2)),
  "player4" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2))
)

#################### FUNCTION CALLS FOR EXAMPLES ####################

print("### EXAMPLES FROM http://michal.ramsza.org/lectures_20231/pl_basic_r/proj2/project.html ###")
print("Example 1 coordination game for two players")
print(getAllPureStrategyNE(game=game1))
print("Example 2 anti-coordination game for three players")
print(getAllPureStrategyNE(game=game2))
print("Example 3 coordination game for four players")
print(getAllPureStrategyNE(game=game3))
