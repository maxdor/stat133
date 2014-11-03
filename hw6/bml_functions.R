#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  points <- runif(r * c)
  points[points <= p/2] <- 1
  points[points > p/2 & points < p] <- 2
  points[points < 1] <- 0
  
  m <- matrix(points, nrow = r, ncol = c)
  
  return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  n <- m
  grid.new <- FALSE
  
  # Move red cars
  for (j in 1:ncol(m)) {
    for (i in 1:nrow(m)) {
      if (m[i, j] == 1) {
        newj <- ifelse(j + 1 > ncol(m), 1, j + 1)
        
        if (m[i, newj] == 0) {
          grid.new <- TRUE
          n[i, j] <- 0
          n[i, newj] <- 1
        }
      }
    }
  }
  
  m <- n
  
  # Move blue cars
  for (i in nrow(m):1) {
    for (j in 1:ncol(m)) {
      if (m[i, j] == 2) {
        newi <- ifelse(i - 1 == 0, nrow(m), i - 1)
        
        if (m[newi, j] == 0) {
          grid.new <- TRUE
          n[i, j] <- 0
          n[newi, j] <- 2
        }
      }
    }
  }
  
  m <- n
  return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m <- bml.init(r, c, p)
  limit <- 1e4
  moved <- TRUE
  iter <- 1
  
  while (moved == TRUE & iter < limit) {
    result <- bml.step(m)
    m <- result[[1]]
    moved <- result[[2]]
    iter <- iter + 1
  }
  
  return(list(Free_Flowing = moved, Iterations = iter))
}
