
run_simulation <- function(parent_generation,numOfDays)
{
  # Create empty list
  population_count <- as.list(rep(0,9))

  # Assign initial counts
  for( i in 0:6)
  {
    population_count[i+1] <- length(which(parent_generation == i))
  }

  i <- 0
  while( i < numOfDays )
  {
    # The number of spawning fish is the count at position 0
    num_of_offspring <- population_count[[1]]

     # Iteration over each position, moving the count for element n to 
    # element n-1 for elements 0 to 8 (ignore the last position as 
    # that will be overwritten by the number of spawning fish)
    for( j in 1:8)
    {
      population_count[j] <- population_count[[j+1]]
    }

    # Add the number of spawning fish to position 6
    population_count[7] <- population_count[[7]] + num_of_offspring

    # Set the number of offspring
    population_count[9] <- num_of_offspring

    # Increment the loop.
    i <- (i + 1)
    num_of_offspring <- NULL
  }

  # Compute the total  
  total <- 0
  for( c in 1:9)
  {
    total <- total + population_count[[c]]
  }
  
  return(total)
}

# Create the initial state
initial_state <- list(4,1,1,4,1,2,1,4,1,3,4,4,1,5,5,1,3,1,1,1,4,4,3,1,5,3,1,2,5,1,1,5,1,1,4,1,1,1,1,2,1,5,3,4,4,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,5,1,1,1,4,1,2,3,5,1,2,2,4,1,4,4,4,1,2,5,1,2,1,1,1,1,1,1,4,1,1,4,3,4,2,1,3,1,1,1,3,5,5,4,3,4,1,5,1,1,1,2,2,1,3,1,2,4,1,1,3,3,1,3,3,1,1,3,1,5,1,1,3,1,1,1,5,4,1,1,1,1,4,1,1,3,5,4,3,1,1,5,4,1,1,2,5,4,2,1,4,1,1,1,1,3,1,1,1,1,4,1,1,1,1,2,4,1,1,1,1,3,1,1,5,1,1,1,1,1,1,4,2,1,3,1,1,1,2,4,2,3,1,4,1,2,1,4,2,1,4,4,1,5,1,1,4,4,1,2,2,1,1,1,1,1,1,1,1,1,1,1,4,5,4,1,3,1,3,1,1,1,5,3,5,5,2,2,1,4,1,4,2,1,4,1,2,1,1,2,1,1,5,4,2,1,1,1,2,4,1,1,1,1,2,1,1,5,1,1,2,2,5,1,1,1,1,1,2,4,2,3,1,2,1,5,4,5,1,4)

# Run simulation, the number of days is the second parameter.
fish_count <- run_simulation(initial_state,256)
print(fish_count, digits=16)
