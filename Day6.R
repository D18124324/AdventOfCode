
decrement <- function(current_fish)
{
  if(current_fish == 0)
  {
    current_fish <- 6
  }
  else
  {
    current_fish <- current_fish - 1
  }

  return(current_fish)
}

simulate <- function(parent_generation,numOfDays)
{
  i <- 0
  while( i < numOfDays )
  {
    # Count the number of fish that will spawn.
    num_of_offspring <- length(which(parent_generation == 0))
    
    # Decrement the count for each fish.
    next_generation <- sapply(parent_generation,decrement)

    if(num_of_offspring > 0)
    {
      # Add the offspring to the list.
      next_generation <- append(next_generation, as.list(rep(8,num_of_offspring)))
    }

    parent_generation <- next_generation

    # Increment the loop.
    i <- (i + 1)
  }

  return(parent_generation)
}

# Create the initial state
initial_state <- list(3,4,3,1,2)

# Run simulation, the number of days is the second parameter.
output_list <- simulate(initial_state,80)

# Print the number of fish.
length(output_list)
