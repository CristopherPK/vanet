genetic <- function(T, k, thresh){
  # initial set with all intersections. 
  S <- 1:nrow(T);
  
  # initializing empty result list.
  s <- c();
  
  # population size
  pop_size <- 400;
  population <- data.frame();
  
  # generating random population.
  pop_rand <- ceiling(pop_size/2); # half of the population is going to be random.
  for(i in 1:pop_rand){
    population <- rbind(population, sample(1:nrow(T), k));
  }
  
  # generating greedy population.
  pop_greedy <- pop_size - pop_rand;
  for(i in 1:pop_greedy){
    population <- rbind(population, greedy(T, k, thresh));
  }
  
  while(k > 0){
    
    
    k <- k -1;
  }
  
  s;
  
}

fitness <- function(genome){
  
}