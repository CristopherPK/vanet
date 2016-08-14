GeneticAlgorithm <- function(T, k, thresh){
  # Calculating boundaries
  num.Vehicles <- nrow(T)
  num.Inter <- length(T[[1]])
  
  # initial set with all intersections. 
  S <- num.Inter
  
  # initializing empty result list.
  s <- c()
  
  # population size
  num.Gen <- 100
  p.Cross <- 0.95
  p.Mut <- 0.1
  pop.Size <- 400
  
  
  print("initializing population.")
  
  population <- data.frame()
  # generating random population.
  pop.Rand <- ceiling(pop.Size/2) # half of the population is going to be random.
  for(i in 1:pop.Rand){
    population <- rbind(population, sample(1:num.Inter, k))
  }
  print("random population generated.")
  
  # generating greedy population.
  pop.Greedy <- pop.Size - pop.Rand
  for(i in 1:pop.Greedy){
    population <- rbind(population, Greedy(T, k, thresh))
  }
  print("greedy population generated.")
  
  print("starting evolutionary approach.")
  while(num.Gen > 0){
    
    # calculating population fitness.
    pop.Fitness <- c()
    for(i in 1:nrow(population)){
      pop.Fitness <- cbind(pop.Fitness,CalcFitness(T,unlist(population[i,])))
    }
    
    # performing tournament selection with size 2. 
    # TODO: edit this function to accept different size of tournament selection.
    parents <- TournamentSelection(pop.Fitness)
    
    # Execute one-point crossover with probability pcross
    childs <- GenerateChilds(population, parents, num.Inter)
    
    # Execute one-point mutation with probability pmut
    childs <- DoMutation(num.Inter, childs)
    
    # Choose the elitist population for the next generation
    
    # Get the best from the first generation as criteria. 
    which(pop.Fitness == max(pop.Fitness))
    
    num.Gen <- num.Gen - 1
  }
  
  s
  
}

# Fitness calculation based on vehicles coverage.
CalcFitness <- function(T, genome){
  # subsetting those in the genome. 
  V <- T[genome,]
  V.Sum <- sapply(V,sum)
  n.Covered <- length(which(V.Sum >= thresh))
  coverage <- (n.Covered/nrow(T))*100
  coverage
}

# Tournament selection with size 2 function.
TournamentSelection <- function(pop.Fitness, size = 2){
  parents <- c()
  pop.Rand <- sample(1:length(pop.Fitness))
  for(i in 1:length(pop.Rand)){
    if((i %% 2) > 0){
      x1 <- pop.Fitness[pop.Rand[i]]
      x2 <- pop.Fitness[pop.Rand[i+1]]
      if(x1 > x2){
        parents <- cbind(parents,pop.Rand[i])
      } else {
        parents <- cbind(parents,pop.Rand[i+1])
      }
    }
  }
  parents
}

CorrectRepetition <- function(genome,num.Inter){
  g <- genome
  while(anyDuplicated(g) > 0){
    g[anyDuplicated(g)] <- sample(1:num.Inter, size = 1)
  }
  g
}

# Create a new generation based on the crossover method chosen.
GenerateChilds <- function(population, parents, num.Inter, crossover.Points = 1){
  childs <- c()
  len.Genome <- length(population[parents[1],])
  for(p in 1:length(parents)){
    if((i %% 2) > 0){
      # add the pcross as criteria for reproduction.
      if(sample(0:1, size = 1, prob = c(1-p.Cross, p.Cross))){
        point <- sample(1:len.Genome,size = 1)
        
        p1 <- unlist(population[parents[i],])
        t1 <- p1[1:point-1]
        t3 <- p1[point:length(p1)]
        
        p2 <- unlist(population[parents[i+1],])
        t2 <- p2[point:length(p2)]
        t4 <- p2[1:point-1]
        
        c1 <- append(t1,t2)
        names(c1) <- NULL
        c1 <- CorrectRepetition(c1,num.Inter)
        childs <- rbind(childs, c1)
        
        c2 <- append(t3,t4)
        names(c2) <- NULL
        c2 <- CorrectRepetition(c1,num.Inter)
        childs <- rbind(childs, c2)
      }
    }
  }
  childs
}

# Doing mutation based on its probability
DoMutation <- function(num.Inter, childs){
  childs.Mut <- c()
  for(i in 1:nrow(childs)){
    genome <- childs[i,]
    if(sample(0:1, size = 1, prob = c(1-p.Mut, p.Mut))){
      mut.Gene <- sample(1:length(genome), size = 1)
      genome[mut.Gene] <- sample(1:num.Inter, size = 1)
    }
    genome <- CorrectRepetition(genome, num.Inter)
    childs.Mut <- rbind(childs.Mut, genome)
  }
  childs.Mut
}