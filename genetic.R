GeneticAlgorithm <- function(T = matrix(), k, thresh){
  # Calculating boundaries
  num.Vehicles <- nrow(T)
  num.Inter <- length(T[[1]])
  
  # initial set with all intersections. 
  S <- num.Inter
  
  # initializing empty result list.
  s <- c()
  best <- c()
  
  # population size
  num.Gen <- 100
  p.Cross <- 0.90
  p.Mut <- 0.10
  pop.Size <- 200
  
  
  print("initializing population.")
  
  # generating random population.
  pop.Rand <- ceiling(pop.Size/2) # half of the population is going to be random.
  population <- sapply(1:pop.Rand, function(i) sample(1:num.Inter, k))
  print("random population generated.")
  
  # generating greedy population.
  pop.Greedy <- pop.Size - pop.Rand
  print("starting greedy population")
  population <- cbind(population, sapply(1:pop.Greedy, function(x) GreedyAlgorithm(T,k,thresh)))
  print("greedy population generated.")
  
  print("starting evolutionary approach.")
  while(num.Gen > 0){
    
    # Calculating population fitness.
    pop.Fitness <- sapply(1:ncol(population), function(i = X) CalcFitness(T,unlist(population[,i]),thresh))
    
    # Performing tournament selection with size 2. 
    # TODO: edit this function to accept different size of tournament selection.
    parents <- TournamentSelection(pop.Fitness)
    
    # Execute one-point crossover with probability pcross
    childs <- GenerateChilds(population, parents, num.Inter, p.Cross = p.Cross)
    
    # Execute one-point mutation with probability pmut
    childs <- DoMutation(num.Inter, childs, p.Mut)
    
    # Choose the elitist population for the next generation
    childs.Fitness <- sapply(1:nrow(childs), function(i = X) CalcFitness(T,unlist(childs[i,]),thresh))
    elite <- which(childs.Fitness >= mean(childs.Fitness))
    pop.Elite <- childs[elite,]
    
    print(max(childs.Fitness))
    best <- rbind(best,childs[which(childs.Fitness == max(childs.Fitness)),])
    
    # Generate random individuals for the next generation
    # TODO: Check if this approach is correct
    while(nrow(pop.Elite) < pop.Size){
      pop.Elite <- rbind(pop.Elite, sample(1:num.Inter, k))
    }
    population <- t(pop.Elite)
    
    num.Gen <- num.Gen - 1
    print(num.Gen)
  }
  
  print("finishing...")
  final.Fitness <- sapply(1:nrow(best), function(i = X) CalcFitness(T,unlist(best[i,]),thresh))
  
  it.Best <- which(final.Fitness == max(final.Fitness))
  
  s <- best[it.Best,]
  
  s
  
}

# Fitness calculation based on vehicles coverage.
CalcFitness <- function(T, individual, thresh){
  # subsetting those in the genome. 
  V <- c()
  V <- sapply(X = T, FUN = function(x) rbind(V,x[individual]))
  V.Sum <- colSums(V)
  n.Covered <- length(which(V.Sum >= thresh))
  coverage <- (n.Covered/nrow(T))*100
  coverage
}

# Tournament selection with size 2 function.
TournamentSelection <- function(pop.Fitness, size = 2){
  parents <- c()
  pop.Rand <- sample(1:length(pop.Fitness))
  for(i in 1:length(pop.Rand)){
    if((i %% 2) > 0 && (i < length(pop.Rand))){
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

# Remove duplicated in an individual.
CorrectDuplicated <- function(genome,num.Inter){
  g <- genome
  while(anyDuplicated(g) > 0){
    g[anyDuplicated(g)] <- sample(1:num.Inter, size = 1)
  }
  g
}

# Create a new generation based on the crossover method chosen.
GenerateChilds <- function(population, parents, num.Inter, crossover.Points = 1, p.Cross){
  childs <- c()
  len.Genome <- length(population[,parents[1]])
  for(i in 1:length(parents)){
    if((i %% 2) > 0 && (i < length(parents))){
      # add the pcross as criteria for reproduction.
      if(sample(0:1, size = 1, prob = c(1-p.Cross, p.Cross))){
        point <- sample(1:len.Genome,size = 1)
        
        p1 <- unlist(population[,parents[i]])
        t1 <- p1[1:point-1]
        t3 <- p1[point:length(p1)]
        
        p2 <- unlist(population[,parents[i+1]])
        t2 <- p2[point:length(p2)]
        t4 <- p2[1:point-1]
        
        c1 <- append(t1,t2)
        names(c1) <- NULL
        c1 <- CorrectDuplicated(c1,num.Inter)
        childs <- rbind(childs, c1)
        
        c2 <- append(t3,t4)
        names(c2) <- NULL
        c2 <- CorrectDuplicated(c2,num.Inter)
        childs <- rbind(childs, c2)
      }
    }
  }
  childs
}

# Doing mutation based on its probability
DoMutation <- function(num.Inter, childs, p.Mut){
  childs.Mut <- c()
  for(i in 1:nrow(childs)){
    genome <- childs[i,]
    if(sample(0:1, size = 1, prob = c(1-p.Mut, p.Mut))){
      mut.Gene <- sample(1:length(genome), size = 1)
      genome[mut.Gene] <- sample(1:num.Inter, size = 1)
    }
    genome <- CorrectDuplicated(genome, num.Inter)
    childs.Mut <- rbind(childs.Mut, genome)
  }
  childs.Mut
}