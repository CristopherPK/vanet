Evolutionary <- function(T = matrix(), k, thresh){
  # define evolutionary params
  num.Gen <- 100
  p.Cross <- 0.90
  p.Mut <- 0.10
  len.Pop <- 200
  len.Pop.Rand <- ceiling(len.Pop/2) # half of the population is going to be random.
  len.Pop.Greedy <- len.Pop - len.Pop.Rand
  
  # get info about the data
  num.Intersections <- nrow(T)
  num.Vehicles <- ncol(T)
  
  # initialize population
  population <-sapply(1:len.Pop.Rand, 
                     function(x) GenerateIndividual(
                      sample(1:num.Intersections,size = k),
                      num.Intersections
                      )
                     )
  
  # cbind 100:200
  population <- cbind(population,
                      sapply( 1:len.Pop.Greedy, 
                      function(x) GenerateIndividual(
                        RandomGreedyAlgorithm(T,k,thresh),
                        num.Intersections
                      )
                ))
  
  # start while loop
  while(num.Gen > 0){
    # evaluate individuals according with fitness function
    pop.Fitness <- sapply(1:ncol(population), function(x) Fitness(T,population[,x],thresh))
    best <- which(pop.Fitness == max(pop.Fitness))
    
    new.Population <- matrix(population[,best],nrow = num.Intersections)
    
    while(ncol(new.Population) < len.Pop){
      # choose parents
      parents <- TournamentSelection(pop.Fitness)
      
      # execute one-point crossover with probability pcross
      children <- GenerateChildren(population, parents, num.Intersections,1,p.Cross)
      
      # execute one-point mutation with probability pmut
      children <- DoMutation(num.Inter, children, p.Mut)
      
      # fixing n of RSUs in a genome
      children <- sapply(1:nrow(children), function(i) CorrectGenome(children[i,],k))
      new.Population <- cbind(new.Population, children)
    }
    
    population <- new.Population[,1:200]
    
    num.Gen <- num.Gen - 1
    print(max(pop.Fitness))
    print(num.Gen)
  }
  
  result.Fitness <- sapply(1:ncol(population), function(x) Fitness(T,population[,x],thresh))
  result <- which(result.Fitness == max(result.Fitness))
  
  population[,result]
  
}

CorrectGenome <- function(genome, k){
  pos <- which(genome == TRUE)
  if(length(pos) > k){
    while(length(pos) > k){
      i <- sample(pos,size = 1)
      genome[i] <- FALSE
      pos <- which(genome == TRUE)
    }
  } else {
    #print(pos)
    while(length(pos) < k){
      if(length(pos) == 0){
        i <- sample((1:length(genome)),size = 1)  
      } else {
        i <- sample((1:length(genome))[-pos],size = 1)
      }
      genome[i] <- TRUE
      pos <- which(genome == TRUE)
    }
  }
  genome
}

GenerateIndividual <- function(points, length){
  individual <- vector("logical", length)
  individual[points] <- TRUE
  # returns
  individual
}

# Fitness calculation based on vehicles coverage.
# T : matrix of intersections x vehicles
# individual : logical vector with deployment points defined as TRUE. 
# thresh: time required for transmission.
Fitness <- function(T, individual, thresh){
  # subsetting those in the genome. 
  pos <- which(individual == TRUE)
  V <- sapply(1:ncol(T), function(i) unlist(T[,i])[pos])
  V.Sum <- colSums(V)
  n.Covered <- length(which(V.Sum >= thresh))
  # n.Covered <- length(which(V.Sum > 0))
  coverage <- (n.Covered/ncol(T))*100
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

# Create a new generation based on the crossover method chosen.
GenerateChildren <- function(population, parents, num.Inter, crossover.Points = 1, p.Cross){
  childs <- c()
  len.Genome <- length(population[,parents[1]])
  for(i in 1:length(parents)){
    if((i %% 2) > 0 && (i < length(parents))){
      # add the pcross as criteria for reproduction.
      if(sample(0:1, size = 1, prob = c(1-p.Cross, p.Cross))){
        point <- sample(1:len.Genome,size = 1)
        
        p1 <- population[,parents[i]]
        t1 <- p1[1:point-1]
        t3 <- p1[point:length(p1)]
        
        p2 <- unlist(population[,parents[i+1]])
        t2 <- p2[point:length(p2)]
        t4 <- p2[1:point-1]
        
        c1 <- append(t1,t2)
        childs <- rbind(childs, c1)
        
        c2 <- append(t3,t4)
        childs <- rbind(childs, c2)
      }
    }
  }
  childs
}

# Doing mutation based on its probability
DoMutation <- function(num.Inter, children, p.Mut){
  children.Mut <- c()
  for(i in 1:nrow(children)){
    genome <- children[i,]
    if(sample(0:1, size = 1, prob = c(1-p.Mut, p.Mut))){
      mut.Gene <- sample(1:length(genome), size = 1)
      if(genome[mut.Gene] == FALSE){
        genome[mut.Gene] = TRUE
      } else {
        genome[mut.Gene] = FALSE
      }
    }
    children.Mut <- rbind(children.Mut, genome)
  }
  children.Mut
}