# analyzing the algorithms 

T0 <- data.matrix(read.table("Scenes/E0-T-60s-4CP"))
T1 <- data.matrix(read.table("Scenes/E1-T-60s-4CP"))

T3 <- data.matrix(read.table("Scenes/E3-T-60s-4CP"))

result <- GeneticAlgorithm(T,5,30)

# greedy result for T0 data set
g0 <- GenerateIndividual(GreedyAlgorithm(T0,5,30), nrow(T0))
Fitness(T0,g0,30)

# greedy result for T1 data set 
g1 <- GenerateIndividual(GreedyAlgorithm(T1,5,30), nrow(T1))
Fitness(T1,g1,30)

# T2
g2 <- GenerateIndividual(GreedyAlgorithm(T2,5,30),nrow(T2))
Fitness(T2,g2,30)

# T3
g3 <- GenerateIndividual(GreedyAlgorithm(T3,5,30),nrow(T3))
Fitness(T3,g3,30)