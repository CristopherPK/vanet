# analyzing the algorithms 

T0 <- data.matrix(read.table("Scenes/E0-T-60s-4CP"))
T1 <- data.matrix(read.table("Scenes/E1-T-60s-4CP"))

T3 <- data.matrix(read.table("Scenes/E3-T-60s-4CP"))

result <- GeneticAlgorithm(T,5,30)

# greedy result for T0 data set
g0 <- GenerateIndividual(c(66,64,19,27,29), nrow(T0))
Fitness(T0,g0,30)

# greedy result for T1 data set 
g1 <- GenerateIndividual(c(9,26,7,11,21), nrow(T1))
Fitness(T1,g1,30)

# T2
g2 <- GenerateIndividual(c(21,20,19,26,16),nrow(T2))
Fitness(T2,g2,30)

# T3
g3 <- GenerateIndividual(c(30,20,45,26,18),nrow(T3))
Fitness(T3,g3,30)