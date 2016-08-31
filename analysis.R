# analyzing the algorithms 

T0 <- data.matrix(read.table("Scenes/E0-T-60s-4CP"))
T1 <- data.matrix(read.table("Scenes/E1-T-60s-4CP"))
T2 <- data.matrix(read.table("Scenes/E2-T-60s-4CP"))
T3 <- data.matrix(read.table("Scenes/E3-T-60s-4CP"))

result <- GeneticAlgorithm(T,5,30)

# greedy result for T0 data set


# greedy result for T1 data set 
g1 <- GenerateIndividual(c(9,26,7,11,21), nrow(T1))