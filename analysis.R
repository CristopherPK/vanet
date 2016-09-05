# analyzing the algorithms 

T0 <- data.matrix(read.table("Scenes/E0-T-60s-4CP"))
T1 <- data.matrix(read.table("Scenes/E1-T-60s-4CP"))
T2 <- data.matrix(read.table("Scenes/E2-T-60s-4CP"))
T3 <- data.matrix(read.table("Scenes/E3-T-60s-4CP"))

result <- GeneticAlgorithm(T,5,30)

# greedy result for T0 data set
g0 <- GenerateIndividual(GreedyAlgorithm(T0,5,30), nrow(T0))
Fitness(T0,g0,30)

# genetic algorithm
result0 <- Evolutionary(T0,5,30)

# greedy result for T1 data set 
g1 <- GenerateIndividual(GreedyAlgorithm(T1,5,30), nrow(T1))
Fitness(T1,g1,30)

result1 <- Evolutionary(T1,5,30)

# T2
g2 <- GenerateIndividual(GreedyAlgorithm(T2,5,30),nrow(T2))
Fitness(T2,g2,30)

result2 <- Evolutionary(T2,5,30)

# T3
g3 <- GenerateIndividual(GreedyAlgorithm(T3,5,30),nrow(T3))
Fitness(T3,g3,30)

result3 <- Evolutionary(T3,5,30)
result3 <- result3[,1]
total_result3 <- cbind(total_result3,result3)

f <- sapply(1:ncol(total_result3), function(x) Fitness(T3,total_result3[,x],thresh))

# analysis case: 
# Genetic Algorithm with Random Initialization.
# 5 times execution
final.Result.Baar <- c()
for(i in 1:5){
  result <- Evolutionary(T3,5,30)
  final.Result.Baar <- cbind(result[,1],final.Result.Baar)
}

fitness.Baar <- sapply(1:ncol(total_result3), function(x) Fitness(T3,total_result3[,x],thresh))
mean(fitness.Baar)

# Fitness : 45.89915