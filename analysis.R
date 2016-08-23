# analyzing the algorithms 

T <- read.table(file = "Scenes/E2-T-60s-4CP")
T <- matrix(T)

result <- GeneticAlgorithm(T,5,30)