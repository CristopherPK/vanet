greedy <- function(T, k, thresh){
  
  # initial set with all intersections. 
  S <- 1:nrow(T)
  # initializing empty lists
  s <- c()
  
  t <- colSums(T)
  W <- rowSums(T)
  
  tj <- list()
  which(t == min(t))
  # which(rowSums(T) == max(rowSums(T))
  # colSums(T), sum of time vehicle spends in all intersections.
  while(k > 0 || length(S) == 0){
    for(j in 1:ncol(T)){
      tj <- append(t, as.integer(0));
      for(i in 1:nrow(T)){
        t[[j]] <- T[j][[1]][i] + t[[j]];

      }
    }
  }
}
