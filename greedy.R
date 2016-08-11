greedy <- function(T, k, thresh){
  
  # initial set with all intersections. 
  S <- 1:nrow(T);
  
  # initializing empty result list.
  s <- c();
  
  #t <- colSums(T)
  #W <- rowSums(T)
  
  # which(rowSums(T) == max(rowSums(T))
  # colSums(T), sum of time vehicle spends in all intersections.
  while(k > 0 || length(S) == 0){
    tj <- list()
    W <- c(rep.int(x = 0,times = nrow(T)))
    
    # for each vehicle.
    for(j in 1:ncol(T)){
      tj <- append(tj, as.integer(0));
      # for each intersection. 
      for(i in 1:nrow(T)){
        if(!(i %in% s)){ # check if intersection wasn't chosen already.
          T_ij <- T[j][[1]][i];
          if(T_ij > (thresh - tj[[j]])){
            W[[i]] <- W[[i]] + (thresh - tj[[j]]);
            tj[[j]] <- 0
          } else {
            tj[[j]] <- tj[[j]] + T_ij;
            W[[i]] <- W[[i]] + T_ij;
          }
        }
      }
    }
    
    w <- which(W == max(W));
    s <- (append(s, w));
    k = k-1;
  }
  
  s;
  
}
