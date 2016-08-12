greedy <- function(T, k, thresh){
  
  # initial set with all intersections. 
  S <- 1:nrow(T);
  
  # initializing empty result list.
  s <- c();

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
          if(T_ij > 0){ # avoiding waste of processing time with zero values. 
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
    }
    
    # this line was changed to be used as input to the genetic approach.
    # w <- which(W == max(W)); 
    
    # genetic approach change
    Ws <- sort(W,decreasing = TRUE)
    w <- which(W == sample(Ws[1:10],size = 1))
    s <- (append(s, w));
    k = k-1;
    
  }
  
  s;
  
}
