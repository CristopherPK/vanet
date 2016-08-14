GreedyAlgorithm <- function(T = matrix(), k, thresh){
  profvis({
  # Calculating boundaries
  num.Vehicles <- nrow(T)
  num.Inter <- length(T[[1]])
  
  # initial set with all intersections. 
  S <- 1:num.Inter
  
  # initializing empty result list.
  s <- c()

  while(k > 0 || length(S) == 0){
    W <- c(rep.int(x = 0,times = num.Inter))
    tj <- c(rep.int(x = 0,times = num.Vehicles))
    
    # for each vehicle.
    for(j in 1:num.Vehicles){
      # for each intersection. 
      Tj <- T[[j]]
      for(i in 1:num.Inter){
        if(!(i %in% s)){ # check if intersection wasn't chosen already.
          Tij <- Tj[i]
          if(Tij > 0){ # avoiding waste of processing time with zero values. 
            if(Tij > (thresh - tj[j])){
              W[i] <- W[i] + (thresh - tj[j])
              tj[j] <- 0
            } else {
              tj[j] <- tj[j] + Tij
              W[i] <- W[i] + Tij
            }
          }
        }
      }
    }
    
    # this line was changed to be used as input to the genetic approach.
    # w <- which(W == max(W)) 
    
    # genetic approach change
    Ws <- sort(W,decreasing = TRUE)
    w <- which(W == sample(Ws[1:10],size = 1))
    
    s <- (append(s, w))
    k = k-1
  }
  })
  s
}
