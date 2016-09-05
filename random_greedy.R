RandomGreedyAlgorithm <- function(T = matrix(), k, thresh){
  # Calculating boundaries
  num.Vehicles <- ncol(T)
  num.Inter <- nrow(T)
  
  # initial set with all intersections. 
  S <- 1:num.Inter
  
  # initializing empty result list.
  s <- c()
  
  tj <- c(rep.int(x = thresh,times = num.Vehicles))
  
  while(k > 0){
    W <- c(rep.int(x = 0,times = num.Inter))
    
    # for each vehicle.
    for(j in 1:num.Vehicles){
      # for each intersection. 
      Tj <- T[,j]
      time <- tj[j]
      for(i in S){
        Tij <- Tj[i]
        if(Tij > time) {
          if(time < 0){
            tj[j] <- 0
            time <- 0
          }
          Tij <- time
        }
        W[i] <- W[i] + Tij
      }
    }
    
    Ws <- sort(W,decreasing = TRUE)
    w <- which(W == sample(Ws[1:10],size = 1))
    
    tj <- sapply(1:num.Vehicles, function(x) if(tj[x] > 0) (tj[x] - T[w,x]) else 0)
    
    s <- append(s, w)
    not.S <- which(S == w)
    S <- S[-not.S]
    k = k-1
  }
  
  s
}
