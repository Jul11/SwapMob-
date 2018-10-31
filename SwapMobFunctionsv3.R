####### functions SwapMob #########

colocated.find <- function(traces, SECONDS, ROUNDING){
  times.split <- LinearPartitionNum(traces$timestamp, SECONDS)
  
  Totes.Coloc <- expandingList()
  for(s in 1:length(times.split)) {
    logdebug("Colocation interval %s groups of %s", s,  length(times.split) )
    df <- traces[times.split[[s]], c("lat", "long")]
    df <- round(df, digits = ROUNDING)
    
    
    propers <- prodlim::row.match(df, unique(df))
    llista.elem <- plyr::count(propers)
    llista.elem <- llista.elem$x[which(llista.elem$freq > 1)]
    llista <- vector( "list", length = length(llista.elem))
    
    if (length(llista.elem) > 0){
      for (i in 1: length(llista.elem)){ 
        llista[[i]] <- times.split[[s]][which(propers == llista.elem[i])]
        if (length(llista[[i]]) == 1){
          llista[[i]] <- list()
        }
      }    
    }
    Totes.Coloc$add(llista) 
  }
  return(Totes.Coloc$as.list())
}

colocated.swap <- function(Totes.Coloc){
  
  Swaps.exmp <- vector(mode = "list", length = length(Totes.Coloc))
  modified.register <- c() 
  modified.trajectories <- c() 
  for(j in 1:length(Totes.Coloc)){
    logdebug("Generating swaps %s of %s", j, length(Totes.Coloc))
    Swaps.exmp[[j]] <- vector(mode = "list", length = length(Totes.Coloc[[j]]))
    
    if (length(Totes.Coloc[[j]]) > 0){
      for (i in 1:length(Totes.Coloc[[j]])){
        elements.grup <- Totes.Coloc[[j]][[i]]
        
        if (is.wholenumber(length(elements.grup)/2)){
          odd <- 1
        } else {odd <- 0}
        
       
        if (length(elements.grup == 2)){
          Swaps.exmp[[j]][[i]] <- rbind(elements.grup[1], elements.grup[2])
        } else {
          Swaps.exmp[[j]][[i]] <- rbind(elements.grup[1:((length(elements.grup)-1 + odd)/2)], 
                                        sample(elements.grup[((length(elements.grup)+ 1 + odd)/2 ):length(elements.grup)],
                                               ((length(elements.grup) - 1 + odd)/2)))
        }
      }  
    }
    if (length(unlist(Swaps.exmp[j])) > 0){
      modified.register <- c(modified.register, unlist(Swaps.exmp[j]))
      modified.trajectories <- c(modified.trajectories, traces$id[unlist(Swaps.exmp[j])])  
    }
  }
  
  swap.rec <- matrix(modified.register, nrow = 2)
  swap.id <- matrix(modified.trajectories, nrow = 2)
  # Treure timestamps repetides
  wo.repetition <- intersect(match(unique(swap.rec[1,]), swap.rec[1, ]),
                             match(unique(swap.rec[2,]), swap.rec[2, ]))
  swap.id <- swap.id[, wo.repetition]
  swap.rec <- swap.rec[, wo.repetition]
  # Order
  swap.id <- swap.id[, order(swap.rec[1, ])]
  swap.rec <- swap.rec[, order(swap.rec[1, ])]

  x <- prodlim::row.match(data.frame(unique(t(swap.id))), data.frame(t(swap.id)))
  swap.id <- swap.id[, x]
  swap.rec <- swap.rec[, x]
  #order by timestamp
  swap.times <- traces$timestamp[swap.rec[1, ]]
  ord.swap <- order(swap.times)

  id.sort <- order(traces$id)
  num.traj <- RecordsIds(traces$id[id.sort]) 
  
  # swapping
  new_id <- traces$id 
  
  for (i in 1:length(swap.id[1, ord.swap])){ 
    logdebug("applied swap %s of %s", i,  length(swap.id[1, ord.swap]))
    s1 <- which(id.sort[num.traj[[swap.id[1, ord.swap[i]]]]] == swap.rec[1, ord.swap[i]])
    s2 <- which(id.sort[num.traj[[swap.id[2, ord.swap[i]]]]] == swap.rec[2, ord.swap[i]])
    
    new_id[id.sort[num.traj[[swap.id[1, ord.swap[i]]]][1:s1]]] <- swap.id[2, ord.swap[i]]
    new_id[id.sort[num.traj[[swap.id[2, ord.swap[i]]]][1:s2]]] <- swap.id[1, ord.swap[i]]
  }
  return(new_id)
}

calculate_intersection <- function(traces){
  id.sort <- order(traces$id)
  num.traj <- RecordsIds(traces$id[id.sort])
  ids <- unique(traces$id)
  swapped.intersection <- vector(, length = length(ids))
  trace.length <- vector(, length = length(ids)) 
  for(i in 1:length(ids)){
    logdebug("traj %s", i)
    trace.length[i]  <- length(num.traj[[ids[i]]])
    swapped.intersection[i] <- length(which(traces$id[id.sort[num.traj[[ids[i]]]]] == 
                                              traces$nou[id.sort[num.traj[[ids[i]]]]]))/trace.length[i]
  } 
  return(c(list(swapped.intersection), list(trace.length)))
}



