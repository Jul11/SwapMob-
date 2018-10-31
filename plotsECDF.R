generate_CDF <- function(swapped.intersection){
  library(ggplot2)
  swapped.intersection <- sort(swapped.intersection, decreasing = TRUE)
  df <- data.frame(Coord = 1:length(swapped.intersection), SizeIntersection = swapped.intersection) 
  # Cummulative Distribution Function ####
  ggplot(df, aes(SizeIntersection)) + stat_ecdf(geom = "line", color = "blue") +
    xlab("Size of intersection") + ylab("ECDF")
}


link_with_X_points <- function(traces, swapped.intersection, trace.length, X){
  id.sort <- order(traces$id)
  num.traj <- RecordsIds(traces$id[id.sort])
  x.linkage <- expandingList()
  for(x.points in 1:X){
    logdebug("linkage %s", x.points)
    ids <- unique(traces$id)
    x.intersects <- vector(, length(trace.length))
    for(i in 1: length(trace.length)){
      if(trace.length[i] < x.points){
        x.s <- sample(trace.length[i], trace.length[i])
      } else {
        x.s <- sample(trace.length[i], x.points)
      }
      x.intersects[i] <- any(traces$nou[id.sort[num.traj[[ids[i]]]][x.s]] == ids[i])
    }
    x.linkage$add(x.intersects)
  }
  x.linkage <- x.linkage$as.list()
  
  df <- c()
  for(j in 1:X){
    x.test <- c(rep(0, length(ids) - length(swapped.intersection[x.linkage[[j]]])),
                swapped.intersection[x.linkage[[j]]])
    
    x.plot <- sort(x.test, decreasing = F)
    # df <- data.frame(Coord = 1:length(x.plot), SizeIntersection = x.plot)
    # assign(paste("df", j, sep = ""), df)
    df <- cbind(df, x.plot)  
  }
  
  df <- as.data.frame(df)
  names(df) <- 1:X
  mydf_m = melt(df) 
  names(mydf_m)[1] <- "Known_Points"
  
  ggplot(mydf_m, aes(x = value)) + 
    stat_ecdf(aes(group = Known_Points, colour = Known_Points)) +
    xlab("% of trajectory disclosed") + ylab("ECDF")  
}
