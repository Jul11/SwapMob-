
#fast list
expandingList <- function(capacity = 10) {
  buffer <- vector('list', capacity)
  length <- 0
  
  methods <- list()
  
  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    capacity <<- capacity * 2
  }
  
  methods$add <- function(val) {
    if(length == capacity) {
      methods$double.size()
    }
    
    length <<- length + 1
    buffer[[length]] <<- val
  }
  
  methods$as.list <- function() {
    b <- buffer[0:length]
    return(b)
  }
  
  methods
}


LinearPartitionNum <- function(timestamps, Width){
  
  times.split <- split(timestamps, ggplot2::cut_width(timestamps, Width))
  
  for (j in 1: length(times.split[[1]])){
    times.split[[1]][j] <- j
  }
  
  i <- 1
  agregar <- 0
  while (i < length(times.split)){
    agregar <- agregar + length(times.split[[i]])
    for (j in 1: length(times.split[[i + 1]])){
      times.split[[i + 1]][j] <- j + agregar
    }
    i <- i + 1
  }
  times.split[[i]] <- times.split[[i]][-which(times.split[[i]] > length(timestamps))]
  return(times.split)
}


is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


library(logging);
# Logging configuration
basicConfig(level='FINEST');
# Level = INFO | DEBUG   # per (no) treure-ho a consola
setLevel(getHandler('basic.stdout'), level='INFO'); 

# output file
outputFile <- paste("log", ".txt", sep="");
addHandler(writeToFile, file=outputFile, level='DEBUG');
# close
removeHandler(writeToFile);


inverso <- function(Vector){
  vect.inv <- vector( length = max(Vector))
  for(i in 1:length(Vector)){
    vect.inv[Vector[i]] <- i
  }
  return(vect.inv)
}

###
RecordsIds <- function(IntegerVector){
  aux <- plyr::count(IntegerVector)
  IDsList <- vector(, length = length(max(aux$x)))
  
  IDsList[aux$x[1]] <- list(c(1:aux$freq[1]))
  for(index in 2: length(aux$x)){
    IDsList[aux$x[index]] <- list(c(1:aux$freq[index]) + sum(aux$freq[1: (index - 1)]))
  }
  return(IDsList)
}

