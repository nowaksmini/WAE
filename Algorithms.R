library(cec2013)

mi <- 100
dimension <- 2
maxIterations <- 500
F <- 0.1
cr <- 0.9
optimalValues = c(seq(-1400, -100, 100), seq(100, 1400, 100))

SelectRandom <- function(functionIndex, P){
  P[sample(1:mi, 1),]
}

SelectAverage <- function(functionIndex, P){
  meanPoint <- array(0, dimension)
  for (i in 1:dimension) {
    meanPoint[i] <- mean(P[,i])
  }
  meanPoint
}

SelectBest <- function(functionIndex, P){
  best <- P[1,]
  for (i in 2:mi) {
    best <- Tournament(functionIndex, P[i,], best)
  }
  best
}

BinomialCrossover <- function(x, y){
  z <- array(0, dimension)
  a <- runif(1, 0, 1)
  for(i in 1:dimension){
    if(a < cr){
      z[i] <- y[i]
    }
    else{
      z[i] <- x[i]
    }
  }
  z
}

ExponentialCrossover <- function(x, y){
  z <- array(0, dimension)
  i <- 1
  a <- runif(1, 0, 1)
  while (i <= dimension){
    if(a < cr){
      z[i] <- y[i]
      i <- i+1
    }else{
      break;
    }
  }
  while (i <= dimension){
    z[i] <- x[i]
    i <- i+1
  }
  z
}

Tournament <- function(functionIndex, P, M){
  #print("Tournament: ")
  #print(P)
  #print(M)
  if (Error(functionIndex, P) <= Error(functionIndex, M)) {
    return(P)
  }
  else {
    return(M)
  }
}

Error <- function(functionIndex, p) {
  return(abs(optimalValues[functionIndex] - cec2013(functionIndex, c(p))))
}

DifferentialEvolution <- function(crossover, select, functionIndex) {
  randomPoints <- runif(mi*dimension, -80, -70)
  P <- matrix(randomPoints, mi, dimension)
  print(cec2013(1,P))
  #print(P)
  H <- P
  t <- 0
  stop <- 0
  while (stop < maxIterations){
    for (i in 1:mi){
      Parent <- select(functionIndex, P)
      Parents <- P[sample(1:mi, 2),]
      M <- Parent + F * (Parents[1,] - Parents[2,])
      O <- crossover(P[i,], M)
      H <- rbind(H,O)
      P[i,] <- Tournament(functionIndex, P[i,], O)
    }
    stop <- stop + 1
  }
  #print("Population")
  #print(P)
  plot(H[,1], H[,2], type="p")
}

DifferentialEvolution(ExponentialCrossover, SelectRandom, 1)
DifferentialEvolution(BinomialCrossover, SelectRandom, 1)
DifferentialEvolution(ExponentialCrossover, SelectAverage, 1)
DifferentialEvolution(BinomialCrossover, SelectAverage, 1)
DifferentialEvolution(ExponentialCrossover, SelectBest, 1)
DifferentialEvolution(BinomialCrossover, SelectBest, 1)
