library(cec2013)

mi <- 3
maxIterations <- 2
F <- 0.4
a <- 0.5
cr <- 0.5

SelectRandom <- function(P){
  P[sample(1:length(P), 1)]
}

SelectAverage <- function(P){
  mean(P)
}

BinomialCrossover <- function(x, y){
  z <- array(0, length(x))
  for(i in 1:length(x)){
    if(a < cr){
      z[i] <- y[i]
    }
    else{
      z[i] <- y[i]
    }
  }
  z
}

ExponentialCrossover <- function(x, y){
  z <- array(0, length(x))
  i <- 1
  while (i <= length(x)){
    if(a < cr){
      z[i] <- y[i]
      i <- i+1
    }else{
      break;
    }
  }
  while (i<= length(x)){
    z[i] <- x[i]
    i <- i+1
  }
  z
}

Tournament <- function(P, M){
  P
}

DifferentialEvolution <- function(crossover, select) {
  P <- array(1:4, c(mi))
  print(P)
  H <- P
  t <- 0
  stop <- 0
  while (stop < maxIterations){
    for (i in 1:mi){
      Parent <- select(P)
      Parents <- P[sample(1:length(P), 2)]
      M <- Parent + F * (Parents[1] - Parents[2])
      O <- crossover(P[i], M)
      H <- c(H,O)
      P[i] <- Tournament(P[i], O)
    }
    stop <- stop + 1
  }
  print("Population")
  print(P)
}

DifferentialEvolution(ExponentialCrossover, SelectRandom)
DifferentialEvolution(BinomialCrossover, SelectRandom)
DifferentialEvolution(ExponentialCrossover, SelectAverage)
DifferentialEvolution(BinomialCrossover, SelectAverage)

